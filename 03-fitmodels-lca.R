
## Program:   03-fitmodels-lca.R
## Task:      Fit multigroup LCA models to EVS data and plot results.
##
## Input:     01-clean-evs5.Rds
##
## Output:    fig3.jpg
##            fig6.jpg
##            fig_a1.jpg
##            fig_a2.jpg
##            fig_a3.jpg
##            02-subset-evs5.Rds
##            clusters_4cl.Rds
##
## Project:   cleavage-identity
## Author:    Martin Lukk / 2024-10-15 (created)

# 0. Program Setup --------------------------------------------------------
library(tidyverse)
library(here)
library(glca)
library(countrycode)
library(parallel)
library(RColorBrewer)
library(patchwork)
library(kableExtra)

get_bics <- function(model_list, cluster = F) {
  bics <- map_dbl(model_list, ~ .x[["gof"]][["BIC"]])
  if (cluster == F)  {
    bics %>%
      tibble(nclass, .) %>%
      rename(bic = ".")
  } else if (cluster == T) {
    bics %>%
      tibble(ncluster, .) %>%
      rename(bic = ".")
  }
}
plot_bics <- function(model_list, cluster = F) {
  if (cluster == F)  {
    xlab <- "Number of Latent Classes"
  } else if (cluster == T) {
    xlab <- "Number of Latent Clusters"
  }
  model_list %>%
    ggplot(aes(x = nclass, y = bic)) +
    geom_line() +
    geom_point(size = 1.25) +
    scale_x_continuous(breaks = nclass) +
    labs(x = xlab,
         y = "BIC") +
    theme_bw()
}
get_rprobs <- function(model, nclass, group = "ALL", cluster = F) {
  
  if (isFALSE(cluster)) {
    df <- model[[nclass - 1]][["param"]][["rho"]][[group]]
  } else if (isTRUE(cluster)) {
    df <- model[[nclass - 1]][["param"]][["rho"]]
  }
  
  clsvars <- c("clstown", "clsregion", "clscountry", "clscontinent", "clsworld")
  
  imap(
    clsvars,
    ~ df[[.x]][,2]
      # Extract class-specific category response probabilities by variable
      # from fitted model object with best-fitting nr. of latent classes
      %>% 
      # Convert to data frame
      as_tibble(rownames = "class") %>% 
      # Clean class values and variable names
      mutate(class = str_remove_all(class, "[^\\d]"),
             country = group) %>%
      rename(!!.x := value) %>%
      # Reshape data long
      pivot_longer(all_of(.x),
                   names_to = "variable",
                   values_to = "prob")
  )
}
get_cprobs <- function(model, nclass, cluster = F) {
  if (isFALSE(cluster)) {
    x <- model[[nclass - 1]]$posterior
  } else if (isTRUE(cluster)) {
    x <- model[[nclass - 1]]$posterior$class
  }
  x %>%
    map_df(~ summarize(.x, across(everything(), mean)), .id = "id") %>%
    pivot_longer(!id, names_to = "class", values_to = "classprob") %>%
    mutate(class = str_extract(class, "\\d+")) %>%
    rename(country = id)
}
get_clusters <- function(model, ncluster = NULL, list = F) {
  if (!is.null(ncluster)) {
    x <- model[[ncluster - 1]]$posterior$cluster
  } else if (is.null(ncluster)) {
    x <- model$posterior$cluster
  }
  if (isFALSE(list)) {
    df1 <- x %>% 
      rownames_to_column("country") %>%
      pivot_longer(
        cols = -country,
        names_to = "cluster",
        names_prefix = "Cluster ",
        values_to = "prob"
      ) %>%
      group_by(country) %>%
      slice_max(order_by = prob) %>%
      ungroup() %>%
      select(-prob) %>%
      mutate(country = as.character(country),
             cluster = as.numeric(cluster)) %>% 
      arrange(cluster)
    
    df2 <- df1 %>%
      count(cluster, name = "size") %>%
      arrange(desc(size))
    
    df3 <- left_join(df2, df1, by = "cluster")
    
    # Give output as list
  } else if (isTRUE(list)) {
    df3 <- x %>% 
      rownames_to_column("country") %>%
      pivot_longer(
        cols = -country,
        names_to = "cluster",
        names_prefix = "Cluster ",
        values_to = "prob"
      ) %>%
      group_by(country) %>%
      slice_max(order_by = prob) %>%
      ungroup() %>%
      select(-prob) %>%
      mutate(country = as.character(country),
             cluster = as.numeric(cluster)) %>% 
      arrange(cluster) %>% 
      # Make list from tidy data frame
      group_by(cluster) %>%
      summarize(countries = list(country)) %>%
      pull(countries)
  }
  df3
}
reorder_classes <- function(data) {
  data %>%
    filter(!(variable %in% c("World"))) %>% 
    group_by(country, class) %>%
    # Sum class conditional response probabilities by class and country
    # (i.e., sort classes by "thickness" of attitudes associated with it)
    summarize(total_prob = sum(prob), .groups = 'drop') %>% 
    # Rank classes by summed probabilities, with the largest sum ranked 1
    arrange(country, desc(total_prob)) %>%
    group_by(country) %>%
    mutate(new_class = rank(-total_prob)) %>%
    ungroup() %>% 
    # Merge new class rankings into original data
    right_join(data, by = c("country", "class")) %>% 
    # Replace original class with the new class
    mutate(class = as.character(new_class)) %>% 
    select(-total_prob, -new_class)
}
score_classes <- function(data, measure = "cosine_dist") {
  
  # Filter for relevant classes and calculate global/local scores
  scored_data <- data %>%
    filter(class %in% c("2", "3")) %>%
    group_by(country, class) %>%
    summarize(
      global_score = sum(prob[variable %in% c("Continent", "World")]),
      local_score  = sum(prob[variable %in% c("Town/City", "Region")]),
      prob_vector  = list(prob),  # Store response probabilities as a vector
      .groups = "drop"
    )
  
  # Pivot for class comparisons
  pivoted_data <- scored_data %>%
    pivot_wider(names_from = class, values_from = c(global_score, local_score, prob_vector),
                names_glue = "{.value}_class{class}")
  
  # Calculate divergence measures
  pivoted_data <- pivoted_data %>%
    rowwise() %>%
    mutate(
      # Cosine Similarity and Distance
      cosine_sim = as.numeric(lsa::cosine(unlist(prob_vector_class2), unlist(prob_vector_class3))),
      cosine_dist = 1 - cosine_sim,
      
      # Divergence Index
      divergence = abs(
        (global_score_class3 - local_score_class3) - 
          (global_score_class2 - local_score_class2)
      )
    ) %>%
    ungroup()
  
  # Record selected class difference measure and highlight countries
  pivoted_data <- pivoted_data |>
    mutate(
      classdiff = !!sym(measure),
      highlight = if_else(classdiff >= quantile(classdiff, 2/3, na.rm = TRUE), TRUE, FALSE)
    )
  
  return(pivoted_data)
}
plot_responseprobs <- function(data, country_name, text_angle = 45, text_size = 7, greyval = 0.1) {
  # Filter data for the specific country
  country_data <- data %>% filter(country == country_name)
  
  # Get color palette
  set2_colors <- brewer.pal(n_distinct(country_data$class), "Set2")
  
  # If cluster is present, set up cluster colors
  has_clusters <- "cluster" %in% names(data)
  if(has_clusters) {
    n_clusters <- n_distinct(data$cluster)
    cluster_colors <- if(n_clusters <= 8) {
      brewer.pal(8, "Set2")
    } else {
      c(
        brewer.pal(8, "Set2"),
        brewer.pal(8, "Set1"),
        brewer.pal(8, "Set3")
      )[1:n_clusters]
    }
    country_cluster <- country_data$cluster[1]
  }
  
  # Ensure the 'class' variable is treated as a factor with proper numeric ordering
  country_data <- country_data %>%
    mutate(class = factor(class, levels = sort(as.numeric(unique(class)))))
  
  # Create class labels for given country
  class_labels <- country_data %>%
    distinct(class, classprob) %>%
    arrange(as.numeric(class)) %>%
    mutate(label = if(n_distinct(class) > 3) {
      paste0(class, " (", round(classprob, 2), ")")
    } else {
      paste0("Class ", class, " (", round(classprob, 2), ")")
    }) %>%
    select(class, label) %>%
    deframe()
  
  if ("highlight" %in% names(country_data)) {
    # Get highlight status for this country
    is_highlighted <- country_data$highlight[1]
    
    # Define alpha values based on highlight status
    alpha_values <- if(is_highlighted) {
      c("1" = 0.25, "2" = 0.85, "3" = 0.85)
    } else {
      c("1" = 0.25, "2" = 0.25, "3" = 0.25)
    }
    
    # Create the plot with highlighting
    p <- country_data %>%
      mutate(linewidth = if(is_highlighted) {
        ifelse(class == "1", 0.35, 0)
      } else {
        0.35
      }) %>%
      ggplot(aes(
        x = fct_relevel(as_factor(variable), "Town/City", "Region", "Country", "Continent", "World"),
        y = prob,
        fill = class,
        color = class,
        linewidth = linewidth)) +
      geom_bar(stat = "identity",
               position = "dodge",
               aes(alpha = class),
               show.legend = F) +
      scale_alpha_manual(values = alpha_values) +
      scale_fill_brewer(palette = "Set2") +
      scale_color_brewer(palette = "Set2") +
      scale_linewidth_identity()
    
  } else {
    # Create fill colors and alpha values before plotting
    fill_colors <- country_data %>%
      distinct(class, classprob) %>%
      mutate(
        fill_color = case_when(
          classprob <= greyval ~ "grey75",
          has_clusters ~ "grey60",
          TRUE ~ set2_colors[as.numeric(class)]
        ),
        alpha_value = ifelse(classprob <= greyval, 0.4, 0.75)
      )
    
    # Join fill colors back to main data
    country_data <- country_data %>%
      left_join(fill_colors, by = c("class", "classprob"))
    
    # Create the plot without highlighting - keeping original behavior exactly
    p <- country_data %>%
      ggplot(aes(
        x = fct_relevel(as_factor(variable), "Town/City", "Region", "Country", "Continent", "World"),
        y = prob,
        fill = fill_color,
        alpha = alpha_value)) +
      geom_bar(stat = "identity",
               position = "dodge",
               show.legend = F) +
      scale_fill_identity() +
      scale_alpha_identity()
  }
  
  # Base theme
  theme_base <- theme_bw() +
    theme(axis.text.x = element_text(angle = text_angle, hjust = 1, size = text_size))
  
  # Add cluster-specific theme elements if clusters present
  if(has_clusters) {
    theme_base <- theme_base +
      theme(
        strip.background = element_rect(
          fill = adjustcolor(cluster_colors[country_cluster], alpha.f = 0.35)
        ),
        strip.text = element_text(color = "black"),
        panel.border = element_rect(color = cluster_colors[country_cluster], linewidth = 1)
      )
  }
  
  # Add the rest of the plot elements
  p + labs(x = "Territorial Identity",
           y = "Response Probability",
           title = country_name) +
    facet_wrap(vars(class), labeller = labeller(class = class_labels), nrow = 1) +
    theme_base
}
get_classmemb <- function(model, ncluster, group) {
  model[[ncluster - 1]][["posterior"]][["class"]][[group]] %>% 
    as.matrix() %>% 
    # Apply stochastic assignment (respondent class assigned based on 
    # random draw from distribution of posterior probabilities)
    Hmisc::rMultinom(., 1) %>% 
    as_tibble_col(column_name = "class") %>% 
    mutate(class = as.numeric(str_extract(class, "\\d+")))
}

# 1. Load Data ------------------------------------------------------------
df <-
  read_rds(here("data", "output", "01-clean-evs5.Rds")) %>%
  filter(if_all(starts_with("cls"), ~ !is.na(.x)))

# 2. Recode variables -----------------------------------------------------
df <-
  df %>% 
  mutate(
    # Code country characteristics
    region  = countrycode(country_f, "country.name", "un.regionsub.name"),
    eu_member = countrycode(country_f, "country.name", "eu28", warn = F),
    eu_member = as.numeric(if_else(eu_member == "EU", 1, 0, missing = 0)),
    # Dichotomize responses
    across(starts_with("cls"), ~ if_else(.x >= 3, 1, 0)),
    # Fix label
    country_f = fct_recode(country_f,
                           "Czechia" = "Czech Republic")
  )

# 3. Fit Structurally Heterogeneous Multigroup LCA Models -----------------
m <- item(clstown, clsregion, clscountry, clscontinent, clsworld) ~ 1
nclass <- 2:12

fits_lca1 <-
  mclapply(
    nclass,
    function(n) glca(formula = m, nclass = n, data = df, seed = 12345, verbose = F,
                     group = country_f,
                     measure.inv = FALSE),
    mc.cores = detectCores()
  )

# Compare solutions
nclass_lowestbic <- get_bics(fits_lca1) %>%  slice_min(bic) %>% pull(nclass)

p_bics_lca1 <-
  get_bics(fits_lca1) %>% 
  plot_bics() +
  labs(caption = "Note: Bayesian information criterion values for structurally heterogeneous latent class models fitted to the 2017-2020\nEuropean Values Study.") +
  theme(plot.caption = element_text(hjust = 0.0))

ggsave(here("figures/fig_a1.jpg"), width = 7.5, height = 4, units = "in", dpi = 400)

# Select best-fitting class solution
nclass_bestfit <- nclass_lowestbic

# Get Class Membership Probabilities by Country
classprobs_lca1 <- get_cprobs(fits_lca1, nclass_bestfit)

# Get Class-Conditional Response Probabilities
countries <- df %>% count(country_f) %>% pull(country_f)

responseprobs_lca1 <-
  map(countries, ~ get_rprobs(fits_lca1, nclass_bestfit, .x)) %>% 
  reduce(bind_rows) %>% 
  mutate(variable = str_to_title(str_remove(variable, "cls")),
         variable = if_else(variable == "Town", "Town/City", variable)) %>% 
  arrange(country, class) %>%
  # Append class probabilities
  left_join(classprobs_lca1, by = c("country", "class")) %>% 
  # Reorder classes based on summed probabilities
  reorder_classes()

# Compute class difference scores
scoring_measure <- "cosine_dist" # Cosine distance or global-local divergence
responseprobs_lca1 <-
  responseprobs_lca1 %>%
  left_join(score_classes(responseprobs_lca1, measure = scoring_measure),
            by = "country")

# Plot Class-conditional Response Probabilities by Country and Combine
p_responseprobs_lca1 <-
  map(countries, ~ plot_responseprobs(responseprobs_lca1, .x)) %>% 
  wrap_plots(., ncol = 5, axis_titles = "collect", axes = "collect") +
  plot_annotation(
    # caption = 'Note: Estimates from structurally heterogeneous latent class models fitted to the 2017-2020 European Values Study. Bar heights indicate estimated class-conditional response probabilities for territorial identification items by country. Highlighted countries are those in the top \ntercile of cosine distance between Class 2 and 3 item response probability vectors, indicating greater divergence between classes in terms of territorial identification patterns. Estimated class proportions listed in parentheses.',
    theme = theme(plot.caption = element_text(hjust = 0.0))
  )

ggsave(here("figures", "fig3.jpg"), plot = p_responseprobs_lca1, device = "jpg",
       width = 15, height = 9.75, units = "in", dpi = 400)

# Save Data for Class Difference Map
class_propdiff <- 
  responseprobs_lca1 |>
  filter(class == 1 | class == 2) |>
  slice_head(by = c("country", "class")) |>
  select(country, class, classprob) |>
  pivot_wider(names_from = class,
              values_from = classprob,
              names_prefix = "class_") |>
  mutate(propdiff = class_1 - class_2,
         country = countrycode::countrycode(country,
                                            origin = "country.name",
                                            destination = "iso3c")) |> 
  rename(ISO3_CODE = country)

write_rds(class_propdiff, here("data", "output", "class_propdiff.rds"))

# 4. Fit Structurally Homogeneous Multigroup LCA Models -------------------
nclass <- 2:12

fits_lca2 <-
  mclapply(
    nclass,
    function(n) glca(formula = m, nclass = n, data = df, seed = 12345, verbose = F,
                     group = country_f,
                     measure.inv = TRUE),
    mc.cores = detectCores()
  )

# 5. Fit Structurally Homogeneous Multigroup LCA Models with Clusters -----
ncluster <- 2:12

fits_lca3_4cl <-
  mclapply(
    ncluster,
    function(n) glca(formula = m, data = df, seed = 12345, verbose = F,
                     nclass = 4, 
                     group = country_f,
                     ncluster = n),
    mc.cores = detectCores()
  )

# Compare solutions
ncluster_lowestbic <-
  get_bics(fits_lca3_4cl, cluster = T) %>%  slice_min(bic) %>% pull(ncluster)

p_bics_lca3_4cl <-
    get_bics(fits_lca3_4cl, cluster = T) %>% 
    plot_bics(cluster = T) +
   labs(caption = "Note: Bayesian information criterion values for structurally homogeneous latent class models with latent clusters, fitted to the\n2017-2020 European Values Study.") +
   theme(plot.caption = element_text(hjust = 0.0))

ggsave(here("figures/fig_a2.jpg"), width = 7.5, height = 4, units = "in", dpi = 400)

# Select best-fitting class solution
ncluster_bestfit <- ncluster_lowestbic - 2 # Select 10-cluster solution.

# Get Class Membership Probabilities by Country
classprobs_lca3 <- get_cprobs(fits_lca3_4cl, ncluster_bestfit, cluster = T)

# Get Cluster Membership by Country
clusterlist_lca3 <- get_clusters(fits_lca3_4cl, ncluster = ncluster_bestfit, list = F)

# Get Class-Conditional Response Probabilities
responseprobs_lca3 <-
  get_rprobs(fits_lca3_4cl, ncluster_bestfit, cluster = T) %>% 
  reduce(bind_rows) %>% 
  mutate(variable = str_to_title(str_remove(variable, "cls")),
         variable = if_else(variable == "Town", "Town/City", variable)) %>%
  select(-country) %>% 
  arrange(class) %>% 
  # Merge attributes
  cross_join(clusterlist_lca3, .) %>% 
  left_join(., classprobs_lca3, by = c("country", "class")) %>% 
  reorder_classes() %>% 
  arrange(cluster, country, class)

# Plot Class-conditional Response Probabilities
p_responseprobs_lca3_noclusters <-
  map(countries,
      ~ plot_responseprobs(responseprobs_lca3 %>% select(-cluster), .x)) %>%
  wrap_plots(., ncol = 5, axis_titles = "collect", axes = "collect") +
  plot_annotation(
    # caption = 'Note: Data from 2017-2020 European Values Study. Bar heights indicate estimated class-conditional response probabilities for territorial identification items by country. Estimated class proportions listed in parentheses. Classes with proportions less than .1 are shown in grey.',
    theme = theme(plot.caption = element_text(hjust = 0.0)))

ggsave(here("figures", "fig_a3.jpg"), plot = p_responseprobs_lca3_noclusters, device = "jpg",
       width = 16, height = 9.25, units = "in", dpi = 400)

countries_expansivist <- 
  clusterlist_lca3 %>% 
  filter(country %in% c("Denmark", "Germany", "Hungary",
                        "Poland", "Switzerland", "France",
                        "Iceland", "Sweden", "Finland",
                        "Netherlands", "Norway")) |> 
  pull(country)

p_responseprobs_lca3_clusters <-
  map(countries_expansivist, ~ plot_responseprobs(responseprobs_lca3 %>% select(-cluster), .x, text_angle = 40, text_size = 6, greyval = 0.055)) %>% 
  wrap_plots(., ncol = 3, axis_titles = "collect", axes = "collect") +
  plot_annotation(
    # caption = 'Note: Estimates from a structurally homogeneous latent class model, with 4 classes and 10 clusters, fitted to the 2017-2020 European Values Study. Bar heights indicate estimated class-conditional\nresponse probabilities for territorial identification items by country. Estimated class proportions listed in parentheses. Classes with proportions less than or equal to .05 are shown in light grey.',
    theme = theme(plot.caption = element_text(hjust = 0.0)))

ggsave(here("figures", "fig6.jpg"), plot = p_responseprobs_lca3_clusters, device = "jpg",
       width = 11, height = 6.25, units = "in", dpi = 400)

# 6. Impute Class Membership, Subset Data, and Save -----------------------
fits_lca3_4cl <-
  # Ensure consistent order of latent classes
  map(fits_lca3_4cl, ~ reorder(.x, decreasing = F))

# Plot classes to check
p_classes <-
  get_rprobs(fits_lca3_4cl, ncluster_bestfit, cluster = T) %>%
  reduce(bind_rows) %>%
  mutate(
    variable = str_to_title(str_remove(variable, "cls")),
    variable = if_else(variable == "Town", "Town/City", variable),
    country = "All",
    classprob = NA_real_
  ) %>%
  arrange(class) %>%
  plot_responseprobs("All")

clusters_sorted <- get_clusters(fits_lca3_4cl, ncluster = ncluster_bestfit, list = T)
clusters_analysis <- c(7, 6, 3, 10) # Select analysis clusters

countries_analysis <- sort(unlist(clusters_sorted[clusters_analysis]))

set.seed(seed = 12345)
classmembs_lca3 <- 
  map(countries_analysis, ~ get_classmemb(fits_lca3_4cl, ncluster_bestfit, .x)) %>%
  set_names(countries_analysis) %>% 
  bind_rows(.id = "country_f")

df_analysis <-
  filter(df, country_f %in% countries_analysis) %>%
  bind_cols(., classmembs_lca3) %>%
  {
    if (all(.$country_f...2 == .$country_f...29)) {
      mutate(., country_f = country_f...2) %>%
      select(-(contains("...")))
    } else
      stop("Countries in merging and merged data set don't match!")
  }

write_rds(df_analysis, here("data", "output", "02-subset-evs5.Rds"))

# Save country clusters from 4-class solution
clusters_4cl <- fits_lca3_4cl[6:11] %>% map(~ get_clusters(.x, list = T))
write_rds(clusters_4cl, here("data", "output", "clusters_4cl.rds"))

# 7. Generate Goodness of Fit Statistics Table (Word) ----------------------------
library(flextable)
library(officer)

# Table 1: Structurally heterogeneous models
lca_stats <- gofglca(fits_lca1, test = "chisq", seed = 12345)$gtable

lca_table <- lca_stats %>%
  as_tibble(rownames = "model") %>%
  mutate(
    classes = as.numeric(model) + 1,
    .before = loglik
  ) %>%
  select(-model) %>%
  mutate(
    loglik = round(loglik, 1),
    AIC = round(AIC, 1),
    BIC = round(BIC, 1),
    entropy = round(entropy, 3),
    Gsq = round(Gsq, 2)
  )

lca_tbl_word1 <- flextable(lca_table) |>
  set_header_labels(
    classes = "Classes",
    loglik = "Log-likelihood",
    AIC = "AIC",
    BIC = "BIC",
    entropy = "Entropy",
    df = "df",
    Gsq = "G²"
  ) |>
  border_remove() |>
  hline_top(border = fp_border(color = "black", width = 1), part = "header") |>
  hline_bottom(border = fp_border(color = "black", width = 1), part = "header") |>
  hline_bottom(border = fp_border(color = "black", width = 1), part = "body") |>
  align(align = "center", part = "all") |>
  font(fontname = "Arial", part = "all") |>
  padding(padding.top = 1, padding.bottom = 1, part = "all") |>
  set_table_properties(layout = "autofit", width = 0.8) |>
  add_footer_lines("Note: Model fit statistics for structurally heterogeneous latent class models fitted to the 2017-2020 European Values Study. Lower AIC and BIC values indicate better fit. Entropy measures classification certainty (higher values indicate better separation between classes).")

save_as_docx(lca_tbl_word1, path = "figures/table_a2.docx")

# Table 2: Structurally homogeneous models
lca_stats <- gofglca(fits_lca2, test = "chisq", seed = 12345)$gtable

lca_table <- lca_stats %>%
  as_tibble(rownames = "model") %>%
  mutate(
    classes = as.numeric(model) + 1,
    .before = loglik
  ) %>%
  select(-model) %>%
  mutate(
    loglik = round(loglik, 1),
    AIC = round(AIC, 1),
    BIC = round(BIC, 1),
    entropy = round(entropy, 3),
    Gsq = round(Gsq, 2)
  )

lca_tbl_word2 <- flextable(lca_table) |>
  set_header_labels(
    classes = "Classes",
    loglik = "Log-likelihood",
    AIC = "AIC",
    BIC = "BIC",
    entropy = "Entropy",
    df = "df",
    Gsq = "G²"
  ) |>
  border_remove() |>
  hline_top(border = fp_border(color = "black", width = 1), part = "header") |>
  hline_bottom(border = fp_border(color = "black", width = 1), part = "header") |>
  hline_bottom(border = fp_border(color = "black", width = 1), part = "body") |>
  align(align = "center", part = "all") |>
  font(fontname = "Arial", part = "all") |>
  padding(padding.top = 1, padding.bottom = 1, part = "all") |>
  set_table_properties(layout = "autofit", width = 0.8) |>
  add_footer_lines("Note: Model fit statistics for structurally homogeneous latent class models fitted to the 2017-2020 European Values Study. Lower AIC and BIC values indicate better fit. Entropy measures classification certainty (higher values indicate better separation between classes).")

save_as_docx(lca_tbl_word2, path = "figures/table_a3.docx")
