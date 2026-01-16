
## Program:   04-fitmodels-logit.R.
## Task:      Fit mixed effects logistic regression models to EVS data to
##            analyze demographic and political correlates of latent class
##            membership.
##
## Input:     02-subset-evs5.Rds
##
## Output:    fig7.jpg
##            fig9.jpg
##            table_a1.docx
##
## Project:   cleavage-identity
## Author:    Martin Lukk / 2024-11-19 (created)

# 0. Program Setup --------------------------------------------------------
library(tidyverse)
library(here)
library(survey)
library(srvyr)
library(nnet)
library(modelsummary)
library(ggtext)
library(patchwork)
library(lmerTest)
library(broom.mixed)
library(margins)
library(gtsummary)
library(flextable)
library(officer)

plot_estimates <- function(model_list, coefs = NULL, add_rows = NULL, ylab = "", caption = "") {
  modelplot(
    list(
      "Particularist" = model_list[["class_particI"]],
      "Expansivist" = model_list[["class_expansI"]],
      "Pluralist" = model_list[["class_pluralI"]],
      "Disengaged" = model_list[["class_disengI"]]
    ),
    coef_map = rev(coefs),
    add_rows = add_rows,
    draw = F
  ) %>% 
    mutate(across(c(estimate, std.error, conf.low, conf.high), ~ .x * 100)) %>%
    ggplot(aes(
      y = term,
      x = estimate,
      xmin = conf.low,
      xmax = conf.high,
      color = model,
      shape = model
    )) +
    geom_vline(xintercept = 0,
               colour = "black",
               linetype = "dashed") +
    geom_pointrange(position = position_dodge(width = .7),
                    show.legend = F) +
    scale_color_brewer(palette = "Set1") +
    scale_fill_brewer(palette = "Set1") +
    labs(
      x = "Average Marginal Effect and 95% Confidence Interval",
      y = ylab,
      caption = caption
    ) +
    facet_grid( ~ model) +
    theme_bw() +
    theme(plot.caption = element_text(hjust = 0),
          axis.text.y = element_markdown())
}

# 1. Load Data ------------------------------------------------------------
df <- read_rds(here("data", "output", "02-subset-evs5.Rds"))

# 2. Code Political Party Families ----------------------------------------
df <-
  df %>%
  mutate(
    partyvote = str_trim(partyvote_f),
    partyvote = case_when(
      # NA
      partyvote %in% c("No answer",
                       "Not applicable") ~ NA,
      # None
      partyvote %in% c(
        "Don't know",
        "CH: No (no other) party appeals to me",
        "DK: Blank vote",
        "HU: No [no other] party appeals to me",
        "PL: No (no other) party appeals to me",
        "FI: No [no other] party appeals to me",
        "FR: No [no other] party appeals to me (spontaneous)",
        "NL: No [no other] party appeals to me (spontaneus)",
        "IS: No (no other) party appeals to me"
      ) ~ "None",
      # Other
      partyvote %in% c(
        "CH: Other, please specify (WRITE IN)",
        "DE: Other, please specify (WRITE IN)",
        "HU: Other, please specify (WRITE IN)",
        "PL: Other, please specify (WRITE IN)",
        "NL: Other, please specify (WRITE IN)",
        "SE: Other",
        "NO: Other, please specify (WRITE IN)",
        "IS: Other, please specify (WRITE IN)",
        # CH
        "CH: Ticino League",
        "CH: Evangelical People's Party",
        "CH: Swiss Labour Party",
        "CH: Movement of the Citizens of French-speaking Switzerland",
        "CH: Pirate Party",
        "CH: Federal Democratic Union",
        "CH: The alternative Left",
        "CH: Conservative Democratic Party",
        # DK
        "DK: Conservative Peoples Party",
        # HU
        "HU: Hungarian Two-tailed Dog Party (MKKP)",
        "HU: Momentum Movement (Momentum)",
        # PL
        "PL: Liberty (Korwin)",
        "PL: Polish Peasants' Party",
        "PL: Together Party",
        # FI
        "FI: Other",
        "FI: Center party",
        "FI: Swedish People's Party in Finland",
        "FI: National Coalition Party",
        # FR
        "FR: France Arise",
        "FR: Other",
        "FR: Other Left Wing Parties (Radical Leftist Party, Republicain's and Citizen's Movement)",
        "FR: Other Right Wing Extremist Parties (The Patriots, National Republican Movement)",
        "FR: Union of Democrats and Independents",
        "FR: Democratic Movement",
        "FR: The Republicans",
        # NL
        "NL: 50Plus",
        "NL: Reformed Political Party",
        # SE
        "SE: Center party",
        "SE: Moderate party",
        # NO
        "NO: Centre Party",
        "NO: Coastal Party",
        "NO: Democrats in Norway",
        "NO: Pensioners' Party",
        "NO: The Christians",
        "NO: Conservative Party",
        # IS
        "IS: Dawn - The organization of justice, fairness and democracy",
        "IS: The Humanist Party",
        "IS: The People's Front of Iceland",
        "IS: The Progressive Party",
        "IS: Reform Party",
        "IS: The Independence Party"
      ) ~ "Other",
      # Social Democratic
      partyvote %in% c(
        "CH: Social Democratic Party (socialist)",
        "DE: German Social-Democratic Party",
        "DK: The Social Democrats",
        "HU: Democratic Coalition (DK)",
        "HU: Dialogue for Hungary (PM)",
        "HU: Hungarian Liberal Party (Liberálisok)",
        "HU: Hungarian Socialist Party (MSZP)",
        "HU: Together – Party for a New Era (Együtt)",
        "PL: Democratic Left Alliance",
        "FI: Social democratic party",
        "FR: Socialist Party",
        "NL: Denk",
        "NL: Labour Party",
        "SE: Social democratic party",
        "NO: Labour Party"
      ) ~ "Social Democratic",
      # Radical Right
      partyvote %in% c(
        "CH: Swiss People's Party",
        "DE: Alternative for Germany",
        "DK: Danish People Party",
        "DK: The New Right",
        "HU: Fidesz",
        "HU: Movement for a Better Hungary (Jobbik)",
        "PL: Kukiz'15",
        "PL: Law and Justice",
        "FI: True Finns (right, small, conservative) - PS",
        "FR: National Front",
        "NL: Forum for Democracy",
        "NL: Party for Freedom",
        "SE: Sweden democrats",
        "NO: Progress Party",
        "IS: The Icelandic National Front"
      ) ~ "Radical Right",
      # Liberal
      partyvote %in% c(
        "CH: The Liberals (Merge from Radicals and Liberals)",
        "DE: German Liberal Party",
        "DK: Danish Social Liberal Party",
        "DK: Venstre, Denmarks Liberal Party",
        "DK: Liberal Alliance",
        "PL: Citizens' Platform",
        "PL: Modern",
        "FR: Act – The constructive right",
        "FR: The Republic Onwards",
        "NL: Democrats 66",
        "NL: People's Party for Freedom and Democracy",
        "SE: Liberals",
        "NO: Liberal Party",
        "IS: Bright Future",
        "IS: Centre Party",
        "IS: Pirate Party"
      ) ~ "Liberal",
      # Green
      partyvote %in% c(
        "CH: Green Party",
        "CH: Green Liberal Party",
        "DE: The Green Party",
        "DK: The Alternative",
        "HU: Politics Can Be Different (LMP)",
        "FI: Green league",
        "FR: Europe Ecology – The Greens",
        "FR: Other Environmentalist Parties",
        "NL: GreenLeft",
        "NL: Party for the Animals",
        "SE: Green party",
        "NO: Green Party",
        "IS: The Left-Green Movement"
      ) ~ "Green",
      # Christian Democratic
      partyvote %in% c(
        "CH: Christian Democratic Party",
        "DE: Christian Democratic Party/Christian Social Union",
        "HU: Christian Democratic People's Party",
        "FI: Christian democrats",
        "NL: Christian Democratic Appeal",
        "NL: Christian Union",
        "SE: Christian Democratic Party",
        "NO: Christian Democratic Party"
      ) ~ "Christian Democratic",
      # Communist/Left Socialist
      partyvote %in% c(
        "DE: The Left",
        "DK: Socialist Peoples Party",
        "DK: Red-Green Alliance",
        "HU: Hungarian Workers' Party (Munkáspárt)",
        "FI: Left alliance",
        "FR: Communist Party",
        "FR: Left Wing Extremist Parties (New Anticapitalist Party, Workers' Struggle, Independent Workers' Party)",
        "FR: Unsubmissive France",
        "NL: Socialist Party",
        "SE: Left wing party",
        "NO: Socialist Left Party",
        "NO: Red Party",
        "IS: The People's Party",
        "IS: The Social Democratic Alliance"
      ) ~ "Communist/Left Socialist",
      .default = partyvote
    ),
    rrI = if_else(partyvote == "Radical Right", 1, 0),
    sdmI = if_else(partyvote == "Social Democratic", 1, 0),
    grnI = if_else(partyvote == "Green", 1, 0),
    cdmI = if_else(partyvote == "Christian Democratic", 1, 0),
    libI = if_else(partyvote == "Liberal", 1, 0),
    lftI = if_else(partyvote == "Communist/Left Socialist", 1, 0),
    nonI = if_else(partyvote == "None", 1, 0)
  )

# 3. Recode Covariates ----------------------------------------------------
df <-
  df %>% 
  mutate(
    # Collapse covariate categories
    edlvl = case_when(
      edlvl %in% c("Less than primary", "Primary", "Lower secondary") ~ "Less than secondary",
      edlvl %in% c("Upper secondary") ~ "Secondary",
      edlvl %in% c("Post-secondary non tertiary", "Short-cycle tertiary") ~ "Some post-secondary",
      edlvl %in% c("Bachelor or equivalent", "Master or equivalent", "Doctoral or equivalent") ~ "University or higher", 
      .default = edlvl),
    # Collapse covariate categories
    religion = case_when(
      religion %in% c("Buddhist", "Hindu", "Jewish", "Orthodox") ~ "Other",
      .default = religion),
    # Generate and modify factor variables
    across(c(class, partyvote, edlvl, religion, bornI, femaleI, country_f), as_factor),
    across(c("age"),  ~ as.vector(scale(.x, center = TRUE, scale = TRUE)), .names = "{.col}_s"),
    religion = fct_relevel(religion, "Protestant"),
    partyvote = fct_relevel(partyvote, "Liberal"),
    edlvl = fct_relevel(edlvl, "Secondary"),
    townsize = fct_relevel(townsize, "20,000-100,000"),
    class_particI = if_else(class == "2", 1, 0),
    class_expansI = if_else(class == "3", 1, 0),
    class_pluralI = if_else(class == "1", 1, 0),
    class_disengI = if_else(class == "4", 1, 0)
  )

# 4. Fit Logistic Regression Models ---------------------------------------
rhs <- ~ partyvote + age_s + femaleI + edlvl + income_mo_log + bornI + religion + townsize + (1 | country_f)
lhs <- c("class_particI", "class_expansI", "class_pluralI", "class_disengI")
models <- map(lhs, ~ update(rhs, as.formula(paste(.x, "~ .")))) %>% set_names(lhs)

fits <- map(models, ~ glmer(.x, df, binomial(link = "logit")) %>% margins)

# 5. Plot Results ---------------------------------------------------------
# Party choice
add_rows <-
  tibble(
    term = c("Liberal", "Liberal", "Liberal", "Liberal"),
    estimate = c(0, 0, 0, 0),
    model = c("Particularist", "Expansivist", "Pluralist", "Disengaged")
  )
attr(add_rows, "position") <- c(20, 32, 44, 56)

p_ame_party <-
  plot_estimates(
    fits,
    coef = c(
      "partyvoteRadical Right" = "Radical Right",
      "partyvoteChristian Democratic" = "Christian Democratic",
      "partyvoteLiberal" = "Liberal", # Reference category
      "partyvoteSocial Democratic" = "Social Democratic",
      "partyvoteGreen" = "Green",
      "partyvoteCommunist/Left Socialist" = "Communist/Socialist",
      "partyvoteOther" = "Other",
      "partyvoteNone" = "None"
    ),
    add_rows = add_rows,
    ylab = "Political Party Preference",
    # caption = 'Note: Estimates are from mixed effects logistic regression models fit to 2017-2020 European Values Study data for Denmark, Finland, France, Germany, \nHungary, Iceland, Netherlands, Norway, Poland, Sweden, and Switzerland. The reference category is the "Liberal" party family. Response category "Other" \naggregates parties belonging to various smaller party families in the analytic sample, including conservative, agrarian, regional, linguistic, and protest parties. \nModels include country random intercepts and controls for age, sex, education, income, native birth, community size, and religion.'
  ) + 
  scale_x_continuous(limits = c(-16, 16), breaks = c(-15, -10, -5, 0, 5, 10, 15))

suppressWarnings(
  ggsave(here("figures", "fig9.jpg"), plot = p_ame_party, device = "jpg",
         width = 10, height = 3.75, units = "in", dpi = 600)
)

# Demographics
add_rows <-
  tibble(
    term = rep(c("", "**Education**", "Secondary",
                 " ", "**Community Size**", "20,000-100,000",
                 "  ", "**Religion**", "Protestant"), 4),
    estimate = rep(c(NA, NA, 0, NA, NA, 0, NA, NA, 0), 4),
    model = rep(c("Particularist", "Expansivist", "Pluralist", "Disengaged"), each = 9)
  )
attr(add_rows, "position") <- c(
  # Particularist positions
  42,42,41,32,31,25,17,17,17,
  # Expansivist positions
  121,120,119,123,122,124,126,125,127,
  # Pluralist positions
  130,129,128,132,131,133,135,134,136,
  # Disengaged positions
  139,138,137,141,140,142,144,143,145
)

p_ame_demog <-
  plot_estimates(
    fits,
    coef = c(
      "age_s" = "Age",
      "femaleI1" = "Female",
      "bornI1" = "Native-born",
      "income_mo_log" = "Income",
      "edlvlLess than secondary"= "Less than secondary",
      "edlvlSome post-secondary" = "Some post-secondary",
      "edlvlUniversity or higher" = "University or higher",
      "townsizeunder 5,000" = "Under 5,000",
      "townsize5,000-20,000" = "5,000-20,000",
      "townsize100,000-500,000" = "100,000-500,000",
      "townsize500,000 and more" = "500,000 and more",
      "religionCatholic" = "Catholic",
      "religionNone" = "None",
      "religionMuslim" = "Muslim",
      "religionOther" = "Other"
    ),
    add_rows = add_rows,
    ylab = "Demographic Characteristics",
    # caption = 'Note: Estimates are from mixed effects logistic regression models fit to 2017-2020 European Values Study data for Denmark, Finland, France, Germany, \nHungary, Iceland, Netherlands, Norway, Poland, Sweden, and Switzerland. The reference categories are "secondary" (education), "20,000-100,000" \n(community size), and "Protestant" (religion). Models include country random intercepts and controls for political party choice.'
  ) + 
  scale_x_continuous(limits = c(-11, 11), breaks = c(-10, -5, 0, 5, 10))

suppressWarnings(
  ggsave(here("figures", "fig7.jpg"), plot = p_ame_demog, device = "jpg",
         width = 10, height = 5.75, units = "in", dpi = 600)
)

# 6. Generate Descriptive Table -------------------------------------------
df_tbl <- 
  df |> 
  select(country, femaleI, age, edlvl, income_mo, bornI, townsize, 
         religion, partyvote)

df_tbl_sum <- 
  df_tbl %>% 
  add_count(name = "n_wave") %>%  
  mutate(n_country = n_distinct(country)) %>%  
  select(femaleI, age, edlvl, income_mo, bornI, townsize, religion, partyvote, n_wave, n_country) %>% 
  mutate(across(c(edlvl, townsize, religion, partyvote), ~ as.factor(.x)),
         across(c(n_wave, n_country), ~ as.double(.x)),
         # Convert income to thousands of Euros
         income_mo = income_mo * 1000,
         # Keep binary variables as factors but with meaningful labels
         femaleI = factor(femaleI, levels = c("0", "1"), labels = c("Male", "Female")),
         bornI = factor(bornI, levels = c("0", "1"), labels = c("Not born in country", "Born in country")),
         # Recode factors in logical order
         edlvl = fct_relevel(edlvl,
                             "Less than secondary",
                             "Secondary", 
                             "Some post-secondary",
                             "University or higher"
         ),
         # Filter out unwanted townsize categories
         townsize = case_when(
           townsize %in% c("under 5,000", "5,000-20,000", "20,000-100,000", 
                           "100,000-500,000", "500,000 and more") ~ as.character(townsize),
           TRUE ~ NA_character_
         ),
         townsize = factor(townsize, levels = c("under 5,000", "5,000-20,000", 
                                                "20,000-100,000", "100,000-500,000", 
                                                "500,000 and more"))
  )

# Create gtsummary table
theme_gtsummary_journal(journal = "qjecon", set_theme = TRUE)
sum_tbl_new <-
  df_tbl_sum %>%
  tbl_summary(
    type = c(n_wave, n_country) ~ "continuous",
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{p}%",
                     c(n_wave, n_country) ~ "{mean}"),
    digits = list(all_continuous() ~ 2,
                  all_categorical() ~ 0,
                  c(n_wave, n_country) ~ 0),
    label = list(
      age ~ "Age",
      femaleI ~ "Sex",  
      edlvl ~ "Highest completed education",
      income_mo ~ "Monthly income (Euros)",  
      bornI ~ "Born in country",
      townsize ~ "Town size",
      religion ~ "Religious affiliation",
      partyvote ~ "Party vote choice",
      n_wave ~ "N (individuals)",
      n_country ~ "N (countries)"
    ),
    missing = "no"
  ) %>%
  modify_header(list(label ~ "Variable",
                     stat_0 ~ "Statistics")) %>%
  modify_footnote(update = everything() ~ NA)

# Convert to flextable and style
desc_tbl_word <- sum_tbl_new |>
  as_flex_table() |>
  border_remove() |>
  hline_top(border = fp_border(color = "black", width = 1), part = "header") |>
  hline_bottom(border = fp_border(color = "black", width = 1), part = "header") |>
  hline_bottom(border = fp_border(color = "black", width = 1), part = "body") |>
  align(j = 2, align = "center", part = "all") |>
  font(fontname = "Arial", part = "all") |>
  padding(padding.top = 1, padding.bottom = 1, part = "all") |> 
  set_table_properties(layout = "autofit", width = 0.8) |>
  add_footer_lines("Note: Cells contain means and standard deviations (in parentheses) for continuous variables and category percentages for discrete variables. Includes samples from subset of countries with particularist-expansivist identity divide, analyzed in sections 3.3 and 3.4. Percentages across variable categories may not sum to one due to rounding.")

save_as_docx(desc_tbl_word, path = "figures/table_a1.docx")
