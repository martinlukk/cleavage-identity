
## Program:   02-plot-crosssect.R
## Task:      Generate descriptive figure of identity variables in EVS5 data.
##
## Input:     01-clean-evs5.Rds
## Output:    fig2.jpg
##
## Project:   cleavage-identity
## Author:    Martin Lukk / 2024-07-26 (created)

# 0. Program Setup --------------------------------------------------------
library(tidyverse)
library(here)
library(survey)
library(srvyr)
library(rlang)

# 1. Load Data ------------------------------------------------------------
df <- read_rds(here("data", "output", "01-clean-evs5.Rds")) %>%
  mutate(
    # Create factor variables
    across(c(starts_with("cls"), country_f, year), as.factor),
    # Shorten come country labels
    country_f = fct_recode(country_f,
                           "Czechia" = "Czech Republic")
  )

# 2. Compute Summary Data -------------------------------------------------

# Create summary data frame for mean response per country and variable
df_sum <- 
  df %>% 
  # Treat responses as numeric variables
  mutate(across(starts_with("cls"), as.numeric)) %>% 
  # Rename "close to" variables by scope of variable
  rename(cls5 = clsworld,
         cls4 = clscontinent,
         cls3 = clscountry,
         cls2 = clsregion,
         cls1 = clstown) %>%
  # Declare survey weights
  as_survey_design(weights = c(weighta)) %>%
  # Group by country
  group_by(country_f) %>%
  # Compute weighted avg response and errors for each "close to" variable
  summarise(across(starts_with("cls"), ~ survey_mean(.x, vartype = "ci", na.rm = T))) %>% 
  rename_with(~ str_c(., "_mean"), .cols = !contains("_")) %>% 
  # Reshape longer
  pivot_longer(cols = !country_f,
               names_to = c("clslvl", "stat"),
               names_sep = "_") %>%
  # Reshape "stat" column wider
  pivot_wider(names_from = stat,
              values_from = value) %>% 
  # Clean up values
  mutate(clslvl = str_remove(clslvl, "cls"),
         clslvl = as.factor(clslvl)) %>% 
  # Compute overall mean
  group_by(country_f) %>%
  mutate(overall_mean = mean(mean)) %>%
  ungroup() %>%
  # Assign labels to clslvl
  mutate(
    clslvl = fct_recode(
      clslvl,
      "Town/City" = "1",
      "Region" = "2",
      "Country" = "3",
      "Continent" = "4",
      "World" = "5"
    )
  )

# 3. Plot average response by country for all variables -------------------

# Create bar graph faceted by country
p_out <-
    df_sum %>%
    ggplot(aes(x = clslvl, y = mean)) +
    geom_bar(aes(fill = clslvl, color = clslvl), stat = "identity", alpha = .85) +
    geom_errorbar(aes(ymin = low, ymax = upp), linewidth = .5, width = .05,
                position = position_dodge(width = .9)) +
    facet_wrap( ~ fct_reorder(country_f, -(overall_mean)), ncol = 5) +
    scale_x_discrete(breaks = NULL) +
    coord_cartesian(ylim = c(0, 4)) +
    labs(
      # caption = "Note: Data from 2017-2020 European Values Study. Bar heights indicate weighted mean levels of closeness reported for five different territorial identities by \ncountry. Error bars indicate 95% confidence intervals. Countries are sorted by average closeness across all territorial identities.",
      x = "",
      y = "",
      fill = 'How close do you feel to:',
      color = 'How close do you feel to:'
    ) +
    theme_bw() +
    scale_color_brewer(palette = "Paired") +
    scale_fill_brewer(palette = "Paired") +
    theme(legend.position = "bottom",
          plot.caption = element_text(hjust = 0))

# 4. Save Plot ------------------------------------------------------------
ggsave(here("figures", "fig2.jpg"), plot = p_out, device = "jpg",
      width = 9.25, height = 9, units = "in", dpi = 600)
