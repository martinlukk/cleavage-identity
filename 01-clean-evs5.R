

## Program:   01-clean-evs5.R
## Task:      Load and clean data from EVS Wave 5 for analysis, save new file.
##
## Input:     ZA7503_v2-0-0.dta
## Output:    01-clean-evs5.Rds
##
## Project:   cleavage-identity
## Author:    Martin Lukk / 2024-07-20 (created)


# 0. Program Setup --------------------------------------------------------
library(tidyverse)
library(haven)
library(here)

# 1. Load Data ------------------------------------------------------------
evs5 <-
  read_dta(here("data", "input", "ZA7503_v2-0-0.dta")) %>% 
  filter(S002EVS %in% c(5)) # Subset to Wave 5 (2017-20)

# 2. Subset variables of interest, rename, and parse ----------------------

# Specify variables of interest and new names
new_old_varnames <-
  tribble(
  ~ newname,        ~ oldname,  ~ is_factor,
  "wave" ,          "S002EVS",  TRUE,
  "country",        "S003",     TRUE,
  "id",             "S006",     FALSE,
  "int_date",       "S012",     FALSE,
  "weighta",        "S017",     FALSE,
  "weighteq",       "S018",     FALSE,
  "weightpop",      "pwght",    FALSE,
  "year",           "S020",     TRUE,
  "country_year",   "S025",     FALSE,
  "country_wave",   "S024",     FALSE,
  # Demographics
  "sex",            "X001",     TRUE,
  "age",            "X003",     FALSE,
  "marital",        "X007",     TRUE,
  "partner",        "X007_02",  TRUE,
  "empstat",        "X028",     TRUE,
  "supervisor",     "X031",     TRUE,
  "unemp_3moin5yr", "X037_01",  TRUE,
  "hhsize",         "X013",     FALSE,
  "edlvl",          "X025R",    TRUE,
  "edlvl2",         "X025A_01", TRUE,
  "edlvl3",         "X025CSEVS",TRUE,
  "occ1",           "X036A",    TRUE,
  "occ2",           "X036B",    TRUE,
  "occ3",           "X036C",    TRUE,
  "occ4",           "X036D",    TRUE,
  "income10",       "X047_EVS", TRUE,
  "income3",        "X047R_EVS",TRUE,
  "income_mo",      "X047D",    FALSE,
  "citizen",        "G005",     TRUE,
  "immigrant",      "G027A",    TRUE,
  "birth_country",  "X002_02A", TRUE,
  "imm_year",       "X002_03",  FALSE,
  "region1",        "X048H_N1", TRUE,
  "region2",        "X048I_N2", TRUE,
  "townsize",       "X049a",    TRUE,
  "religion",       "F025",     TRUE,
  # Attitudes
  "partyvote",      "E181A",    TRUE,
  "partyvote_lr",   "E181C",    TRUE,
  "clscontinent",   "G062",     FALSE,
  "clsworld",       "G063",     FALSE,
  "clstown",        "G255",     FALSE,
  "clsregion",      "G256",     FALSE,
  "clscountry",     "G257",     FALSE,
  "impborn",        "G033",     TRUE,
  "impresp",        "G034",     TRUE,
  "impanc",         "G035",     TRUE,
  "implang",        "G036",     TRUE
  )

# Create vector of names for factor variables
factor_vars <- new_old_varnames %>% filter(is_factor == T) %>% pull(newname)

# Create analysis data set
df <- 
  evs5 %>% 
  # Select variables
  select(all_of(new_old_varnames$oldname)) %>% 
  # Set Stata missing values to NA
  haven::zap_missing() %>% 
  # Rename with new variable names
  rename(set_names(new_old_varnames$oldname, new_old_varnames$newname)) %>% 
  # Convert labelled variables to factors
  mutate(across(any_of(factor_vars), ~ as_factor(.x),  .names = "{.col}_f"),
         across(where(is.labelled), zap_labels),
         country_f = str_trim(country_f)) %>% 
  select(country, country_f, id, year,
         starts_with("weight"),
         sex, age, edlvl2_f, occ3_f, region1, region2, income_mo,
         citizen, immigrant, religion_f, townsize_f, partyvote_f, partyvote_lr,
         starts_with("cls"))

# 3. Recode variables -----------------------------------------------------

# Outcome variables
cls_vars <- c("clstown", "clsregion", "clscountry", "clscontinent", "clsworld")
df <- df %>% 
  mutate(
    # Recode cls variable negative values as NA
    across(all_of(cls_vars), ~ if_else(.x < 0, NA_real_, .x)),
    # Reverse code cls variables (so that greater values indicate more support)
    across(all_of(cls_vars), ~ (5 - .x))
  )

# Covariates
ctrl_vars <- c("sex", "age", "income_mo", "citizen", "immigrant", "partyvote_lr")

df <- df %>% 
  mutate(
    # Recode ctrl variable negative values as NA
    across(all_of(ctrl_vars), ~ if_else(.x < 0, NA_real_, .x)),
    # Drop NAs for factor variables
    townsize = if_else(
      townsize_f %in% c(
        "Not asked in survey",
        "No answer",
        "Don't know"
      ), NA, townsize_f
    ),
    edlvl = if_else(
      edlvl2_f %in% c(
        "Missing: Other",
        "Not asked in survey",
        "Not applicable",
        "No answer",
        "Don´t know"
      ), NA, edlvl2_f
    ),
    occ = if_else(
      occ3_f %in% c(
        "Missing: Other",
        "Not applicable",
        "No answer",
        "Don´t know"
      ), NA, occ3_f
    ),
    religion = case_when(
      religion_f %in% c("No answer", "Don´t know") ~ NA,
      religion_f == "Do not belong to a denomination" ~ "None",
      religion_f == "Roman Catholic" ~ "Catholic",
      religion_f == "Orthodox (Russian/Greek/etc.)" ~ "Orthodox",
      religion_f == "Jew" ~ "Jewish",
      religion_f %in% c("Other Christian (Evangelical/Pentecostal/Free church/etc.)", "Other") ~ "Other",
      .default = religion_f
    ),
    across(all_of(starts_with("region")), ~ if_else((.x == "-1" | .x == "-4"), NA_character_, .x)),
    # Recode specific variables
    femaleI = case_when(
      sex == 1 ~ 0,
      sex == 2 ~ 1
      ),
    immigrantI = case_when(
      immigrant == 1 ~ 0,
      immigrant == 2 ~ 1
      ),
    bornI = case_when(
      immigrantI == 1 ~ 0,
      immigrantI == 0 ~ 1
    ),
    income_mo_log = log(income_mo),
    partyvote_lr = if_else(partyvote_lr == 44, NA_real_, partyvote_lr),
  ) %>% 
  rename(citizenI = citizen)

# Subset cleaned variables for analysis
df <- df %>%
  select(
    country, country_f, id, year, starts_with("weight"),
    femaleI, age, edlvl, income_mo, income_mo_log, citizenI, bornI, townsize,
    partyvote_lr, partyvote_f, starts_with("region"), religion, occ,
    all_of(cls_vars)
  )

# 4. Save cleaned data set to file ----------------------------------------
write_rds(df, here("data", "output", "01-clean-evs5.Rds"))
