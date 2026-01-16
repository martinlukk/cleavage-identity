
## Program:   06-plot-classprop_map.R
## Task:      Plot class share difference map.
##
## Input:     class_propdiff.rds
## Output:    fig4.jpg
##
## Project:   cleavage-identity
## Author:    Martin Lukk / 2024-01-14 (created)

# 0. Program Setup --------------------------------------------------------

# Note: {sf} and its dependencies don't work well with {renv} package
# management. For best results, open this script in a new RStudio Project
# or Positron folder (i.e., without initiating {renv}). Then specify the
# original working directory in the line below (../cleavage-identity/)
# and run the code as usual. This is only required for scripts that produce maps.

working_directory <- "../cleavage-identity"
setwd(working_directory)

library(sf)
library(dplyr)
library(ggplot2)
library(giscoR)
library(rcartocolor)

# 1. Load Data ------------------------------------------------------------
class_propdiff <- readr::read_rds("data/output/class_propdiff.rds")

# 2. Load Shapefile -------------------------------------------------------
year <- 2016
projection <- 3035

countries <-
  giscoR::gisco_countrycode |> 
  filter(un.region.name  == "Europe" | 
           iso.name.en %in% c("Georgia", "Armenia", "Azerbaijan", "Turkey")) |> 
  filter(!(ISO3_CODE %in% c("ALA", "GBR"))) |> 
  pull(ISO3_CODE)

sf <- gisco_get_countries(country = countries,
                          year = year,
                          epsg = projection)

# Get UK NUTS level 1 regions
uk_regions <-
  gisco_get_nuts(
    country = "UK", nuts_level = 1, year = year, epsg = projection)

# Separate Great Britain and Northern Ireland
uk_split <- uk_regions %>%
  # Group into GB vs NI
  mutate(
    CNTR_NAME = case_when(
      NUTS_ID == "UKN" ~ "Northern Ireland",
      TRUE ~ "Great Britain"
    ),
    ISO3_CODE = case_when(
      NUTS_ID == "UKN" ~ "NIR",
      TRUE ~ "GBR"
    ),
    CNTR_ID = case_when(
      NUTS_ID == "UKN" ~ "NI",
      TRUE ~ "GB"
    ),
    NAME_ENGL = CNTR_NAME
  ) %>%
  group_by(CNTR_NAME, ISO3_CODE, CNTR_ID, NAME_ENGL) %>%
  summarise(geometry = st_union(geometry)) %>%
  ungroup()

sf <- bind_rows(sf, uk_split)

# 3. Merge Data -----------------------------------------------------------
map_data <- 
  sf %>%
  left_join(class_propdiff, by = "ISO3_CODE")

# 4. Plot ----------------------------------------------------------------
p_classprop_map1 <- 
  ggplot(map_data) +
  geom_sf(aes(fill = propdiff)) +
  scale_fill_carto_c(type = "diverging",
                     palette = "Geyser", direction = -1,
                     limits = c(-1, 1)) +
  labs(
      fill = "Relative class share\n",
      #  caption = "Note: Values computed using estimates from structurally heterogeneous latent class models fitted to the 2017-2020 European Values Study. Values indicate the difference in estimated \npopulation shares between Pluralist and Particularist classes, with higher values indicating a greater share of Pluralists relative to Particularists. Unshaded countries are not included\nin the EVS."
      ) +
  xlim(c(2800000, 7400000)) +
  ylim(c(1500000, 5210000)) +
  theme_void() +
  theme(legend.position = "bottom",
        plot.caption = element_text(hjust = 0.00))

# 5. Save ----------------------------------------------------------------
ggsave("figures/fig4.jpg",
       plot = p_classprop_map1, device = "jpg",
       width = 10, height = 8.75, units = "in", dpi = 400)
