
## Program:   07-plot-regions.R
## Task:      Plot geographical distribution of latent classes.
##
## Input:     02-subset-evs5.Rds
## Output:    fig8.jpg
##
## Project:   cleavage-identity
## Author:    Martin Lukk / 2024-01-13 (created)

# 0. Program Setup --------------------------------------------------------

# Note: {sf} and its dependencies don't work well with {renv} package
# management. For best results, open this script in a new RStudio Project
# or Positron folder (i.e., without initiating {renv}). Then specify the
# original working directory in the line below (../cleavage-identity/)
# and run the code as usual. This is only required for scripts that produce maps.

working_directory <- "../cleavage-identity"
setwd(working_directory)

library(tidyverse)
library(survey)
library(srvyr)
library(sf)
library(giscoR)
library(rcartocolor)
library(maps)

summarize_class <- function(data, country_name, summary_var) {
  
  data <- data %>% filter(country_f == country_name)
  
  varname <- summary_var
  isfactor <- is.factor(data[[summary_var]])
  
  if (isfactor == TRUE) {
    
    df <- 
      data %>%
      select(class, weighta, !!sym(varname)) %>%
      drop_na() %>%
      as_survey_design(weights = c(weighta)) %>%
      group_by(!!sym(varname), class) %>%
      summarize(value = survey_prop(vartype = "ci", proportion = TRUE)) %>%
      ungroup() %>%
      rename(NUTS_ID = varname) %>%
      relocate(class, NUTS_ID, value)
    
  } else {
    
    print("Input variable doesn't exist or type is not factor!")
    
  }
  
}
get_propdiff <- function(data) {
  data |> 
    select(class, NUTS_ID, value) |> 
    pivot_wider(names_from = class,
                values_from = value,
                names_prefix = "class_") |> 
    mutate(propdiff = class_3 - class_2)
}

# 1. Load Data ------------------------------------------------------------
df <-
  read_rds("data/output/02-subset-evs5.Rds") |> 
  select(country_f, class, starts_with("region"), starts_with("weight")) |> 
  mutate(region2 = if_else(country_f == "Germany", region1, region2), # DE only has NUTS1 regions
         across(c("class", starts_with("region")), as.factor))

# 2. Summarize Data -------------------------------------------------------
countries_data <-
  df |> 
  pull(country_f) |> 
  unique()

df_sum <- 
  purrr::map(countries_data, ~ summarize_class(df, .x, "region2")) |> 
  purrr::map(get_propdiff) |> 
  bind_rows()

# 3. Load Shapefiles ------------------------------------------------------
year <- 2016
projection <- 3035

# Get country borders
countries_europe <-
  giscoR::gisco_countrycode |> 
  filter(un.region.name  == "Europe") |> 
  filter(!(ISO3_CODE %in% c("ALA"))) |> 
  pull(ISO3_CODE)

sf_borders <- gisco_get_countries(country = countries_europe,
                          year = year,
                          epsg = projection)

sf_outline <- 
  sf_borders %>%
  filter(NAME_ENGL %in% countries_data)

# Get NUTS2 region borders
sf_nuts2 <-
  gisco_get_nuts(
    # DE has only NUTS1 regions
    country = countries_data[countries_data != "Germany"],
    nuts_level = 2,
    year = year,
    epsg = projection
  )

# Get DE NUTS1 region borders
sf_nuts1_de <-
  gisco_get_nuts(
    country = "DE", nuts_level = 1, year = year, epsg = projection)

# Get names of largest cities in each country
cities_largest <-
  world.cities |>
  filter(country.etc %in% countries_data) |>
  slice_max(order_by = pop, by = "country.etc") |> 
  select(name, lat, long)

sf_cities <- 
  cities_largest |> 
  st_as_sf(coords = c("long", "lat"), crs = 4326) |> 
  st_transform(crs = projection)

sf <- bind_rows(sf_borders, sf_nuts2, sf_nuts1_de)

# 4. Merge Data -----------------------------------------------------------
map_data <- 
  sf %>%
  left_join(df_sum, by = "NUTS_ID")

# 5. Plot ----------------------------------------------------------------
p_classprop_map2 <- 
   ggplot() +
   # Class shares
   geom_sf(data = map_data, aes(fill = propdiff), alpha = .85, linewidth = .10) +
   scale_fill_carto_c(type = "divergent",
                      palette = "Geyser", direction = -1,
                      limits = c(-.41, .41)
   ) +
   # Borders and cities
   geom_sf(data = sf_outline, fill = NA, color = "black", linewidth = .15) +
   geom_sf(data = sf_cities, size = 1.2, shape = 15, color = "black") +
   labs(
    fill = "Relative class share\n",
    # caption = "Note: Values computed using estimates from a 4-class structurally homogeneous latent class model fitted to the 2017-2020 \nEuropean Values Study. Values indicate the difference in estimated regional population shares between Expansivist and \nParticularist classes, with higher values indicating a greater share of Expansivists relative to Particularists. Square markers \nindicate location of each country's largest population center. Unshaded countries do not have significant expansivist classes."
  ) +
   coord_sf(xlim = c(2820000, 5380000), ylim = c(2270000, 5240000), expand = T) +
   theme_void() +
   theme(legend.position = "bottom",
         plot.caption = element_text(hjust = 0.00))

# 6. Save ----------------------------------------------------------------
ggsave("figures/fig8.jpg",
       plot = p_classprop_map2, device = "jpg",
       width = 7, height = 8.7, units = "in", dpi = 400)
