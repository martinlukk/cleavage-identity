
## Program:   05-plot-cooccurrence.R
## Task:      Plot co-occurrence of countries across cluster solutions in
##            latent class models.
##
## Input:     clusters_4cl.rds
## Output:    fig5.jpeg
##
## Project:   cleavage-identity
## Author:    Martin Lukk / 2025-01-10 (created)

# 0. Program Setup --------------------------------------------------------
library(tidyverse)
library(here)
library(reshape2)
library(igraph)
library(RColorBrewer)
set.seed(12345)

# 1. Load Data ------------------------------------------------------------
cluster_list <- read_rds(here("data", "output", "clusters_4cl.rds"))

# 2. Create Co-occurrence Matrix ------------------------------------------
# Initialize matrix
countries <- unique(unlist(cluster_list))
co_matrix <- matrix(0, nrow = length(countries), ncol = length(countries), 
                    dimnames = list(countries, countries))

# Populate matrix
for (model_index in seq_along(cluster_list)) {
  model <- cluster_list[[model_index]]
  
  for (cluster_index in seq_along(model)) {
    cluster <- model[[cluster_index]]

    
    # Only proceed if the cluster has at least two countries
    if (length(cluster) >= 2) {
      # Get all country pairs within this cluster
      pairs <- combn(cluster, 2, simplify = FALSE)
      for (pair in pairs) {
    
        co_matrix[pair[1], pair[2]] <- co_matrix[pair[1], pair[2]] + 1
        co_matrix[pair[2], pair[1]] <- co_matrix[pair[2], pair[1]] + 1
      }
    }
  }
}

# Convert co-occurrence matrix to igraph
g <- graph_from_adjacency_matrix(
  co_matrix, 
  mode = "undirected", 
  weighted = TRUE,
  diag = FALSE
)

# 3. Plot Co-occurrence ---------------------------------------------------
# Country name to ISO code mapping
country_codes <- c(
  "Albania" = "AL",
  "Azerbaijan" = "AZ",
  "Belarus" = "BY",
  "Georgia" = "GE",
  "Russia" = "RU",
  "Ukraine" = "UA",
  "Great Britain" = "GB",
  "Finland" = "FI",
  "Netherlands" = "NL",
  "Sweden" = "SE",
  "Italy" = "IT",
  "North Macedonia" = "MK",
  "Serbia" = "RS",
  "Armenia" = "AM",
  "Bosnia and Herzegovina" = "BA",
  "Croatia" = "HR",
  "Estonia" = "EE",
  "Lithuania" = "LT",
  "Denmark" = "DK",
  "France" = "FR",
  "Germany" = "DE",
  "Hungary" = "HU",
  "Iceland" = "IS",
  "Norway" = "NO",
  "Poland" = "PL",
  "Switzerland" = "CH",
  "Austria" = "AT",
  "Bulgaria" = "BG",
  "Czechia" = "CZ",
  "Montenegro" = "ME",
  "Portugal" = "PT",
  "Romania" = "RO",
  "Slovakia" = "SK",
  "Slovenia" = "SI",
  "Spain" = "ES"
)

# Add abbreviated names
V(g)$name <- country_codes[V(g)$name]

# Detect communities
comm <- cluster_louvain(g)
V(g)$community <- membership(comm)

layout <- layout_with_kk(g)

# Get Set2 palette colors
set2_colors <- brewer.pal(n = max(membership(comm)), name = "Set2")

# Create hull fill colors with transparency
hull_fills <- adjustcolor(set2_colors, alpha.f = 0.2)

jpeg(here("figures", "fig5.jpg"), 
     width = 7, height = 6, units = "in", res = 400)

par(mar = c(0, 0, 0, 0)) 

plot(
  comm, 
  g,
  layout = layout,
  vertex.size = 10,
  vertex.label.cex = 0.7,
  vertex.label.color = "black",
  vertex.label.family = "sans",
  col = set2_colors[membership(comm)],
  edge.width = E(g)$weight / max(E(g)$weight) * 2,
  edge.color = adjustcolor("gray40", alpha = 0.3),
  mark.col = hull_fills,
  mark.border = set2_colors,
  mark.expand = 15
)

dev.off()
