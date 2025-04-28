# Copyright 2025 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# This script loads aggregated csv data files containing spatial data for
# municipality of interest in BC (loc).
# It plots the locations on top of a map of B.C.

# ------------------------------------------------------------------------
# Script: 02b-plot-localities.R

# Description: plot localities on a map of BC

# Requirements:
#   - Requires R packages: `tidyverse`, `glue`, `janitor`, `sf`. `rmapshaper`, `rnaturalearth`
#   - Depends on `settings.R` for paths and constants.
#   - Requires input DA/DB shapefiles and the crosswalk CSV file.
#   - Requires read/write access to relevant data folders.

# Side Effects/Outputs:
#   - Makes a map of localities we used
# ------------------------------------------------------------------------

library(tidyverse)
library(glue)
library(janitor)
library(sf)
library(rmapshaper)    # simplify geometries
library(bcmaps) # get BC outline

source("R/settings.R")

# ----------------------------------------------------------------------------
# Load and prepare mapping data
# ----------------------------------------------------------------------------

# map of BC
bc_map <- bc_bound() |>
  st_transform(crs = 3005)

# csd level shapefiles for each locality
csd_shapefile <- 
  st_read(glue("{SHAPEFILE_OUT}/processed_csd_with_location.gpkg")) %>%
  mutate(across(c(csd_name), as.character),
         across(c(landarea), as.numeric))

# get centroids for labels
csd_centroids <- st_centroid(csd_shapefile) 
csd_centroids_nudged <- csd_centroids |> 
  mutate(
      geom = case_when(
        csd_name == 'Dawson Creek' ~ geom + c(-150000, 40000),
        csd_name == 'Langford' ~ geom + c(130000, 10000), # move east for langford
        TRUE ~ geom + c(0, 70000)  # move label north to not overlap 
  )
      ) %>%
  st_set_crs(st_crs(csd_centroids))

# locations of all nearby SBC locations
sbc_locs <- read_csv(glue("{SRC_DATA_FOLDER}/service_bc_locs.csv")) |>
  st_as_sf(
    coords = c('coord_x', 'coord_y'),
    crs = 3005
  ) 

#-----------------------------------------------------------------------------
# build map 
#-----------------------------------------------------------------------------

csds <- csd_shapefile |>
  filter(csd_name %in% CSD_NAMES)

csd_centroids_nudged <- csd_centroids_nudged |>
  filter(csd_name %in% CSD_NAMES)

sbc_locs <- sbc_locs |>
  filter(csd_name %in% CSD_NAMES)

out <- ggplot() + 
  geom_sf(
    data = bc_map,
    fill = 'white',
    color='black'
    ) +
  geom_sf(
    data = csds, 
    aes(fill = as.factor(csd_name)),
    color = 'darkgrey',
    alpha = 1
  ) +
  geom_sf(
    data = sbc_locs,
    shape = 23,
    fill = 'yellow',
    color = 'black',
    size = 2,
    stroke = 1.1
  ) +
  geom_sf_text(
    data = csd_centroids_nudged,
    aes(label = csd_name), 
    size=4, 
    fontface='bold'
    ) +
  scale_fill_brewer(
    palette = 'Set2',
    name = 'Locality'
    ) +
  coord_sf(expand = FALSE) + # remove whitespace
  labs(
    title = 'Pilot Regions',
    
  ) +
  MAP_THEME +
  theme(
    legend.position = "none",
    axis.title = element_blank()
    )

out

# save
ggsave(
   filename = glue("pilot_regions.png"),
   path = MAP_OUT,
   plot = out,
   width = 8,
   height = 7,
   dpi = 300
)
