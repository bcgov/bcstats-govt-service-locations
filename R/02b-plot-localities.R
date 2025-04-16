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
source("R/fxns/pre-processing.R") # for read_all_locs()
# ----------------------------------------------------------------------------
# Load and prepare mapping data
# ----------------------------------------------------------------------------

# map of BC
bc_map <- bc_bound() |> 
  st_transform(crs = 3005)

# da level shapefiles for each locality
fn <- glue("{SHAPEFILE_OUT}/processed_da_with_location.shp")
da_shapefile <- tryCatch({
  st_read(fn) %>%
    rename("landarea" = "landare",
           "loc" = "loctn_d") %>%
    mutate(across(c(daid, loc), as.character)) # Explicitly declare data types on join columns
}, error = function(e) {
  message(glue("Error reading or processing file {fn}: {e$message}"))
  return(NULL) # Return NULL on error
})

# simplify geographies to 1 per locality
localities <- da_shapefile |> 
  group_by(loc) |> 
  summarize(geometry = st_union(geometry)) |> 
  ungroup() |> 
  mutate(
    locality = paste0(LOC_LIST[loc],' (',loc,')')
  )

# get centroids for labels
locality_centroids <- st_centroid(localities) 
locality_centroids_nudged <- locality_centroids |> 
mutate(
    geometry = case_when(
      loc == '227' ~ geometry + c(-150000, 40000),
      loc == '909' ~ geometry + c(130000, 10000), # move east for langford
      TRUE ~ geometry + c(0, 70000)  # move label north to not overlap 
      )
    )|>  
  st_set_crs(st_crs(locality_centroids))

# locations of all nearby SBC locations 
sbc_locs <- read_csv(glue("{SRC_DATA_FOLDER}/service_bc_locs.csv")) |>
  st_as_sf(
    coords = c('coord_x', 'coord_y'),
    crs = 3005
  )

#-----------------------------------------------------------------------------
# build map 
#-----------------------------------------------------------------------------

out <- ggplot() + 
  geom_sf(
    data = bc_map, 
    fill = 'white', 
    color='black'
    ) +
  geom_sf(
    data = localities, 
    aes(fill = as.factor(locality)),
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
    data = locality_centroids_nudged,
    aes(label = locality), 
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
    legend.position="none",
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
