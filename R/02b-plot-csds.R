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
  st_read(glue("{SHAPEFILE_OUT}/full-csd_with_location.gpkg")) %>%
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
sbc_locs <- read_csv(glue("{SRC_DATA_FOLDER}/full-service-bc-locs.csv")) |>
  st_as_sf(
    coords = c('coord_x', 'coord_y'),
    crs = 3005
  ) 

# drive time data - required for 'SBC catchments'
drivetime_data <-
  read_csv(
    glue("{SRC_DATA_FOLDER}/full-processed-drivetime-data.csv"),
    col_types = cols(.default = "c")
  ) %>%
  clean_names() %>%
  mutate(across(c(drv_time_sec, drv_dist), as.numeric))

## db shapefiles
db_shapefile <-
  st_read(glue("{SHAPEFILE_OUT}/full-db_with_location.gpkg")) %>%
  mutate(across(c(landarea), as.numeric))

# db populations (for checking empty DBs)
pop_db <- read_csv(
  glue("{SRC_DATA_FOLDER}/population-db.csv"),
  col_types = cols(.default = "c")
) %>%
  clean_names() %>%
  mutate(across(c(area_sq_km, population, dwellings, households), as.numeric))


#-----------------------------------------------------------------------------
# build map 1: - CSDs of interest only
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

#-----------------------------------------------------------------------------
# build map.2: - CSDs of interest as a layer + a layer for 'catchments'
#-----------------------------------------------------------------------------

# first assign every dissemination block a 'nearest facility'
assigned_facility <- drivetime_data |>
  count(dbid, nearest_facility) |>
  arrange(dbid, desc(n)) |>
  group_by(dbid) |>
  slice_head(n = 1) |>
  rename(assigned = nearest_facility) |>
  select(-n) |>
  ungroup()

# Create catchment areas by merging DBs assigned to the same facility
catchment_map_data <- db_shapefile |>
  left_join(assigned_facility, by = "dbid") |>
  filter(!is.na(assigned)) |>  # Remove DBs without facility assignment
  filter(assigned %in% sbc_locs$nearest_facility) |>  # Keep only facilities in our CSDs of interest
  # Group by facility and merge geometries
  group_by(assigned) |>
  summarize(
    n_dbs = n(),
    .groups = "drop"
  ) |>
  # Simplify the geometries for better mapping performance
  ms_simplify(keep = 0.05, keep_shapes = TRUE)

# Build map with facility catchments
catchment_plot <- ggplot() + 
  # Base BC map
  geom_sf(
    data = bc_map, 
    fill = 'white', 
    color = 'black'
  ) +
  # Facility catchment areas
  geom_sf(
    data = catchment_map_data, 
    aes(fill = assigned),
    color = 'darkgrey',
    alpha = 0.7
  ) +
  # CSDs of interest as outlined areas
  geom_sf(
    data = csds, 
    fill = NA, 
    color = 'black',
    linewidth = 1.2,
    alpha = 0.9
  ) +
  # Service BC locations
  geom_sf(
    data = sbc_locs,
    shape = 23,
    fill = 'yellow',
    color = 'black',
    size = 1.5,
    stroke = 1
  ) +
  # CSD labels
  geom_sf_text(
    data = csd_centroids_nudged,
    aes(label = csd_name), 
    size = 4, 
    fontface = 'bold'
  ) +
  # Colors for catchment areas
  scale_fill_brewer(
    palette = 'Set3',
    name = 'Service BC Facility'
  ) +
  coord_sf(expand = FALSE) + # remove whitespace
  labs(
    title = 'Service BC Facility Catchment Areas',
    subtitle = 'Areas showing which Service BC facility is closest to each location'
  ) +
  MAP_THEME 

# Display catchment map
catchment_plot

# Save catchment map
ggsave(
  filename = glue("service_bc_catchments_for_pilot_regions.png"),
  path = MAP_OUT,
  plot = catchment_plot,
  width = 10,
  height = 8,
  dpi = 300
)

#-----------------------------------------------------------------------------
# build map 3: Provincial view of all Service BC catchment areas
#-----------------------------------------------------------------------------

# Get all Service BC locations across the province (not just pilot regions)
all_sbc_locs <- read_csv(glue("{SRC_DATA_FOLDER}/full-service-bc-locs.csv")) |>
  st_as_sf(
    coords = c('coord_x', 'coord_y'),
    crs = 3005
  )

# Create catchment areas for all facilities across the province
# First identify DBs without facility assignments
unassigned_dbs <- db_shapefile |>
  left_join(assigned_facility, by = "dbid") |>
  filter(is.na(assigned)) |>  # Keep only DBs without facility assignment
  # Group and merge geometries
  summarize(
    n_dbs = n(),
    assigned = "Unassigned", # Label for these areas
    .groups = "drop"
  ) |>
  # Simplify the geometries
  rmapshaper::ms_simplify(keep = 0.01, keep_shapes = TRUE)

# Create catchments for assigned DBs
provincial_catchments <- db_shapefile |>
  left_join(assigned_facility, by = "dbid") |>
  filter(!is.na(assigned)) |>  # Remove DBs without facility assignment
  # Group by facility and merge geometries
  group_by(assigned) |>
  summarize(
    n_dbs = n(),
    .groups = "drop"
  )

# Simplify the geometries for better mapping performance
# Use a higher keep value for provincial view to maintain important features
simplified_catchments <- provincial_catchments |>
  rmapshaper::ms_simplify(keep = 0.01, keep_shapes = TRUE)

# Create a provincial map showing all Service BC catchment areas
provincial_plot <- ggplot() + 
  # Base BC map
  geom_sf(
    data = bc_map, 
    fill = 'white', 
    color = 'black',
    size = 0.2
  ) +
  # Unassigned areas
  geom_sf(
    data = unassigned_dbs,
    fill = 'darkgray',
    color = 'darkgrey',
    alpha = 0.5,
    size = 0.1
  ) +
  # All facility catchment areas
  geom_sf(
    data = simplified_catchments, 
    aes(fill = assigned),
    color = 'darkgrey',
    alpha = 0.7,
    size = 0.1
  ) +
  # All Service BC locations
  geom_sf(
    data = all_sbc_locs,
    shape = 23,
    fill = 'yellow',
    color = 'black',
    size = 2,
    stroke = 0.8
  ) +
  # Colors for catchment areas - use a color palette that works with many categories
  scale_fill_viridis_d(
    option = "turbo",
    name = 'Service BC Facility'
  ) +
  coord_sf(expand = FALSE) + # remove whitespace
  labs(
    title = 'Service BC Facility Catchment Areas - Provincial View',
    subtitle = 'Areas showing which Service BC facility is closest to each location across BC\nDark gray areas are not assigned to any facility (no drive data available)'
  ) +
  MAP_THEME +
  theme(
    legend.position = "none",  # Remove legend as there are too many facilities
    axis.title = element_blank()
  )

# Display provincial catchment map
print(provincial_plot)

# Save provincial catchment map
ggsave(
  filename = glue("service_bc_catchments_provincial.png"),
  path = MAP_OUT,
  plot = provincial_plot,
  width = 12,
  height = 10,
  dpi = 300
)

# save the list of unassigned dbs, together with their 2021 census pops
db_check <- db_shapefile %>% 
  left_join(assigned_facility, by = "dbid") %>% 
  left_join(pop_db, by='dbid') %>% 
  mutate(has_assignee = if_else(is.na(assigned), "No", "Yes"))  %>% 
  select(dbid, has_assignee, assigned, population) %>% 
  st_drop_geometry()  %>% 
  tibble()

# look at populations of unassigned dbs
db_check  %>% 
  group_by(has_assignee) %>% 
  summarize(
    n = n_distinct(dbid),
    average_pop = mean(population, na.rm = TRUE),
    min_pop = min(population, na.rm = TRUE),
    max_pop = max(population, na.rm = TRUE),
    median_pop = median(population, na.rm = TRUE),
    pct_zero_pop = sum(population == 0, na.rm = TRUE) / n(),
    pct_na_pop = sum(is.na(population)) / n(),
    .groups = "drop"
    )

# save the list of dbs that do/don't have assignees for further investigation
db_check  %>% 
  filter(has_assignee == "No") %>%
  select(-has_assignee, -assigned) %>%
  write_csv(glue("{TABLES_OUT}/unassigned_dbs.csv"))
