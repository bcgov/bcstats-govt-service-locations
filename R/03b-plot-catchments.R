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

# ------------------------------------------------------------------------
# Script: 03b-plot-catchments.R

# Description: This script creates maps of the Service BC facility catchment 
# areas, including both drive time based assignments and spatial assignments.
# It reads the pre-computed assignment data rather than recalculating it.

# Requirements:
#   - Requires R packages: `tidyverse`, `glue`, `janitor`, `sf`, `rmapshaper`, `bcmaps`
#   - Depends on `settings.R` for paths and constants
#   - Requires the output from 03a-create-catchments.R
#   - Requires input DB shapefiles and location data

# Side Effects/Outputs:
#   - Creates maps of catchment areas, saved to the MAP_OUT directory
# ------------------------------------------------------------------------

library(tidyverse)
library(glue)
library(janitor)
library(sf)
library(rmapshaper)    # simplify geometries
library(bcmaps)        # get BC outline

source("R/settings.R")

# ----------------------------------------------------------------------------
# Load and prepare mapping data
# ----------------------------------------------------------------------------

# Map of BC
bc_map <- bc_bound() |>
  st_transform(crs = 3005)

# CSD level shapefiles for each locality
csd_shapefile <- 
  st_read(glue("{SHAPEFILE_OUT}/full-csd_with_location.gpkg")) %>%
  mutate(across(c(csd_name), as.character),
         across(c(landarea), as.numeric))

# Locations of all Service BC locations 
sbc_locs <- read_csv(glue("{SRC_DATA_FOLDER}/full-service-bc-locs.csv")) |>
  st_as_sf(
    coords = c('coord_x', 'coord_y'),
    crs = 3005
  ) 

# DB shapefiles
db_shapefile <-
  st_read(glue("{SHAPEFILE_OUT}/full-db_with_location.gpkg")) %>%
  mutate(across(c(landarea), as.numeric))

# Read the pre-computed assignment data 
complete_assignments <- read_csv(
  glue("{SRC_DATA_FOLDER}/complete_db_assignments.csv")
  ) %>%
  mutate(
    across(
      c(dbid, assigned, assignment_method), 
      as.character)
      )

# Get centroids for labels
csd_centroids <- st_centroid(csd_shapefile) 
csd_centroids_nudged <- csd_centroids |> 
  mutate(
      geom = case_when(
        csd_name == 'Dawson Creek' ~ geom + c(-170000, 50000),
        csd_name == 'Langford' ~ geom + c(140000, 15000), # move east for langford
        TRUE ~ geom + c(0, 70000)  # move label north to not overlap 
      )
  ) %>%
  st_set_crs(st_crs(csd_centroids))

# Filter to relevant datasets for the maps
csds <- csd_shapefile |>
  filter(csd_name %in% CSD_NAMES)

csd_centroids_nudged <- csd_centroids_nudged |>
  filter(csd_name %in% CSD_NAMES)

pilot_sbc_locs <- sbc_locs |>
  filter(csd_name %in% CSD_NAMES)

  # Save the maps to the outputs directory
maps_folder <- file.path(MAP_OUT, "complete_catchments")
if (!dir.exists(maps_folder)) {
  dir.create(maps_folder, recursive = TRUE)
}

# ----------------------------------------------------------------------------
# Map 1: Pilot regions overview
# ----------------------------------------------------------------------------

pilot_map <- ggplot() + 
  geom_sf(
    data = bc_map, 
    fill = 'white', 
    color = 'black'
  ) +
  geom_sf(
    data = csds, 
    aes(fill = as.factor(csd_name)),
    color = 'darkgrey',
    alpha = 1
  ) +
  geom_sf(
    data = pilot_sbc_locs,
    shape = 23,
    fill = 'yellow',
    color = 'black',
    size = 2,
    stroke = 1.1
  ) +
  geom_sf_text(
    data = csd_centroids_nudged,
    aes(label = csd_name), 
    size = 5, 
    fontface = 'bold'
  ) +
  scale_fill_brewer(
    palette = 'Set2',
    name = 'Locality'
  ) +
  coord_sf(expand = FALSE) + # remove whitespace
  labs(
    title = 'Pilot Regions\n'
  ) +
  MAP_THEME +
  theme(
    legend.position = "None",  
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis text
    axis.ticks = element_blank(),  # Remove axis ticks
    panel.grid = element_blank()   # Remove background grid lines
  )

pilot_map

# Save the pilot regions map
ggsave(
  filename = glue("pilot_regions.png"),
  path = maps_folder,
  plot = pilot_map,
  width = 8,
  height = 7,
  dpi = 300
)

# ----------------------------------------------------------------------------
# Map 2: Original drive-time based catchments (with unassigned areas)
# ----------------------------------------------------------------------------

# Create original catchment areas (with unassigned areas)
original_catchments <- db_shapefile |>
  left_join(complete_assignments, by = "dbid") |>
  filter(assignment_method == 'drive_time') |>  
  # Keep only facilities in our CSDs of interest
  filter(assigned %in% pilot_sbc_locs$nearest_facility) |>  
  # Group by facility and merge geometries
  group_by(assigned) |>
  summarize(
    n_dbs = n(),
    assignment_method = "drive_time",
    .groups = "drop"
  ) |>
  # Simplify the geometries for better mapping performance
  ms_simplify(keep = 0.05, keep_shapes = TRUE)

# Create "unassigned" areas
unassigned_dbs <- db_shapefile |>
  left_join(complete_assignments, by = "dbid") |>
  filter(assignment_method == 'nearest_facility') |>
  # Keep only facilities in our CSDs of interest
  filter(assigned %in% pilot_sbc_locs$nearest_facility) |>  
  # Group and merge geometries
  summarize(
    n_dbs = n(),
    assigned = "Unassigned", # Label for these areas
    assignment_method = "none",
    .groups = "drop"
  ) |>
  # Simplify the geometries
  rmapshaper::ms_simplify(keep = 0.05, keep_shapes = TRUE)

# Build map with original catchments
original_map <- ggplot() + 
  # Base BC map
  geom_sf(
    data = bc_map, 
    fill = 'white', 
    color = 'black'
  ) +
  # Unassigned areas
  geom_sf(
    data = unassigned_dbs,
    fill = 'darkgray',
    color = 'darkgrey',
    alpha = 0.5
  ) +
  # Facility catchment areas
  geom_sf(
    data = original_catchments, 
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
    data = pilot_sbc_locs,
    shape = 23,
    fill = 'yellow',
    color = 'black',
    size = 2,
    stroke = 1
  ) +
  geom_sf_text(
    data = csd_centroids_nudged,
    aes(label = csd_name), 
    size = 4, 
    fontface = 'bold'
  ) +
  # Colors for catchment areas
  scale_fill_viridis_d(
    option = "turbo",
    name = 'Service BC Facility',
    na.value = "darkgray"
  ) +
  coord_sf(expand = FALSE) + # remove whitespace
  labs(
    title = 'Service BC Facility Catchment Areas - Drive Time Method',
    subtitle = 'Areas showing which Service BC facility is closest based on drive time\nDark gray areas are not assigned to any facility (no drive data available)'
  ) +
  MAP_THEME +
  theme(legend.position = "none")  # Remove legends

original_map
# Save the original catchments map
ggsave(
  filename = glue("pilot_original_catchments.png"),
  path = maps_folder,
  plot = original_map,
  width = 12,
  height = 10,
  dpi = 300
)

# ----------------------------------------------------------------------------
# Map 3: Complete catchments (with spatial assignments)
# ----------------------------------------------------------------------------

# Create complete catchment areas (with no unassigned areas)
complete_catchments <- db_shapefile |>
  left_join(complete_assignments, by = "dbid") |>
  # Group by facility and merge geometries
  group_by(assigned, assignment_method) |>
  # Keep only facilities in our CSDs of interest
  filter(assigned %in% pilot_sbc_locs$nearest_facility) |>  
  summarize(
    n_dbs = n(),
    .groups = "drop"
  ) |>
  # Simplify the geometries for better mapping performance
  ms_simplify(keep = 0.05, keep_shapes = TRUE)

# Map with complete catchments
complete_map <- ggplot() + 
  # Base BC map
  geom_sf(
    data = bc_map, 
    fill = 'white', 
    color = 'black'
  ) +
  # All catchment areas (drive time + spatial assignments)
  geom_sf(
    data = complete_catchments, 
    aes(fill = assigned, alpha = assignment_method),
    color = 'darkgrey'
  ) +
  # CSDs of interest as outlined areas
  geom_sf(
    data = csd_shapefile, 
    fill = NA, 
    color = "darkgrey", 
    size = 0.2, 
    alpha = 0.5
  ) +
  # Service BC locations
  geom_sf(
    data = pilot_sbc_locs,
    shape = 23,
    fill = 'yellow',
    color = 'black',
    size = 2,
    stroke = 1
  ) +
  geom_sf_text(
    data = csd_centroids_nudged,
    aes(label = csd_name), 
    size = 4, 
    fontface = 'bold'
  ) +
  # Colors for catchment areas
  scale_fill_viridis_d(
    option = "turbo",
    name = 'Service BC Facility',
    na.value = NA#"whitesmoke"
  ) +
  # Alpha for assignment method
  scale_alpha_manual(
    values = c("drive_time" = 0.9, "nearest_facility" = 0.4),
    name = "Assignment Method",
    labels = c("Drive Time", "Direct Assignment")
  ) +
  coord_sf(expand = FALSE) + # remove whitespace
  labs(
    title = "Service BC Catchment Areas: Address Method",
    subtitle = "With CSD Boundaries and Service BC Office Locations"
  ) +
  MAP_THEME +
  theme(
    legend.position = "None",  
    axis.title = element_blank(),  # Remove axis titles
    axis.text = element_blank(),   # Remove axis text
    axis.ticks = element_blank(),  # Remove axis ticks
    panel.grid = element_blank()   # Remove background grid lines
  )
complete_map
# Save the complete catchments map
ggsave(
  filename = glue("pilot_complete_catchments.png"),
  path = maps_folder,
  plot = complete_map,
  width = 10,
  height = 8,
  dpi = 300
)

# ----------------------------------------------------------------------------
# Map 4: Provincial view of all catchments (without unassigned areas)
# ----------------------------------------------------------------------------

# Create original catchment areas (with unassigned areas)
province_original_catchments <- db_shapefile |>
  left_join(complete_assignments, by = "dbid") |>
  filter(assignment_method == 'drive_time') |>  
  # Group by facility and merge geometries
  group_by(assigned) |>
  summarize(
    n_dbs = n(),
    assignment_method = "drive_time",
    .groups = "drop"
  ) |>
  # Simplify the geometries for better mapping performance
  ms_simplify(keep = 0.01, keep_shapes = TRUE)

# Create "unassigned" areas
unassigned_dbs <- db_shapefile |>
  left_join(complete_assignments, by = "dbid") |>
  filter(assignment_method == 'nearest_facility') |>
  # Group and merge geometries
  summarize(
    n_dbs = n(),
    assigned = "Unassigned", # Label for these areas
    assignment_method = "none",
    .groups = "drop"
  ) |>
  # Simplify the geometries
  rmapshaper::ms_simplify(keep = 0.01, keep_shapes = TRUE)

# Build map with original catchments
province_original_map <- ggplot() + 
  # Base BC map
  geom_sf(
    data = bc_map, 
    fill = 'white', 
    color = 'black'
  ) +
  # Unassigned areas
  geom_sf(
    data = unassigned_dbs,
    fill = 'darkgray',
    color = 'darkgrey',
    alpha = 0.5
  ) +
  # Facility catchment areas
  geom_sf(
    data = province_original_catchments, 
    aes(fill = assigned),
    color = 'darkgrey',
    alpha = 0.7
  ) +
  # Service BC locations
  geom_sf(
    data = sbc_locs,
    shape = 23,
    fill = 'yellow',
    color = 'black',
    size = 2,
    stroke = 1
  ) +
  # Colors for catchment areas
  scale_fill_viridis_d(
    option = "turbo",
    name = 'Service BC Facility',
    na.value = "darkgray"
  ) +
  coord_sf(expand = FALSE) + # remove whitespace
  labs(
    title = 'Service BC Facility Catchment Areas - Drive Time Method',
    subtitle = 'Areas showing which Service BC facility is closest based on drive time\nDark gray areas are not assigned to any facility (no drive data available)'
  ) +
  MAP_THEME +
  theme(legend.position = "none")  # Remove legends

province_original_map
# Save the original catchments map
ggsave(
  filename = glue("province_original_catchments.png"),
  path = maps_folder,
  plot = province_original_map,
  width = 12,
  height = 10,
  dpi = 300
)

# ----------------------------------------------------------------------------
# Map 5: Provincial view of all catchments (with unassigned areas)
# ----------------------------------------------------------------------------

# Create complete catchment areas (with no unassigned areas)
province_complete_catchments <- db_shapefile |>
  left_join(complete_assignments, by = "dbid") |>
  # Group by facility and merge geometries
  group_by(assigned, assignment_method) |>
  summarize(
    n_dbs = n(),
    .groups = "drop"
  ) |>
  # Simplify the geometries for better mapping performance
  ms_simplify(keep = 0.05, keep_shapes = TRUE)

# Map with complete catchments
province_complete_map <- ggplot() + 
  # Base BC map
  geom_sf(
    data = bc_map, 
    fill = 'white', 
    color = 'black'
  ) +
  # All catchment areas (drive time + spatial assignments)
  geom_sf(
    data = province_complete_catchments, 
    aes(fill = assigned),
    color = 'darkgrey',
    alpha = 0.7
  ) +
  # Service BC locations
  geom_sf(
    data = sbc_locs,
    shape = 23,
    fill = 'yellow',
    color = 'black',
    size = 2,
    stroke = 1
  ) +
  # Colors for catchment areas
  scale_fill_viridis_d(
    option = "turbo",
    name = 'Service BC Facility',
    na.value = "darkgray"
  ) +
  coord_sf(expand = FALSE) + # remove whitespace
  labs(
    title = 'Service BC Facility Complete Catchment Areas',
    subtitle = 'Originally unassigned areas assigned based on proximity to facilities'
  ) +
  MAP_THEME +
  theme(legend.position = "none")  # Remove legends

province_complete_map

# Save the complete catchments map
ggsave(
  filename = glue("province_complete_catchments.png"),
  path = maps_folder,
  plot = province_complete_map,
  width = 12,
  height = 10,
  dpi = 300
)
