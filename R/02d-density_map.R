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
# municipality of interest in BC (loc). It produces maps at the dissemination block level 
# displaying quantitative information on basic descriptive statisics

# ------------------------------------------------------------------------
# Script: 02d-density_map.R

# Description: Creates spatial density maps showing drive times to the 
# nearest Service BC office for different census subdivisions in British Columbia.
# Uses kernel density estimation to create smoothed heatmaps of drive times.

# Requirements:
#   - Requires R packages: `tidyverse`, `glue`, `janitor`, `sf`, `tigris`, 
#     `spatstat`, `stars`, `bcmaps`, `terra`, `fs`, `snakecase`
#   - Depends on `settings.R` for paths and constants.
#   - Requires input CSV data files with drive times, population data,
#     Service BC location data, and census subdivision shapefiles.
#   - Requires read/write access to the map output folder.

# Side Effects/Outputs:
#   - Creates SVG maps in the "{MAP_OUT}/csd-drive-distance-maps/temp" directory
#   - Each map shows the spatial distribution of drive times to the nearest
#     Service BC office for a specific census subdivision
# ------------------------------------------------------------------------

library(tidyverse)
library(glue)
library(janitor)
library(sf)
library(tigris)
library(spatstat)
library(stars)
library(bcmaps)
library(terra)
library(fs)      # For directory operations
library(snakecase) # For to_snake_case function

source("R/settings.R")

output_subdir <- "no-common-scale"  # Subdirectory to save maps

# Ensure output directory exists
output_path <- glue("{MAP_OUT}/csd-drive-distance-maps/{output_subdir}")
if (!dir_exists(output_path)) {
  dir_create(output_path, recurse = TRUE)
  message("Created output directory: ", output_path)
}

# -----------------------------------------------------------------------------------------------------
# Read data points into one data frame
# -----------------------------------------------------------------------------------------------------
crosswalk <-
  read_csv(glue("{SRC_DATA_FOLDER}/csd-da-db-loc-crosswalk.csv"), col_types = cols(.default = "c")) %>%
  clean_names()

drivetime_data <-
  read_csv(glue("{SRC_DATA_FOLDER}/reduced-drivetime-data.csv"), col_types = cols(.default = "c")) %>%
  clean_names() %>%
  mutate(across(c(drv_time_sec, drv_dist), as.numeric))

drivetime_data <- drivetime_data %>%
  inner_join(crosswalk, by = c("dbid", "daid", "csdid", "csd_name", "csd_desc"))

pop_db <- read_csv(glue("{SRC_DATA_FOLDER}/population-db.csv"), col_types = cols(.default = "c")) %>%
  clean_names() %>%
  mutate(across(c(area_sq_km, population, dwellings, households), as.numeric))

#------------------------------------------------------------------------------
# Read service bc location data from source folder
#------------------------------------------------------------------------------
servicebc <- read_csv(glue("{SRC_DATA_FOLDER}/reduced-service_bc_locs.csv")
           , col_types = cols(.default = "c")) %>%
  clean_names() %>%
  st_as_sf(coords = c("coord_x", "coord_y"), crs = 3005)

# -----------------------------------------------------------------------------------------------------
# read csd shapefiles
# -----------------------------------------------------------------------------------------------------

shp_csd_all <- census_subdivision() %>%
  select(4) %>%
  filter(CENSUS_SUBDIVISION_NAME %in% (servicebc %>% pull(csd_name))) %>%
  clean_names()

# Check if we have any matching CSDs
if (nrow(shp_csd_all) == 0) {
  stop("No matching census subdivisions found. Check the 'csd_name' values in the Service BC locations data.")
}

# -----------------------------------------------------------------------------------------------------
# make map data
# -----------------------------------------------------------------------------------------------------

# --- User-defined settings for plots ---
map_title <- "Spatial Distribution of Drive Times"
subtitle_pref <- "Estimated Drive Times to Nearest Service BC Office"
fill_label <- "Drive time (Minutes)"
common_scale <- FALSE    # Whether to use a common scale for all maps


# Calculate drive times in minutes for plotting
drivetime_data <- drivetime_data %>% 
  mutate(plotvar = drv_time_sec / 60)

# Set limits prior to subsetting points if using common scale
fill_theme <- FILL_THEME$clone()
if (common_scale == TRUE){
  fill_theme$limits <- range(drivetime_data$plotvar, na.rm = TRUE)
  fill_theme$oob <- scales::squish
}

for (csd in shp_csd_all %>% pull(census_subdivision_name)){

  message(glue("Generating map for {csd} ..."))

  shp_csd <- shp_csd_all %>% filter(census_subdivision_name == csd)

  points <- drivetime_data %>% 
    st_as_sf(coords = c("address_albers_x", "address_albers_y"), crs = 3005) %>%
    st_intersection(shp_csd)

  # Check if there are any points in this CSD
  if (nrow(points) == 0) {
    warning(glue("No points found in census subdivision {csd}. Skipping this map."))
    next
  }

  # Convert to ppp object with weights
  stats_ppp <- as.ppp(points$geometry, W = as.owin(shp_csd))
  marks(stats_ppp) <- points$plotvar

  # Use tryCatch to handle potential errors in smoothing
  smooth_stats_stars <- tryCatch({
    stars::st_as_stars(Smooth(stats_ppp, sigma = 1000, dimyx =300))
  }, error = function(e) {
    warning(glue("Error generating spatial smooth map for {csd}: {e$message}"))
    return(NULL)
  })

  # Skip to next iteration if density calculation failed
  if (is.null(smooth_stats_stars)) next

  # Convert back to sf so it's compatible with ggplot2::geom_sf()
  smooth_stats_sf <- st_as_sf(smooth_stats_stars) %>%
    st_set_crs(3005)

  map_plot <- ggplot() +
    geom_sf(data = smooth_stats_sf, aes(fill = v), color = NA) +
    geom_sf(data = shp_csd, fill = NA, color = "grey70", linewidth = 1) +
    geom_sf(data = points, size = 0.25, color = "grey40", alpha = 0.5) +
    coord_sf(crs = 3005) +
    fill_theme +
    MAP_THEME +
    labs(
      title = map_title,
      subtitle = glue("{subtitle_pref} - {csd} (Smoothed Data)"),
      fill = fill_label,
      x = "\nLongitude",
      y = "Latitude\n"
    )

  # Save the plot
  fn <- to_snake_case(glue("drv_time_min-smoothed-{csd}"))

  ggsave(
    filename = glue("{fn}.svg"),
    path = output_path,
    plot = map_plot,
    width = 8,
    height = 7,
    device = "svg"
  )
  
  message(glue("Map for {csd} saved to {output_path}/{fn}.svg"))
}

