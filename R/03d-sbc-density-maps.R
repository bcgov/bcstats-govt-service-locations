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
# Script: 03d-sbc-density-maps.R

# Description: Creates spatial density maps showing drive times to the 
# nearest Service BC office for different Service BC catchment regions.
# Uses kernel density estimation to create smoothed heatmaps of drive times.
# This script is adapted from 02d-density_map.R but uses catchment regions
# instead of census subdivisions.

# Requirements:
#   - Requires R packages: `tidyverse`, `glue`, `janitor`, `sf`, `tigris`, 
#     `spatstat`, `stars`, `terra`, `fs`, `snakecase`
#   - Depends on `settings.R` for paths and constants.
#   - Requires input CSV data files with drive times, population data,
#     Service BC location data, and complete DB assignments from 03a-create-catchments.R.
#   - Requires read/write access to the map output folder.

# Side Effects/Outputs:
#   - Creates PNG maps in the "{MAP_OUT}/sbc-catchment-drive-distance-maps" directory
#   - Each map shows the spatial distribution of drive times to a Service BC office
#     for that facility's catchment area
# ------------------------------------------------------------------------

# =========================================================================== #
# Load libraries and settings ----
# =========================================================================== #

source("R/settings.R")

output_subdir <- "sbc-catchment-drive-distance-maps"  # Subdirectory to save maps

# Ensure output directory exists
output_path <- glue("{MAP_OUT}/{output_subdir}")
if (!dir_exists(output_path)) {
  dir_create(output_path, recurse = TRUE)
  message("Created output directory: ", output_path)
}

# -----------------------------------------------------------------------------------------------------
# Read required data
# -----------------------------------------------------------------------------------------------------

# --- Population data for DB's containing columns for area, population, dwellings, and households
pop_db <- read_csv(glue("{SRC_DATA_FOLDER}/population-db.csv"), col_types = cols(.default = "c")) %>%
  clean_names() %>%
  mutate(across(c(area_sq_km, population, dwellings, households), as.numeric)) %>%
  mutate(people_per_household = population / dwellings) %>%
  select(-c(region_name, dwellings, households, area_sq_km, population))

# --- Drive time data containing columns for address coordinates (address_albers_x, address_albers_y)
drivetime_data <-
  read_csv(glue("{SRC_DATA_FOLDER}/full-processed-drivetime-data.csv"), col_types = cols(.default = "c")) %>%
  clean_names() %>%
  mutate(across(c(drv_time_sec, drv_dist), as.numeric)) %>% 
  mutate(drv_time_min = drv_time_sec / 60) %>% # Calculate drive times in minutes for plotting
  st_as_sf(coords = c("address_albers_x", "address_albers_y"), remove = TRUE, crs = 3005)

# add population information to the drive time data
drivetime_data <- drivetime_data %>%
  left_join(pop_db, by = join_by(dbid))

# --- Service BC location data
servicebc <-
  read_csv(glue("{SRC_DATA_FOLDER}/full-service-bc-locs.csv"), col_types = cols(.default = "c")) %>%
  clean_names() %>%
  st_as_sf(coords = c("coord_x", "coord_y"), remove = TRUE, crs = 3005)

# --- DB shapefiles
db_shapefile <-
  st_read(glue("{SHAPEFILE_OUT}/full-db_with_location.gpkg")) %>%
  mutate(across(c(landarea), as.numeric))

# --- CSD shapefiles
csd_shapefile <- 
  st_read(glue("{SHAPEFILE_OUT}/full-csd_with_location.gpkg")) %>%
  clean_names() %>%
  mutate(across(c(landarea), as.numeric))

# --- Read the complete assignments data (catchment information)
complete_assignments <- read_csv(
  glue("{SRC_DATA_FOLDER}/complete_db_assignments.csv")
  ) %>%
  mutate(
    across(
      c(dbid, assigned, assignment_method), 
      as.character)
      )

# Join drive time data with catchment assignments
drivetime_data <- drivetime_data %>%
  left_join(complete_assignments, by = "dbid")

# -----------------------------------------------------------------------------------------------------
# Configure map settings common to all catchments
# -----------------------------------------------------------------------------------------------------

# --- User-defined settings for plots ---
plotvar <- "drv_dist"
map_title <- "Spatial Distribution of Drive Distances"
fill_label <- "Drive Distance (km)"
common_scale <- FALSE    # Whether to use a common scale for all maps

# --- Set limits prior to subsetting points if using common scale
fill_theme <- FILL_THEME$clone()
if (common_scale == TRUE){
  fill_theme$limits <- range(drivetime_data[[plotvar]], na.rm = TRUE)
  fill_theme$oob <- scales::squish
}

# -----------------------------------------------------------------------------------------------------
# Create individual maps for each catchment
# -----------------------------------------------------------------------------------------------------

# Filter CSDs to just those in the pilot areas
pilot_csds <- csd_shapefile
if(exists("CSD_NAMES")) {
  pilot_csds <- csd_shapefile %>%
    filter(csd_name %in% CSD_NAMES)
}

# --- Loop over each CSD of interest and create maps for their facilities
for (csd_name in unique(pilot_csds$csd_name)) {
  
  # Get the CSD shape
  csd_shape <- pilot_csds %>%
    filter(csd_name == !!csd_name)
  
  # Find the facility in this CSD
  csd_facility <- servicebc %>%
    filter(csd_name == !!csd_name)
  
  # Skip if no facility found for this CSD
  if (nrow(csd_facility) == 0) {
    warning(glue("No Service BC facility found for {csd_name}. Skipping this CSD."))
    next
  }
  
  # Get the facility ID for this CSD
  facility_id <- csd_facility$nearest_facility[1]
  
  # Get the Service BC location for this facility
  facility_location <- servicebc %>%
    filter(nearest_facility == facility_id)
  
  # Skip if we couldn't find this facility
  if (nrow(facility_location) == 0) {
    warning(glue("Could not find facility information for {facility_id}. Skipping this map."))
    next
  }
  
  # Get the catchment boundary for this facility
  catchment_dbs <- complete_assignments %>%
    filter(assigned == facility_id) 
    
  # Skip if we couldn't find this catchment
  if (nrow(catchment_dbs) == 0) {
    warning(glue("No DBs found in catchment for {facility_id}. Skipping this map."))
    next
  }
    # Join with DB shapefile to get geometries
  catchment_shape <- db_shapefile %>%
    inner_join(catchment_dbs, by = "dbid") %>%
    # Dissolve to create a single polygon representing the catchment
    group_by(assigned) %>%
    summarize(
      n_dbs = n(),
      .groups = "drop"
    )
    
  # Identify DBs with no drive data (those assigned by proximity)
  no_drive_data_dbs <- db_shapefile %>%
    inner_join(
      catchment_dbs %>% 
        filter(assignment_method == "nearest_facility"),
      by = "dbid"
    )
    # Get the points for this catchment
  points <- drivetime_data %>%
    filter(assigned == facility_id) %>% 
    filter(assigned == nearest_facility)
  
  # Ensure all points fall within the catchment shape boundary (spatial filter)
  points <- points %>%
    st_filter(catchment_shape, .predicate = st_within)
  
  # Check if there are any points in this catchment
  if (nrow(points) == 0) {
    warning(glue("No points found in catchment for {facility_id}. Skipping this map."))
    next
  }
  message(glue("Generating map for {facility_id} catchment in {csd_name}..."))

  # Convert to ppp object with weights
  # Ignore warnings about duplicate points - these are likely due to multi-unit housing
  stats_ppp <- suppressWarnings(as.ppp(points$geometry, W = as.owin(catchment_shape)))
  marks(stats_ppp) <- points[[plotvar]]
  
  # Use tryCatch to handle potential errors in smoothing
  smooth_stats_stars <- tryCatch({
    stars::st_as_stars(Smooth(stats_ppp, sigma = 1000, dimyx = 300))
  }, error = function(e) {
    warning(glue("Error generating spatial smooth map for {facility_id}: {e$message}"))
    return(NULL)
  })

  # Skip to next iteration if density calculation failed
  if (is.null(smooth_stats_stars)) next

  # Convert back to sf so it's compatible with ggplot2::geom_sf()
  smooth_stats_sf <- st_as_sf(smooth_stats_stars) %>%
    st_set_crs(3005) 

  # Build map
  map_plot <- ggplot() +
    # Smoothed density surface
    geom_sf(data = smooth_stats_sf, aes(fill = v), color = NA) +
    # set label of gradient 
    scale_fill_viridis_c(option = "mako", alpha = 0.6, name = fill_label) +
    # change the fill scale after applying the gradient 
    ggnewscale::new_scale_fill() +
    # Areas with no drive data (red shading)
    geom_sf(data = no_drive_data_dbs, aes(fill = "No Drive Data Available"), color = NA, alpha = 0.5) +
    scale_fill_manual(
      name = NULL,
      values = c("No Drive Data Available" = "red")
    ) +
    # Catchment boundary
    geom_sf(data = catchment_shape, fill = NA, color = "grey70", linewidth = 0.8) +
    # Points (addresses)
    geom_sf(data = points, size = 0.15, color = "grey40", alpha = 0.5) +
    # Original CSD boundary
    geom_sf(data = csd_shape, fill = NA, color = "black", linewidth = 0.8) +
    # Service BC location
    geom_sf(data = facility_location, aes(shape = "Service BC Location"),
            fill = 'yellow', color = 'black', size = 2, stroke = 1.1) +
    coord_sf(crs = 3005) + 
    scale_shape_manual(
      name = NULL,
      values = c("Service BC Location" = 23) 
    ) +    
    MAP_THEME +
    labs(
      title = map_title,
      subtitle = glue("Catchment for {facility_id}.\nAssociated pilot region {csd_name} outlined in black."),
      fill = fill_label,
      x = element_blank(), # "\nLongitude",
      y = element_blank() # "Latitude\n"
    ) +
    theme(
      plot.title.position = "plot"
    ) +
    guides(
      shape = guide_legend(
        override.aes = list(
          fill = "yellow", 
          size = 4)
      )    )
  
  # Save the plot
  # For Kamloops and Dawson Creek set width = 18, height = 12
  fn <- to_snake_case(glue("catchment-{plotvar}-commonscale={common_scale}-{csd_name}-{facility_id}"))
  ggsave(
    filename = glue("{fn}.png"),
    path = output_path,
    plot = map_plot,
    width = 12,
    height = 7,
    device = "png"
  )
}

