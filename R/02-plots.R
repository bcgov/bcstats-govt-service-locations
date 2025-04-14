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
# Script: 02-plots.R

# Description: Loads previously processed DA and DB shapefiles (containing location IDs)
# and corresponding processed drive time summary statistics CSV files. Prepares the final 
# spatial data frames ready for mapping.  A sample map visualization is generated and saved to disk.

# Requirements:
#   - Requires R packages: `tidyverse`, `glue`, `janitor`, `sf`.
#   - Depends on `settings.R` for configuration constants (paths, `MAP_THEME`).
#   - Requires shapefiles in `SHAPEFILE_DIR`.
#   - Requires drive time summary CSV files in `SRC_DATA_FOLDER` containing variables (numeric type) to map
#   - Requires the `build_map` function to be defined and available.
#   - Requires read access to input files/paths.

# Side Effects/Outputs:
#   - Calls the `build_map` function, which prints a ggplot map object to
#     the default graphics device.
#   - Prints errors and stops execution if input files are missing.
#   - (Currently does not write any new files to disk unless `build_map` is modified to do so).
# ------------------------------------------------------------------------


library(tidyverse)
library(glue)
library(janitor)
library(sf)

source("R/settings.R")
source("R/fxns/plots.R")

#------------------------------------------------------------------------------
# Read shape file data from source folder
# Note: saving shapeiles truncates the column names, reassignment needed
#------------------------------------------------------------------------------

fn <- glue("{SHAPEFILE_OUT}/processed_da_with_location.shp")
da_shapefile <- tryCatch({
  st_read(fn) %>%
  rename("landarea" = "landare",
         "loc" = "loctn_d") %>%
  mutate(across(c(daid, loc), as.character)) # Explictly declare data types on join columns
}, error = function(e) {
   message(glue("Error reading or processing file {fn}: {e$message}"))
   return(NULL) # Return NULL on error
})

if (is.null(da_shapefile)) {
  stop("Failed to load or process DA shapefile. Stopping script.")
}

fn <- glue("{SHAPEFILE_OUT}/processed_db_with_location.shp")
db_shapefile <- tryCatch({
  st_read(fn) %>%
  rename("dissemination_block_id" = "dssmn__",
         "landarea" = "landare",
         "loc" = "loctn_d") %>% 
  mutate(across(c(dissemination_block_id, loc), as.character)) # Explictly declare data types on join columns
}, error = function(e) {
   message(glue("Error reading or processing file {fn}: {e$message}"))
   return(NULL) # Return NULL on error
})

if (is.null(db_shapefile)) {
  stop("Failed to load or process DB shapefile. Stopping script.")
}

#------------------------------------------------------------------------------
# Read drive time data from source folder and
# keep all shapes and potentially colour the missing ones differently
#------------------------------------------------------------------------------
fn <- glue("{SRC_DATA_FOLDER}/da_average_times_dist_all_locs.csv")
da_drivetime_data <- tryCatch({
  read_csv(fn) %>%
  clean_names() %>%
  mutate(across(c(daid, loc), as.character)) # Explictly declare data types on join columns
}, error = function(e) {
   message(glue("Error reading or processing file {fn}: {e$message}"))
   return(NULL) # Return NULL on error
})

if (is.null(da_drivetime_data)) {
  stop("Failed to load or process DA drivetime data. Stopping script.")
}

fn <- glue("{SRC_DATA_FOLDER}/db_average_times_dist_all_locs.csv")
db_drivetime_data <-  tryCatch({
  read_csv(fn) %>%
  clean_names() %>%
  mutate(across(c(dissemination_block_id, loc), as.character)) # Explictly declare data types on join columns
}, error = function(e) {
   message(glue("Error reading or processing file {fn}: {e$message}"))
   return(NULL) # Return NULL on error
})

if (is.null(db_drivetime_data)) {
  stop("Failed to load or process DB drivetime data. Stopping script.")
}

#------------------------------------------------------------------------------
# Read service bc location data from source folder
#------------------------------------------------------------------------------

fn <- SBCLOC_FILEPATH
servicebc <- tryCatch({
  read_csv(fn) %>%
  mutate(across(c(loc), as.character)) %>% # Explictly declare data types on join columns
  st_as_sf(coords = c("coord_x", "coord_y"), crs = 3005)
}, error = function(e) {
   message(glue("Error reading or processing file {fn}: {e$message}"))
   return(NULL) # Return NULL on error
})

if (is.null(servicebc)) {
  stop("Failed to load or process DA shapefile. Stopping script.")
}

#------------------------------------------------------------------------------
# Join shapefiles to data for mapping
# Use left_join to color differently those da/db's missing data
#------------------------------------------------------------------------------
da_drivetime_map_data <- da_shapefile %>%
  left_join(da_drivetime_data, by = join_by(daid, loc)) 

if (nrow(da_drivetime_map_data) == 0)  {
  stop("No DA map data after joining with shapefiles")
}

db_drivetime_map_data <- db_shapefile %>%
  left_join(db_drivetime_data, by = join_by(dissemination_block_id, loc))

if (nrow(db_drivetime_map_data) == 0)  {
  stop("No DB map data after joining with shapefiles")
}

#------------------------------------------------------------------------------
# build map - this is where we provide options for build map function
#------------------------------------------------------------------------------
for (loc in names(LOC_LIST)) {
   loc_name <- LOC_LIST[[loc]]
   message(glue("Generating map for {loc_name} ({loc_code})..."))

  map_data <- da_drivetime_map_data # or da_drivetime_map_data

  var <- "drv_dist_mean"  # colnames(map_data) for other options
  var_title <- "Mean Distance (km)"

  plot_title <- glue("{var_title} by Dissemination Block, {loc_name}")

map_plot <- build_map(
  data = map_data,
  servicebc_data = servicebc,
  varname = var,
  loc_id = loc,
  map_theme = MAP_THEME,
  fill_scale = FILL_THEME,
  plot_title = plot_title,
  legend_title = var_title
)

# Save the plot - ** to be done still
ggsave(
    filename = glue("{var}_locality_{loc}.png"),
    path = MAP_OUT,
    plot = map_plot,
    width = 8,
    height = 7,
  dpi = 300
)
}
