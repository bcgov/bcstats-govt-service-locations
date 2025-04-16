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
# Script: 02-map-plots.R

# Description: Loads previously processed DA and DB shapefiles (containing location IDs)
# and corresponding processed drive time summary statistics CSV files. Prepares the final 
# spatial data frames ready for mapping.  A sample map visualization is generated and saved to disk.

# Requirements:
#   - Requires R packages: `tidyverse`, `glue`, `janitor`, `sf`, `svglite`, `scales`
#   - Depends on `settings.R` for configuration constants (paths, `MAP_THEME`).
#   - Requires shapefiles in `SHAPEFILE_DIR`.
#   - Requires drive time summary CSV files in `SRC_DATA_FOLDER` containing variables (numeric type) to map
#   - Requires the `build_map` function in `R/fxns/plots.r`

# Side Effects/Outputs:
#   - Calls `build_map` function, which prints a ggplot map object to
#     the default graphics device.
#   - Prints errors and stops execution if input files are missing.
# ------------------------------------------------------------------------


library(tidyverse)
library(glue)
library(janitor)
library(sf)
library(svglite)
library(scales)
library(snakecase)

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

na_prop <- sum(is.na(da_drivetime_map_data$n_address))/ nrow(da_drivetime_map_data)
message(glue("({percent(na_prop)}) of NAs in DA map data"))

low_counts_prop <- sum(da_drivetime_map_data$n_address < 5) / nrow(da_drivetime_map_data)
message(glue("({percent(low_counts_prop)}) of DA regions contain fewer than 5 observations"))

db_drivetime_map_data <- db_shapefile %>%
  left_join(db_drivetime_data, by = join_by(dissemination_block_id, loc))

if (nrow(db_drivetime_map_data) == 0)  {
  stop("No DB map data after joining with shapefiles")
}

na_prop <- sum(is.na(db_drivetime_map_data$n_address))/ nrow(db_drivetime_map_data)
message(glue("({percent(na_prop)}) of NAs in DB map data"))

low_counts_prop <- sum(db_drivetime_map_data$n_address < 5) / nrow(db_drivetime_map_data)
message(glue("({percent(low_counts_prop)}) of DB regions have fewer than 5 observations"))


# ------------------------------------------------------------------
# testing only
# just for checking low counts
# ------------------------------------------------------------------

da_drivetime_data$loc_name <-unlist(LOC_LIST[da_drivetime_data$loc])
db_drivetime_data$loc_name <-unlist(LOC_LIST[db_drivetime_data$loc])

da_drivetime_data %>%
  filter(n_address < 5) %>%
  select(!starts_with("drv")) %>%
  sf::st_drop_geometry() %>%
  write_csv(glue("{TABLES_OUT }/low_counts_da.csv"))

db_drivetime_data %>%
  filter(n_address < 5) %>%
  select(!starts_with("drv")) %>%
  sf::st_drop_geometry() %>%
  write_csv(glue("{TABLES_OUT }/low_counts_db.csv"))

#------------------------------------------------------------------------------
# build map - this is where we provide options for build map function
#------------------------------------------------------------------------------

# user-defined map parameters
var <- "n_address"  # colnames(map_data) for other options
var_title <- "Count of Addresses"
region_title <- "Dissemination Area"
plot_subtitle <- "(Regions with 0-4 data points are shown in red)"

map_data  <- db_drivetime_map_data

if(region_title == "Dissemination Area"){
  map_data  <- da_drivetime_map_data
}

map_data <- map_data %>%
  mutate(n_address = if_else(n_address < 5, NA, n_address))

for (loc in names(LOC_LIST)) {
  loc_name <- LOC_LIST[[loc]]
  message(glue("Generating map for {loc_name} ({loc})..."))

  plot_title <- glue("{var_title} by {region_title}, {loc_name}")

  map_plot <- build_map(
    data = map_data,
    servicebc_data = servicebc,
    varname = var,
    loc_id = loc,
    map_theme = MAP_THEME,
    fill_scale = FILL_THEME,
    plot_title = plot_title,
    plot_subtitle = plot_subtitle,
    legend_title = var_title
  )

  # Save the plot
  fn <- to_snake_case(glue("{var} by {region_title}, {loc_name}"))
  fn <- glue("{fn}.svg")
  ggsave(
    filename = fn,
    path = MAP_OUT,
    plot = map_plot,
    width = 8,
    height = 7,
    device = "svg"
  )
}

