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
   message(glue("Error reading file {fn}:  {e$message}"))
})

fn <- glue("{SHAPEFILE_OUT}/processed_db_with_location.shp")
db_shapefile <- tryCatch({
  st_read(fn) %>%
  rename("dissemination_block_id" = "dssmn__",
         "landarea" = "landare",
         "loc" = "loctn_d") %>% 
  mutate(across(c(dissemination_block_id, loc), as.character)) # Explictly declare data types on join columns
}, error = function(e) {
   message(glue("Error reading file {fn}:  {e$message}"))
})


#------------------------------------------------------------------------------
# Read drive time data from source folder an
# eep all shapes and potentially colour the missing ones differently
#------------------------------------------------------------------------------
fn <- glue("{SRC_DATA_FOLDER}/da_average_times_dist_all_locs.csv")
da_drivetime_data <- tryCatch({
  read_csv(fn) %>%
  clean_names() %>%
  mutate(across(c(daid, loc), as.character)) # Explictly declare data types on join columns
}, error = function(e) {
   message(glue("Error reading file {fn}:  {e$message}"))
})

fn <- glue("{SRC_DATA_FOLDER}/db_average_times_dist_all_locs.csv")
db_drivetime_data <-  tryCatch({
  read_csv(fn) %>%
  clean_names() %>%
  mutate(across(c(dissemination_block_id, loc), as.character)) # Explictly declare data types on join columns
}, error = function(e) {
   message(glue("Error reading file {fn}:  {e$message}"))
})

#------------------------------------------------------------------------------
# Join shapefiles to data for mapping
# Use left_join to color differently those da/db's missing data
#------------------------------------------------------------------------------
da_drivetime_map_data <- da_shapefile %>%
  left_join(da_drivetime_data, by = join_by(daid))

if (nrow(da_drivetime_map_data) == 0)  {
  stop("No DA map data after joining with shapefiles")
}

db_drivetime_map_data <- db_shapefile %>%
  left_join(db_drivetime_data, by = join_by(dissemination_block_id))

if (nrow(db_drivetime_map_data) == 0)  {
  stop("No DB map data after joining with shapefiles")
}

#------------------------------------------------------------------------------
# build map
#------------------------------------------------------------------------------

build_map(
  data = db_drivetime_map_data,
  varname = "dwellings",
  loc_id = "909",
  map_theme = MAP_THEME
)
