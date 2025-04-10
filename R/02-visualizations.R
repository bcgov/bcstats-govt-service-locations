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

da_shapefile <- st_read(glue("{SHAPEFILE_DIR}/processed_da_with_location.shp"))

# TO DO: assign proper column names in earlier save
db_shapefile <- st_read(glue("{SHAPEFILE_DIR}/processed_db_with_location.shp")) %>%
  rename("dissemination_block_id" = "dssmn__",
         "landarea" = "landare",
         "location_id" = "loctn_d")

da_drivetime_data <- read_csv(glue("{SRC_DATA_FOLDER}/da_average_times_dist_all_locs.csv")) %>%
  clean_names() %>%
  mutate(across(c(daid, loc), as.character))

db_drivetime_data <- read_csv(glue("{SRC_DATA_FOLDER}/db_average_times_dist_all_locs.csv")) %>%
  clean_names() %>%
  mutate(across(c(dissemination_block_id, loc), as.character))

da_drivetime_map_data <- da_shapefile %>%
  left_join(da_drivetime_data, by = join_by(daid))

# sample map
db_drivetime_map_data <- db_shapefile %>%
  left_join(db_drivetime_data, by = join_by(dissemination_block_id))

# Map average drive distance by DA for locality "909"
build_map(
  data = db_drivetime_map_data,
  varname = "dwellings",
  loc_id = "909",
  loc_col = "loc",
  map_theme = MAP_THEME
)

# test for missing loc, remove MAP_THEME, varname not in data
