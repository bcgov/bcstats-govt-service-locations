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
# Script: 00b-make-map-data.R

# Description: Loads and processes provincial DA and DB shapefiles.
# To reduce disk space and load time for development, specific localities are kept,
# based on a crosswalk file. The processeed shapefiles are written to to disk.

# Requirements:
#   - Requires R packages: `tidyverse`, `glue`, `janitor`, `sf`.
#   - Depends on `settings.R` for paths and constants.
#   - Requires input DA/DB shapefiles and the crosswalk CSV file.
#   - Requires read/write access to relevant data folders.

# Side Effects/Outputs:
#   - Writes shapefiles containing filtered DA/DB geometries
#     with location IDs to disk.
#   - Prints errors and stops execution if inputs are missing/invalid or
#     joins yield no results.
# ------------------------------------------------------------------------

library(tidyverse)
library(glue)
library(janitor)
library(sf)

source("R/settings.R")


# -----------------------------------------------------------------------------------------
# Load and prepare da-db-location crosswalk file
# -----------------------------------------------------------------------------------------

crosswalk <- tryCatch({
  read_csv(CROSSWALK_FILEPATH, col_types = cols(.default = "c"))
}, error = function(e) {
  stop(glue("Failed to read or process crosswalk file '{CROSSWALK_FILEPATH}': {e$message}"))
})

if (nrow(crosswalk) == 0) {
  stop("Crosswalk data is empty after processing.")
}

# -----------------------------------------------------------------------------------------
# Load and prepare da shapefile - check for invalid data
# -----------------------------------------------------------------------------------------

da_shapefiles_processed <- tryCatch({
    st_read(DA_SHAPE_FILEPATH) %>%
    filter(PRUID == "59") %>%
    st_transform(crs = 3005) %>%
    clean_names() %>%
    select(daid = dauid, landarea, geometry)
}, error = function(e) {
  stop(glue("Failed to read or process da shapefile '{DA_SHAPE_FILEPATH}': {e$message}"))
})

if (nrow(da_shapefiles_processed) == 0) {
  stop("DA shapefile data is empty after filtering for BC.")
}

# -----------------------------------------------------------------------------------------
# Load and prepare db shapefile - check for invalid data
# -----------------------------------------------------------------------------------------

db_shapefiles_processed <- tryCatch({
    st_read(DB_SHAPE_FILEPATH) %>%
    filter(PRUID == "59") %>%
    st_transform(crs = 3005) %>%
    clean_names() %>%
    select(dissemination_block_id = dbuid, landarea, geometry)

}, error = function(e) {
  stop(glue("Failed to read or process da shapefile '{DB_SHAPE_FILEPATH}': {e$message}"))
})

if (nrow(db_shapefiles_processed) == 0) {
  stop("DB shapefile data is empty after filtering for BC.")
}


# -----------------------------------------------------------------------------------------
# Filter shapefile to include only those da/db's in our localities of interest
# could do some light error checking here, but we don't have a true mapping from
# location to da/db (locality is still not definded and we don't have shapefiles for those)
# -----------------------------------------------------------------------------------------

db_with_location  <- db_shapefiles_processed  %>%
   inner_join(crosswalk %>% distinct(daid, dissemination_block_id, location_id), by = "dissemination_block_id")

if (nrow(db_with_location) == 0) {
  stop("No DAs after joining with the crosswalk.")
}

da_with_location  <- da_shapefiles_processed  %>%
   inner_join(crosswalk %>% distinct(daid, location_id), by = "daid")

if (nrow(da_with_location) == 0) {
  stop("No DAs after joining with the crosswalk.")
}


# -----------------------------------------------------------------------------------------
# write processed da/db shapefiles to source folder
# -----------------------------------------------------------------------------------------
# TODO - add tryCatch logic
st_write(da_with_location, glue("{SHAPEFILE_OUT}/processed_da_with_location.shp"))
st_write(db_with_location, glue("{SHAPEFILE_OUT}/processed_db_with_location.shp"))
