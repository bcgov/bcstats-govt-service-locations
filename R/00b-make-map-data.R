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
library(bcmaps)
library(bcdata)

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
  bcmaps::census_dissemination_area() %>%
    clean_names() %>%
    select("daid" = "dissemination_area_id", "landarea" = "feature_area_sqm", geometry)
}, error = function(e) {
  stop(glue("Failed to read or process da shapefile from bcmaps: {e$message}"))
})

if (nrow(da_shapefiles_processed) == 0) {
  stop("DA shapefile data is empty after filtering regions")
}

# -----------------------------------------------------------------------------------------
# Load and prepare db shapefile - check for invalid data
# currently a fragile - submitted issue to bcmaps for db layer
# -----------------------------------------------------------------------------------------

res <- bcdc_search("Current Census Dissemination Block")
i <- which(names(res) == "current-census-dissemination-blocks")

if (length(i) == 0) {
  stop(glue("Failed to find a valid DB shapefile in bcdata catalog"))
}

db_shapefiles_processed <- tryCatch({
  bcdc_query_geodata(res[[first(i)]]$id) %>% 
    collect() %>% 
    clean_names() %>%
    select(dissemination_block_id, "landare" = "feature_area_sqm")

}, error = function(e) {
  stop(glue("Failed to read or process db shapefile from bcdata catalog: {e$message}"))
})

if (nrow(db_shapefiles_processed) == 0) {
  stop("DB shapefile data is empty after filtering regions.")
}


# -----------------------------------------------------------------------------------------
# Filter shapefile to include only those da/db's in our localities of interest
# could do some light error checking here, but we don't have a true mapping from
# location to da/db (locality is still not defined and we don't have shapefiles for those)
# -----------------------------------------------------------------------------------------

db_with_location  <- db_shapefiles_processed  %>%
  inner_join(
    crosswalk %>%
      distinct(daid, dissemination_block_id, location_id) 
              , by = "dissemination_block_id"
  )

if (nrow(db_with_location) == 0) {
  stop("No DAs after joining with the crosswalk.")
}

da_with_location <- da_shapefiles_processed  %>%
  inner_join(
    crosswalk %>%
      distinct(daid, location_id)
    , by = "daid"
  )

if (nrow(da_with_location) == 0) {
  stop("No DAs after joining with the crosswalk.")
}

# -----------------------------------------------------------------------------------------
# write processed da/db shapefiles to source folder - changing to
# bcmaps/bcdata version introduces new warnings about truncated columns, but
# doesn't appear to otherwise affect things upstream
# -----------------------------------------------------------------------------------------
# TODO - add tryCatch logic
st_write(da_with_location, glue("{SHAPEFILE_OUT}/processed_da_with_location.shp"), append = TRUE)
st_write(db_with_location, glue("{SHAPEFILE_OUT}/processed_db_with_location.shp"), append = TRUE)
