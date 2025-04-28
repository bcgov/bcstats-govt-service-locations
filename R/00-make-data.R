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

#------------------------------------------------------------------------------
# Load req'd libraries and source constants and other settings
#------------------------------------------------------------------------------
source("R/settings.R")

library(tidyverse)
library(glue)
library(janitor)
#library(e1071)
library(sf)
library(bcmaps)
library(bcdata)

#------------------------------------------------------------------------------
# Pre-process drive time files for each locality and merge as a single file
#------------------------------------------------------------------------------

preprocess_locs <- function(fn, loc, tag = 'servicebc') {

  read_csv(fn, col_types = cols(.default = "c")) %>%
    clean_names() %>%
    filter(tag == tag) %>%
    rename(address_albers_x = site_albers_x,
           address_albers_y = site_albers_y,
           dbid = dissemination_block_id) %>%
    mutate(daid = str_sub(dbid, 1, 8))
}

file_paths <- file.info(list.files(DT_DATA_FOLDER,
                                   full.names = TRUE,
                                   pattern = NO_ERRS_FILE_PATTERN,
                                   recursive = TRUE)) %>%
  rownames_to_column("fn") %>%
  select(fn)


processed_files <- purrr::map_dfr(
  .x = file_paths$fn,
  .f = preprocess_locs
)

#------------------------------------------------------------------------------
# Make shapefiles for da-db-loc-csd
#------------------------------------------------------------------------------

csd_shapefiles <- census_subdivision() %>%
  clean_names() %>%
  select(
    csdid = census_subdivision_id,
    csd_name = census_subdivision_name,
    csd_desc = census_subdivision_type_desc,
    landarea = feature_area_sqm, geometry)

da_shapefiles <- census_dissemination_area() %>%
  clean_names() %>%
  select(daid = dissemination_area_id, landarea = feature_area_sqm, geometry)

# the metadata says 2016, but the data itself says its from census 2021, so use as crosswalk basis
db_shapefiles <- bcdc_query_geodata('76909e49-8ba8-44b1-b69e-dba1fe9ecfba') %>%
  collect() %>%
  clean_names() %>%
  select(
    dbid = dissemination_block_id,
    daid = dissemination_area_id,
    csdid = census_subdivision_id,
    landarea = feature_area_sqm
  )

#------------------------------------------------------------------------------
# Make a crosswalk for da-db-loc-csd
#------------------------------------------------------------------------------
# corresp data frame contains all DB's in BC
# remove geometry column for crosswalk
corresp <- db_shapefiles |>
  st_drop_geometry() |>
  select(-landarea) |>
  # get csd names from csd shapefile
  left_join(csd_shapefiles |> st_drop_geometry() |> select(-landarea), by = "csdid")

# contains all DB's in our data
crosswalk <-
  processed_files %>%
  distinct(daid, dbid) %>%
  left_join(corresp, by = join_by(daid, dbid))

# Data checks - some db's outside our csd's of interest.
# Let's leave them in for now and come back to this later after looking at them on a map.
crosswalk %>% count(csd_name, csdid)

# check these addresses out later, esp. bulkley-nechako as this region contains a small
# cluster of homes near Smithers, I believe.
extras <- processed_files %>%
  inner_join(crosswalk) %>%
  filter(!csd_name %in% CSD_NAMES)


#------------------------------------------------------------------------------
# Make population data
#------------------------------------------------------------------------------
pop_da <- cancensus::get_census(
    dataset = CANCENSUS_YEAR,
    regions = list(PR = "59"), # grab only BC
    level = 'DA'
  ) %>%
  clean_names() %>%
  select(c(all_of(POP_COLS), geo_uid)) %>%
  rename(daid = geo_uid)

pop_db <- cancensus::get_census(
    dataset = CANCENSUS_YEAR,
    regions = list(PR = "59"), # grab only BC
    level = 'DB' 
  ) %>%
  clean_names() %>%
  select(c(all_of(POP_COLS), geo_uid)) %>%
  rename(dbid = geo_uid)

pop_csd <- cancensus::get_census(
    dataset = CANCENSUS_YEAR,
    regions = list(PR = "59"), # grab only BC
    level = 'CSD' 
  ) %>%
  clean_names() %>%
  select(c(all_of(POP_COLS), geo_uid)) %>%
  rename(csd_name = region_name, csdid = geo_uid)

#------------------------------------------------------------------------------
# Service BC location data
#------------------------------------------------------------------------------

service_bc_locations <- processed_files %>% 
  left_join(crosswalk, by = join_by(dbid, daid)) %>%
  distinct(csd_name, csdid, nearest_facility, coord_x, coord_y)

#------------------------------------------------------------------------------
# Write output files
#------------------------------------------------------------------------------
write_csv(service_bc_locations, glue("{SRC_DATA_FOLDER}/full-service-bc-locs.csv"))
write_csv(processed_files, glue("{SRC_DATA_FOLDER}/full-drivetime-data.csv"))

write_csv(corresp, glue("{SRC_DATA_FOLDER}/full-csd-da-db-loc-correspondance.csv"))
write_csv(crosswalk, glue("{SRC_DATA_FOLDER}/full-csd-da-db-loc-crosswalk.csv"))

write_csv(pop_da, glue("{SRC_DATA_FOLDER}/full-population-da.csv"))
write_csv(pop_da %>% filter(csd_name %in% CSD_NAMES), glue("{SRC_DATA_FOLDER}/reduced-population-da.csv"))

write_csv(pop_db, glue("{SRC_DATA_FOLDER}/full-population-db.csv"))
write_csv(pop_db %>% filter(csd_name %in% CSD_NAMES), glue("{SRC_DATA_FOLDER}/reduced-population-db.csv"))

write_csv(pop_csd, glue("{SRC_DATA_FOLDER}/full-population-csd.csv"))
write_csv(pop_csd %>% filter(csd_name %in% CSD_NAMES), glue("{SRC_DATA_FOLDER}/reduced-population-csd.csv"))

st_write(da_shapefiles, glue("{SHAPEFILE_OUT}/full-da_with_location.gpkg"), append = FALSE) 
st_write(db_shapefiles, glue("{SHAPEFILE_OUT}/full-db_with_location.gpkg"), append = FALSE)
st_write(csd_shapefiles, glue("{SHAPEFILE_OUT}/full-csd_with_location.gpkg"), append = FALSE)

#------------------------------------------------------------------------------
# Write output files, filtering on CSD's of interest
#------------------------------------------------------------------------------
service_bc_locations %>% 
  filter(csd_name %in% CSD_NAMES) %>%
  write_csv(glue("{SRC_DATA_FOLDER}/reduced-service_bc_locs.csv"))

processed_files %>% 
  left_join(crosswalk, by = join_by(dbid, daid)) %>%
  filter(csd_name %in% CSD_NAMES) %>%
  write_csv(glue("{SRC_DATA_FOLDER}/reduced-drivetime-data.csv"))

da_shapefiles %>%
  inner_join(corresp %>% filter(csd_name %in% CSD_NAMES)) %>%
  st_write(glue("{SHAPEFILE_OUT}/reduced-da-with-location.gpkg"), append = FALSE)

db_shapefiles %>%
  inner_join(corresp  %>% filter(csd_name %in% CSD_NAMES)) %>%
  st_write(glue("{SHAPEFILE_OUT}/reduced-db-with-location.gpkg"), append = FALSE)

csd_shapefiles %>%
  filter(csd_name %in% CSD_NAMES) %>%
  st_write(glue("{SHAPEFILE_OUT}/reduced-csd-with-location.gpkg"), append = FALSE)

pop_csd %>%
  filter(csd_name %in% CSD_NAMES) %>%
  write_csv(glue("{SRC_DATA_FOLDER}/reduced-population-csd.csv"))

pop_da %>%
  filter(csd_name %in% CSD_NAMES) %>%
  write_csv(glue("{SRC_DATA_FOLDER}/reduced-population-da.csv"))

pop_db %>%
  filter(csd_name %in% CSD_NAMES) %>%
  write_csv(glue("{SRC_DATA_FOLDER}/reduced-population-db.csv"))

# clean up the environment
rm(list = ls())
gc()
