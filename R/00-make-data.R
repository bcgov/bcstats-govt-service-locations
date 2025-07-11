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

#------------------------------------------------------------------------------
# read drive time files for the full data
#------------------------------------------------------------------------------
fn <- glue::glue("{DT_DATA_FOLDER}/final_result_no_errors.csv")
full_processed_files <- read_csv(fn, col_types = cols(.default = "c")) %>%
    clean_names() %>%
    filter(tag == tag) %>%
    rename(address_albers_x = site_albers_x,
           address_albers_y = site_albers_y,
           dbid = dissemination_block_id) %>%
    mutate(daid = str_sub(dbid, 1, 8))

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
# Make a correspondance file for da-db-loc-csd
#------------------------------------------------------------------------------
# corresp data frame contains all DB's in BC
# don't need the geometry column
corresp <- db_shapefiles |>
  st_drop_geometry() |>
  select(-landarea) |>
  # get csd names from csd shapefile
  left_join(csd_shapefiles |> st_drop_geometry() |> select(-landarea), by = "csdid")

# Data checks - some db's are outside our csd's of interest.
# Let's leave them in for now and come back to this later after looking at them on a map.
# contains all DB's in our data
crosswalk <-
  full_processed_files %>%
  distinct(daid, dbid) %>%
  left_join(corresp, by = join_by(daid, dbid))

# check these addresses out later, esp. bulkley-nechako as this region contains a small
# cluster of homes near Smithers, I believe.
extras <- full_processed_files %>%
  inner_join(crosswalk) %>% 
  filter(!csd_name %in% CSD_NAMES)

#------------------------------------------------------------------------------
# Make population data
#------------------------------------------------------------------------------
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

service_bc_locations <- full_processed_files %>% 
  left_join(crosswalk, by = join_by(dbid, daid)) %>%
  distinct(csd_name, csdid, nearest_facility, coord_x, coord_y)

#------------------------------------------------------------------------------
# Write output files
#------------------------------------------------------------------------------
write_csv(service_bc_locations, glue("{SRC_DATA_FOLDER}/full-service-bc-locs.csv"))
write_csv(full_processed_files, glue("{SRC_DATA_FOLDER}/full-processed-drivetime-data.csv"))

write_csv(corresp, glue("{SRC_DATA_FOLDER}/csd-da-db-loc-correspondance.csv"))

write_csv(pop_db, glue("{SRC_DATA_FOLDER}/full-population-db.csv"))
write_csv(pop_csd, glue("{SRC_DATA_FOLDER}/full-population-csd.csv"))

st_write(db_shapefiles, glue("{SHAPEFILE_OUT}/full-db_with_location.gpkg"), append = FALSE)
st_write(csd_shapefiles, glue("{SHAPEFILE_OUT}/full-csd_with_location.gpkg"), append = FALSE)

#------------------------------------------------------------------------------
# Write output files, filtering on CSD's of interest
#------------------------------------------------------------------------------
service_bc_locations %>% 
  filter(csd_name %in% CSD_NAMES) %>%
  write_csv(glue("{SRC_DATA_FOLDER}/reduced-service_bc_locs.csv"))

full_processed_files %>% 
  left_join(crosswalk, by = join_by(dbid, daid)) %>%
  filter(csd_name %in% CSD_NAMES) %>%
  write_csv(glue("{SRC_DATA_FOLDER}/reduced-drivetime-data.csv"))

db_shapefiles %>%
  inner_join(corresp  %>% filter(csdid %in% CSDIDS)) %>%
  st_write(glue("{SHAPEFILE_OUT}/reduced-db-with-location.gpkg"), append = FALSE)

csd_shapefiles %>%
  filter(csdid %in% CSDIDS) %>%
  st_write(glue("{SHAPEFILE_OUT}/reduced-csd-with-location.gpkg"), append = FALSE)

pop_csd %>% 
  inner_join(
    crosswalk %>% 
    distinct(csdid) %>% 
    filter(csdid %in% CSDIDS)
  , by = join_by(csdid)) %>%
  write_csv(glue("{SRC_DATA_FOLDER}/reduced-population-csd.csv"))

pop_db %>%
  inner_join(crosswalk, by = join_by(dbid)) %>%
  filter(csdid %in% CSDIDS) %>%
  write_csv(glue("{SRC_DATA_FOLDER}/reduced-population-db.csv"))

# clean up the environment
rm(list = ls())
gc()
