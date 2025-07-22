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
## population projections ----
## from catalogue
#------------------------------------------------------------------------------

# figure out ID of file we want
# bcdc_search("sub-provincial projections")
# population projections: 86839277-986a-4a29-9f70-fa9b1166f6cb
#       - csd resource: 0e15d04d-127c-457a-b999-20800c929927
# household estimates: 2a8ddf6c-dfb9-4187-a66d-9bb15b15ea83
# note that these don't have gender/age breakdowns, which we will want

pop_projections <- bcdc_get_data(
  "86839277-986a-4a29-9f70-fa9b1166f6cb",
  resource = "0e15d04d-127c-457a-b999-20800c929927"
) |>
  janitor::clean_names() |>
  mutate(
    region = paste0(
      "59",
      str_pad(as.character(region), width = 5, side = "left", pad = "0")
    )
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
  clean_names()  %>%
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

# =========================================================================== #
# DB population projections ----
# we want to convert the CSD population projections into DB projections
# we do this by approximation,
# and assume that the proportion of the DB that makes
# up the CSD each year doesn't change
# while not perfect, it will give reasonable estimates
# has a pct of csd column that we can use
# to multiply by any other population projections to get estimates
# =========================================================================== #

# first, we create a new 'csd_clean' label to match the population projections
# as some CSDs are rolled up in the projections
get_clean_csd <- pop_db |>
  left_join(corresp, by = c("dbid")) |>
  left_join(
    pop_projections |>
      distinct(region) |>
      mutate(in_projections = 1),
    by = c("csdid" = "region")
  ) |>
  # if rolled up, last 3 digits replaced with '999'
  mutate(
    csd_clean = if_else(
      is.na(in_projections),
      paste0(str_sub(csdid, 1, 4), "999"),
      csdid
    )
  )

# now get pct of each 'clean' csd that is taken up by each DB
prop_of_csd <- get_clean_csd |>
  group_by(csd_clean) |>
  mutate(csd_population = sum(population, na.rm=TRUE)) |>
  ungroup() |>
  mutate(pct_of_csd = if_else(population==0, 0, population / csd_population))

# join back to projections to get yearly estimates
# for each age, gender, year of interest
db_projections <- prop_of_csd |>
  select(
    dbid,
    daid,
    csdid,
    csd_clean,
    csd_name,
    csd_desc,
    area_sq_km,
    population,
    csd_population,
    dwellings,
    households,
    pct_of_csd
  ) |>
  left_join(
    pop_projections |>
      filter(year %in% c(CURRENT_YEAR, CURRENT_YEAR + 5, CURRENT_YEAR + 10)),
    by = c("csd_clean" = "region"),
    relationship = "many-to-many"
  )

# Preprocess db_projections data to transform age columns into rows
# The columns x0, x1, etc. represent different age groups
db_projections_transformed <- db_projections |>
  # Get all column names that start with 'x' followed by digits (age columns)
  pivot_longer(
    cols = starts_with("x") & matches("^x\\d+$"),
    names_to = "age_column",
    values_to = "population_by_age"
  ) |>
  # Extract age values from column names (remove 'x' prefix)
  mutate(
    age = as.numeric(str_replace(age_column, "^x", "")),
    # Create age groups (0-4, 5-9, etc.)
    age_group = case_when(
      age < 5 ~ "0-4",
      age < 10 ~ "5-9",
      age < 15 ~ "10-14",
      age < 20 ~ "15-19",
      age < 25 ~ "20-24",
      age < 30 ~ "25-29",
      age < 35 ~ "30-34",
      age < 40 ~ "35-39",
      age < 45 ~ "40-44",
      age < 50 ~ "45-49",
      age < 55 ~ "50-54",
      age < 60 ~ "55-59",
      age < 65 ~ "60-64",
      age < 70 ~ "65-69",
      age < 75 ~ "70-74",
      age < 80 ~ "75-79",
      age < 85 ~ "80-84",
      age < 90 ~ "85-89",
      TRUE ~ "90+"
    ),
    # Calculate population estimate for each DB
    population = population_by_age * pct_of_csd,
    total = total * pct_of_csd
  )

#------------------------------------------------------------------------------
# Write output files
#------------------------------------------------------------------------------
write_csv(service_bc_locations, glue("{SRC_DATA_FOLDER}/full-service-bc-locs.csv"))
write_csv(full_processed_files, glue("{SRC_DATA_FOLDER}/full-processed-drivetime-data.csv"))

write_csv(corresp, glue("{SRC_DATA_FOLDER}/csd-da-db-loc-correspondance.csv"))
saveRDS(db_projections_transformed, glue("{SRC_DATA_FOLDER}/full_db_projections_transformed.rds"))

write_csv(pop_db, glue("{SRC_DATA_FOLDER}/full-population-db.csv"))
write_csv(pop_csd, glue("{SRC_DATA_FOLDER}/full-population-csd.csv"))

write_csv(pop_projections, glue("{SRC_DATA_FOLDER}/full-population-projections.csv"))

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
