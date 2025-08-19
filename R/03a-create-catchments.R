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
# Script: 03a-create-catchments.R

# Description: This script assigns every dissemination block (DB) to a 
# catchment area, first using drive time data and then using spatial 
# proximity for any unassigned DBs. It saves the results to a CSV file 
# for use in subsequent scripts.

# Requirements:
#   - Requires R packages: `tidyverse`, `glue`, `janitor`, `sf`
#   - Depends on `settings.R` for paths and constants
#   - Requires the assignment functions from `calculations.r`
#   - Requires input DB shapefiles and drive time data

# Side Effects/Outputs:
#   - Saves complete DB assignments to a CSV file in the TABLES_OUT folder
#   - Saves assignment statistics to a CSV file in the TABLES_OUT folder
# ------------------------------------------------------------------------

source("R/settings.R")
source("R/fxns/calculations.r")  

# ----------------------------------------------------------------------------
# Load input data
# ----------------------------------------------------------------------------

# Locations of all Service BC locations 
sbc_locs <- read_csv(glue("{SRC_DATA_FOLDER}/full-service-bc-locs.csv")) |>
  st_as_sf(
    coords = c('coord_x', 'coord_y'),
    crs = 3005
  )

# Drive time data - required for 'SBC catchments'
drivetime_data <-
  read_csv(
    glue("{SRC_DATA_FOLDER}/full-processed-drivetime-data.csv"),
    col_types = cols(.default = "c")
  ) %>%
  clean_names() %>%
  mutate(across(c(drv_time_sec, drv_dist), as.numeric))

# Census dissemination block population data
pop_db <- read_csv(glue("{SRC_DATA_FOLDER}/full-population-db.csv"), col_types = cols(.default = "c")) %>%
  clean_names() %>%
  mutate(across(c(area_sq_km, population, dwellings, households), as.numeric))

# DB shapefiles
db_shapefile <-
  st_read(glue("{SHAPEFILE_OUT}/full-db-with-location.gpkg")) %>%
  mutate(across(c(landarea), as.numeric))


# ----------------------------------------------------------------------------
# Create catchment areas using traditional drive time assignment
# ----------------------------------------------------------------------------

# First assign every dissemination block a 'nearest facility' based on drive time
drive_time_assignments <- assign_nearest_facility(drivetime_data)

# Calculate how many DBs are unassigned
unassigned_count_before <- db_shapefile |>
  left_join(drive_time_assignments, by = "dbid") |>
  filter(is.na(assigned)) |>
  nrow()

message("Number of unassigned DBs before spatial assignment: ", unassigned_count_before)

# ----------------------------------------------------------------------------
# Apply spatial assignment method to fill in unassigned areas
# ----------------------------------------------------------------------------

# Assign all unassigned DBs directly to the nearest facility
complete_assignments <- assign_unassigned_dbs(
  db_shapefile,
  drive_time_assignments,
  facility_locations = sbc_locs,
  batch_size = 20000,
  verbose = TRUE
)

# Calculate how many DBs are unassigned after spatial assignment
unassigned_count_after <- db_shapefile |>
  left_join(complete_assignments, by = "dbid") |>
  filter(is.na(assigned)) |>
  nrow()

message("Number of unassigned DBs after spatial assignment: ", unassigned_count_after)

# Calculate how many DBs were assigned using the spatial method
spatial_assignments_count <- complete_assignments |>
  filter(assignment_method == "nearest_facility") |>
  nrow()

message("Number of DBs assigned directly to nearest facility: ", spatial_assignments_count)

# Save the complete assignments for use in other scripts
write_csv(
  complete_assignments,
  glue("{SRC_DATA_FOLDER}/complete_db_assignments.csv")
)

# ----------------------------------------------------------------------------
# Make complete assignments list to share with SBC
# Also shapefiles containing service BC catchments
# (shp file likely a more familiar format for SBC)
# ----------------------------------------------------------------------------

# remove unused columns (assignment_method, min_distance) for SBC cut
complete_assignments |>
  select(dbid, assigned) |>
  distinct() |> # there shouldn't be any dups, but just in case
  write_csv(glue::glue("{FOR_SBC_OUT}/complete-db-assignments-for-SBC.csv"))

# create shapefiles for each SBC facility location catchment
db_shapefile |>
  left_join(complete_assignments, by = "dbid") |>
  filter(!is.na(assigned)) |>  # there shouldn't be any nas, but just in case
  summarize(geometry = st_union(geom), .by = "assigned") |>
  st_write(glue::glue("{FOR_SBC_OUT}/sbc-catchments.shp"))

# ----------------------------------------------------------------------------
# Extra checks for QA here down. This is not part of the main assignment process.
# ----------------------------------------------------------------------------

# QA the unassigned DBs 
# save the list of unassigned dbs, together with their 2021 census pops
db_check <- complete_assignments %>% 
  left_join(pop_db, by='dbid') %>% 
  left_join(db_no_route, by='dbid') %>%
  select(dbid, assigned, assignment_method, population, no_route) %>% 
  mutate(no_route = if_else(is.na(no_route), FALSE, no_route))

# look at populations of unassigned dbs
# note that the 4 rows will show:
# drive_time / no_route = FALSE - those with successful drive times
# drive_time / no_route = TRUE - those dbs with some address errors
# nearest_facility / no_route = FALSE - those dbs that had no addresses at all
# nearest_facility / no_route = TRUE - those dbs that had addresses but they all errored
db_qa_summary  <- db_check  %>% 
  group_by(assignment_method, no_route) %>% 
  summarize(
    n = n_distinct(dbid),
    average_pop = mean(population, na.rm = TRUE),
    min_pop = min(population, na.rm = TRUE),
    max_pop = max(population, na.rm = TRUE),
    median_pop = median(population, na.rm = TRUE),
    pct_zero_pop = sum(population == 0, na.rm = TRUE) / n(),
    pct_na_pop = sum(is.na(population)) / n(),
    .groups = "drop"
    )

db_qa_summary

# and the summary stats
db_qa_summary %>% 
  write_csv(glue("{TABLES_OUT}/unassigned_dbs_summary.csv"))


# save the list of dbs that do/don't have assignees for further investigation
db_check %>% 
  arrange(assignment_method, desc(population)) %>% 
  write_csv(glue("{TABLES_OUT}/unassigned_dbs.csv"))

rm(list = ls())
gc()
