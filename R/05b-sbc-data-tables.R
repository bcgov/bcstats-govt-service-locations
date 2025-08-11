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


# =========================================================================== #
# Load libraries and settings ----
# =========================================================================== #

source("R/settings.R")

# =========================================================================== #
# Read required data in ----
# =========================================================================== #

# Crosswalk contains 52,423 DB's over 751 CSD's.  The CSD's align in the
# csd_shapefiles from bcmaps, and cancensus data.  The DB's do not align perfectly
# in that there are 36 fewer DB's in the census data than in the data from BC Geographic Warehouse .
# However, this crosswalk only contains the census DB's and CSD's.  The two geographies are
# in alignment for this table, as best as I can tell (i.e. this is a complete crosswalk).
crosswalk <-
  read_csv(
    glue("{SRC_DATA_FOLDER}/csd-da-db-loc-correspondance.csv"),
    col_types = cols(.default = "c")
  ) |>
  clean_names()


# Read in the full set of data provided by DSS.
# Includes 2,052,805 records over 41,992 DB's and 526 CSD's
drivetime_data <-
  read_csv(
    glue("{SRC_DATA_FOLDER}/full-processed-drivetime-data.csv"),
    col_types = cols(.default = "c")
  ) |>
  clean_names() |>
  mutate(across(c(drv_time_sec, drv_dist), as.numeric))

# complete assignments contains 52424 DB's across 65 Service BC locations.
# contains a crosswalk between DB's and Service BC locations.
complete_assignments <-
  read_csv(glue("{SRC_DATA_FOLDER}/complete-db-assignments.csv"),
            col_types = cols(.default = col_guess())
  ) |>
  mutate(dbid = as.character(dbid))

# 65 Service BC locations over 526 CSD's.  So, this file will contain 
# multiple csd's for some service bc locations.  This comes from our drivetime data. 
sbc_locs <- read_csv(SBCLOC_FILEPATH) |>
  clean_names()

# contains pre-processed population projections for 52387 DB's and 751 CSD's, 
# the unincorporated municipalities are rolled up into 29 regions, so the count of
# CSD's is actually 191.
db_projections_transformed <- readRDS(glue("{SRC_DATA_FOLDER}/full-db-projections-transformed.rds"))

# --------------------------------------------------------------------------------
# make drivetime data
# --------------------------------------------------------------------------------

# Join with drivetime data
drivetime_data_full <- complete_assignments |>
  left_join(drivetime_data, by = "dbid") |>
  # fix the daid column to have no NAs
  mutate(daid = str_sub(dbid, 1, 8))  #~ 10,000 extra dbids - TODO: investigate this.

sbc_names <- sbc_locs |>
  #filter (csd_name %in% CSD_NAMES) |> # toggle comment to focus on specific CSD's
  pull(nearest_facility)

# this method is useful when we want to look at specific csd's
# filtering doesn't reduce the data for the full set of assignments.
drivetime_data_focused <- drivetime_data_full |>
  filter(assigned %in% sbc_names) |>
  filter(nearest_facility %in% sbc_names | is.na(nearest_facility)) |>
  left_join(crosswalk, by = c("dbid", "daid"))

#------------------------------------------------------------------------------
# bin the data by driving distance
#------------------------------------------------------------------------------

drive_distance_bins <- drivetime_data_focused |>
  summarize(
    n_under_5 = sum(drv_dist < 5.0, na.rm = TRUE),
    n_5_to_20 = sum(drv_dist >= 5.0 & drv_dist < 20.0, na.rm = TRUE),
    n_20_plus = sum(drv_dist >= 20.0, na.rm = TRUE),
    .by = c(assigned)) |>
  filter(assigned %in% sbc_names)

#------------------------------------------------------------------------------
# count of addresses by assigned
#------------------------------------------------------------------------------

addresses_serviced <- drivetime_data_focused |>
  summarize(
    n_addresses = n(),
    mean_drv_dist = mean(drv_dist, na.rm = TRUE),
    .by = c(assigned)
  ) |>
  filter(assigned %in% sbc_names)

# counts of CSD's covered by each service BC location
csds_serviced <- drivetime_data_focused |>
  summarize(
    n_csds = n_distinct(csdid),
    .by = c(assigned)
  ) |>
  filter(assigned %in% sbc_names)

#------------------------------------------------------------------------------
# DB population projections ----
#------------------------------------------------------------------------------

population_estimates_three_year_all <- drivetime_data_focused |>
  distinct(assigned, assignment_method, csdid, dbid) |>
  # expand drivetime data to include all years of interest for each row
  expand_grid(tibble(year = rep(unique(db_projections_transformed$year)))) |>
  left_join(
    db_projections_transformed |>
      filter(gender == "T") |>
      select(dbid, year, age, population, total, area_sq_km),
    by = c("dbid", "year")
  ) |>
  filter(!is.na(csdid))

population_estimates_three_year <- population_estimates_three_year_all |>
  summarize(
    pop = sum(population, na.rm = TRUE),
    .by = c(assigned, year)
  ) |>
  pivot_wider(
    names_from = year,
    values_from = pop,
    values_fill = 0
  ) |>
  filter(assigned %in% sbc_names)

#------------------------------------------------------------------------------
# count of age groups by assigned
#------------------------------------------------------------------------------
age_estimates_current_year <- population_estimates_three_year_all |>
  filter(year == 2025) |>
  group_by(assigned) |>
  summarize(
    age_0_19 = sum(population[age < 19], na.rm = TRUE),
    age_19_64 = sum(population[age >= 19 & age < 65], na.rm = TRUE),
    age_65_plus = sum(population[age >= 65], na.rm = TRUE)
  ) |>
  filter(assigned %in% sbc_names)


# =========================================================================== #
# All together ----
# =========================================================================== #

# Combined, assuming we want the full set of CSD's
# If we want to rollup to Unincorporated areas then we can group by region_name/clean_csd in aggregations above
combined_stats <- population_estimates_three_year |>
  left_join(age_estimates_current_year, by = c("assigned")) |>
  left_join(addresses_serviced, by = "assigned") |>
  left_join(csds_serviced, by = c("assigned")) |>
  left_join(drive_distance_bins, by = c("assigned")) |>
  relocate(n_addresses, n_csds, .after = `2035`)

combined_stats |>
  filter(assigned %in% sbc_names) 

