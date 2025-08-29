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
# Read required data
# =========================================================================== #

# Crosswalk contains 52,423 DB's over 751 CSD's.  The CSD's align in the
# csd_shapefiles from bcmaps, and cancensus data.  The DB's do not align perfectly
# in that there are 36 fewer DB's in the census data than in the data from BC Geographic Warehouse .
# However, this crosswalk only contains the census DB's and CSD's.  The two geographies are
# in alignment for this table, as best as I can tell (i.e. this is a complete crosswalk).
crosswalk <- read_csv(glue("{SRC_DATA_FOLDER}/csd-da-db-loc-correspondance.csv"),
   col_types = cols(.default = "c"))

# census populations - 745 census subdivisions found in our data
# This data comes from the census (52,387 DB's).
# This is 36 fewer than listed in the BC Geographic Warehouse (52,423).
# There is a note that the data in the is from 2016, but each record is labeled 2021, so possible we need to check on this.
pop_db <- read_csv(
  glue("{SRC_DATA_FOLDER}/full-population-db.csv"),
  col_types = cols(.default = "c")
) |>
  clean_names() |>
  mutate(across(c(area_sq_km, population, dwellings, households), as.numeric)) |>
  inner_join(crosswalk, by = join_by(dbid))

# our full set of drive data from DSS:  2,052,803 records over 41,991 DB's and 525 CSD's.
# This is 10,432 fewer DB's than in the BC Data Catalog/BC Geographic Warehouse data (52,423).
# 4 db's from our drive data are not in the census data, but they are all in the data from BC Data Catalog.  They
# represent very small areas (islands, looks like) around vancouver island.
# They are in the set of 36 DB's that are missing, which are also mostly areas around vancouver island and the lower mainland.

drivetime_data <- read_csv(
    glue("{SRC_DATA_FOLDER}/full-processed-drivetime-data.csv"),
    col_types = cols(.default = "c")
) |>
  clean_names() |>
  mutate(across(c(drv_time_sec, drv_dist), as.numeric)) |>
  inner_join (crosswalk, join_by(dbid, daid))

# these DBs are from the census data, so contain 52,387 DB's and 751 CSD's. Population projections come from BC Stats population projections + census population ratios. 
# For those csdid's/db's that are not in the population projections, they 
# were proportionally allocated to the corresponding Unincorporated CSD (regional district).

db_projections_transformed_raw <- readRDS(glue("{SRC_DATA_FOLDER}/full-db-projections-transformed.rds")) |> 
  filter(dbid %in% (crosswalk |> pull(dbid))) |> 
  filter(gender == 'T', year %in% c(2025, 2030, 2035))

# =========================================================================== #
# DB population projections, current, 5yr, 10yr
# number of addresses in each CSD (from our data)
# number of offices in each CSD (from our data)
# =========================================================================== #

population_estimates_three_year <- db_projections_transformed_raw  |>
  summarise(
    population = sum(population, na.rm = TRUE), 
    .by = c(year, region_name, csdid)) |>
  pivot_wider(
    names_from = year, 
    values_from = population, 
    values_fill = 0)

age_estimates_current_year <- db_projections_transformed_raw |>
filter(year == 2025) |>
  summarize(
    est_population_0_to_14_yrs = sum(population[age >= 0 & age < 15], na.rm = TRUE),
    est_population_15_to_24_yrs = sum(population[age >= 15 & age < 25], na.rm = TRUE),
    est_population_25_to_64_yrs = sum(population[age >= 25 & age < 65], na.rm = TRUE),
    est_population_over_64_yrs = sum(population[age >= 65], na.rm = TRUE),
    .by = c(region_name, csdid)
  )

median_population <- db_projections_transformed_raw |>
  filter(year == 2025) |>
  summarize(
     population = sum(population, na.rm = TRUE),
     .by = c(csdid, csd_name, age)) |>
  summarize(
    median_age = ifelse(sum(population, na.rm = TRUE) == 0, 0, weighted.median(age, population, na.rm = TRUE)),
    mean_age = ifelse(sum(population, na.rm = TRUE) == 0, 0, weighted.mean(age, population, na.rm = TRUE)),
    .by = c(csdid, csd_name)
  )

#------------------------------------------------------------------------------
# drivetime metrics by csd
#------------------------------------------------------------------------------

drivetime_metrics <- drivetime_data |>
  summarise(
    n_addresses = n(),
    n_sbc_offices = n_distinct(nearest_facility),
    mean_driving_distance = mean(drv_dist, na.rm = TRUE),
    median_driving_distance = median(drv_dist, na.rm = TRUE),
    mean_driving_time = mean(drv_time_sec, na.rm = TRUE) / 60,
    median_driving_time = median(drv_time_sec, na.rm = TRUE) / 60,
    .by = c(csd_name, csdid)
  )

#------------------------------------------------------------------------------
# bin the data by driving distance and time
#------------------------------------------------------------------------------

drive_distance_bins <- drivetime_data |>
  summarize(
    n_addresses_0_5_km = sum(drv_dist < 5.0, na.rm = TRUE),
    n_addresses_5_10_km = sum(drv_dist >= 5.0 & drv_dist < 10.0, na.rm = TRUE),
    n_addresses_10_15_km = sum(drv_dist >= 10.0 & drv_dist < 15.0, na.rm = TRUE),
    n_addresses_15_30_km = sum(drv_dist >= 15.0 & drv_dist < 30.0, na.rm = TRUE),
    n_addresses_30_45_km = sum(drv_dist >= 30.0 & drv_dist < 45.0, na.rm = TRUE),
    n_addresses_45_60_km = sum(drv_dist >= 45.0 & drv_dist < 60.0, na.rm = TRUE),
    n_addresses_60_90_km = sum(drv_dist >= 60.0 & drv_dist < 90.0, na.rm = TRUE),
    n_addresses_90_135_km = sum(drv_dist >= 90.0 & drv_dist < 135.0, na.rm = TRUE),
    n_addresses_135_180_km = sum(drv_dist >= 135.0 & drv_dist < 180.0, na.rm = TRUE),
    n_addresses_180_plus_km = sum(drv_dist >= 180.0, na.rm = TRUE),
    .by = c(csd_name, csdid)
  )

drive_time_bins <- drivetime_data |>
  mutate(drv_time_min = drv_time_sec / 60) |>
  summarize(
    n_addresses_0_5_min = sum(drv_time_min < 5, na.rm = TRUE),
    n_addresses_5_10_min = sum(drv_time_min >= 5 & drv_time_min < 10, na.rm = TRUE),
    n_addresses_10_15_min = sum(drv_time_min >= 10 & drv_time_min < 15, na.rm = TRUE),
    n_addresses_15_20_min = sum(drv_time_min >= 15 & drv_time_min < 20, na.rm = TRUE),
    n_addresses_20_30_min = sum(drv_time_min >= 20 & drv_time_min < 30, na.rm = TRUE),
    n_addresses_30_40_min = sum(drv_time_min >= 30 & drv_time_min < 40, na.rm = TRUE),
    n_addresses_40_60_min = sum(drv_time_min >= 40 & drv_time_min < 60, na.rm = TRUE),
    n_addresses_60_90_min = sum(drv_time_min >= 60 & drv_time_min < 90, na.rm = TRUE),
    n_addresses_90_120_min = sum(drv_time_min >= 90 & drv_time_min < 120, na.rm = TRUE),
    n_addresses_120_150_min = sum(drv_time_min >= 120 & drv_time_min < 150, na.rm = TRUE),
    n_addresses_over_150_min = sum(drv_time_min >= 150, na.rm = TRUE),
    .by = c(csd_name, csdid)
  )

# =========================================================================== #
# All together ----
# =========================================================================== #

# Combined, assuming we want the full set of CSD's
# If we want to rollup to Unincorporated areas then we can group by region_name/clean_csd in aggregations above
# Since we only have data on 525 of the 751 CSDS  => missing data on 220/423 IRI's, 3/160 RDA's, and 3/3 S-E's
combined_stats <- population_estimates_three_year |>
  left_join(age_estimates_current_year, by = c("csdid", "region_name")) |>
  left_join(drivetime_metrics, by = "csdid") |>
  left_join(drive_distance_bins, by = c("csdid", "csd_name")) |>
  left_join(drive_time_bins, by = c("csdid", "csd_name")) |>
  mutate(rural_office = 'Y/N', rural_residents = 0) |>
  relocate(csd_name, csdid, .before = region_name) |>
  relocate(n_addresses, n_sbc_offices, .after = region_name) |>
  relocate(addresses_under_5_km, addresses_5_to_20_km, addresses_over_20_km, .after = median_driving_distance) |>
  rename(
    estimated_population_2025 = `2025`,
    `5_yr_projection_2030` = `2030`,
    `10_year_projection_2035` = `2035`) 


# =========================================================================== #
# Write output table to CSV ----
# =========================================================================== #

# Create output directory if it doesn't exist
if (!dir.exists(TABLES_OUT)) {
  dir.create(TABLES_OUT, recursive = TRUE)
}

# Write combined statistics table
write_csv(combined_stats, file.path(TABLES_OUT, "csd-statistics-for-SBC.csv"))

# Print summary of what was written
cat("Combined statistics written to:", file.path(TABLES_OUT, "csd-statistics-for-SBC.csv"), "\n")
cat("Total CSDs in combined statistics:", nrow(combined_stats), "\n")
