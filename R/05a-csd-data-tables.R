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

# these are from the census data, so contain 52,387 DB's and 751 CSD's.
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
  summarise(population = sum(population, na.rm = TRUE), .by = c(year, region_name, csdid)) |>
  pivot_wider(names_from = year, values_from = population, values_fill = 0)

age_estimates_current_year <- db_projections_transformed_raw |>
  filter(year == 2025) |>
  mutate(age_grp = case_when(
    age < 19 ~ "0-19",
    age >= 19 & age < 65 ~ "19-64",
    age >= 65 ~ "65+"
  )) |>
  summarise(
    population = sum(population, na.rm = TRUE),
    .by = c(region_name, csdid, age_grp)) |>
  pivot_wider(
    names_from = age_grp,
    values_from = population,
    values_fill = 0)

addresses_serviced <- drivetime_data |>
  summarise(n_address = n(), .by = c(csd_name, csdid))

offices_serviced <- drivetime_data |>
  summarise(n_offices = n_distinct(nearest_facility), .by = c(csdid, csd_name))

#------------------------------------------------------------------------------
# bin the data by driving distance
#------------------------------------------------------------------------------

drive_distance_bins <- drivetime_data |>
  mutate(
    dist_bin = case_when(
      drv_dist < 5 ~ "Under 5 km",
      between(drv_dist, 5, 20) ~ "5 to 20 km",
      TRUE ~ "20+ km"
    )
  ) |>
  summarise(total_count = n(),
    .by = c(csd_name, csdid, dist_bin)) |>
  pivot_wider(
    names_from = dist_bin,
    values_from = total_count,
    values_fill = 0
  )


# =========================================================================== #
# All together ----
# =========================================================================== #

# Combined, assuming we want the full set of CSD's
# If we want to rollup to Unincorporated areas then we can group by region_name/clean_csd in aggregations above
# Since we only have data on 525 of the 751 CSDS  => missing data on 220/423 IRI's, 3/160 RDA's, and 3/3 S-E's
combined_stats <- population_estimates_three_year |>
  left_join(age_estimates_current_year, by = c("csdid", "region_name")) |>
  left_join(addresses_serviced, by = "csdid") |>
  left_join(offices_serviced, by = c("csdid", "csd_name")) |>
  left_join(drive_distance_bins, by = c("csdid", "csd_name")) |>
  relocate(csd_name, .before = region_name) |>
  relocate(n_address, n_offices, .after = `2035`)

# =========================================================================== #
# Write output table to CSV ----
# =========================================================================== #

# Create output directory if it doesn't exist
if (!dir.exists(TABLES_OUT)) {
  dir.create(TABLES_OUT, recursive = TRUE)
}

# Write combined statistics table
write_csv(combined_stats, file.path(TABLES_OUT, "csd_combined_statistics.csv"))

# Print summary of what was written
cat("Combined statistics written to:", file.path(TABLES_OUT, "csd_combined_statistics.csv"), "\n")
cat("Total CSDs in combined statistics:", nrow(combined_stats), "\n")
