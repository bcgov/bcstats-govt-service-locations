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
source("R/fxns/rural-fxns.R")

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

db_shapefiles <- st_read(glue("{SHAPEFILE_OUT}/full-db-with-location.gpkg")) |>
  rename(geometry = geom) |>
  st_transform(crs = 3005) |>
  select(dbid, csdid, geometry)

popcenter_boundaries <-
  st_read(
    glue("{SRC_DATA_FOLDER}/shapefiles/popcenter-statscan.gpkg"),
    layer = "popcenter_statscan"
  ) |>
  rename(geometry = geom) |>
  st_transform(crs = 3005)

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
  read_csv(
    glue("{SRC_DATA_FOLDER}/complete-db-assignments.csv"),
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
db_projections_transformed <- readRDS(glue(
  "{SRC_DATA_FOLDER}/full-db-projections-transformed.rds"
))

# --------------------------------------------------------------------------------
# make drivetime data
# --------------------------------------------------------------------------------

# Join with drivetime data
drivetime_data_full <- complete_assignments |>
  left_join(drivetime_data, by = "dbid") |>
  # fix the daid column to have no NAs
  mutate(daid = str_sub(dbid, 1, 8)) #~ 10,000 extra dbids - TODO: investigate this.

sbc_names <- sbc_locs |>
  distinct(nearest_facility) |>
  #filter (csd_name %in% CSD_NAMES) |> # toggle comment to focus on specific CSD's
  pull(nearest_facility)

# this method is useful when we want to look at specific csd's
# filtering doesn't reduce the data for the full set of assignments.
drivetime_data_focused <- drivetime_data_full |>
  filter(assigned %in% sbc_names) |>
  filter(nearest_facility %in% sbc_names | is.na(nearest_facility)) |>
  left_join(crosswalk, by = c("dbid", "daid"))

## add the current year population estimate to the drivetime data for later use
# as well as estimated household size for an address
drivetime_data_focused <- drivetime_data_focused |>
  left_join(
    db_projections_transformed |>
      filter(gender == "T", year == CURRENT_YEAR) |>
      group_by(dbid, year) |>
      summarize(population = sum(population)) |>
      ungroup() |>
      select(dbid, population),
    by = "dbid"
  ) |>
  group_by(dbid) |>
  mutate(n_address = n()) |>
  ungroup() |>
  mutate(hh_size_estimate = population / n_address)

#------------------------------------------------------------------------------
# bin the data by driving distance
# note: if you check the total pop from here to the estimated pop done below
#       this is slightly lower - I think okay as estimated pop will include dbs
#       that had no addresses in our database, but we include for completeness
#       in the assigned region
#------------------------------------------------------------------------------

drive_distance_bins <- drivetime_data_focused |>
  summarize(
    n_0_5_km = sum(hh_size_estimate[drv_dist < 5.0], na.rm = TRUE),
    n_5_10_km = sum(
      hh_size_estimate[drv_dist >= 5.0 & drv_dist < 10.0],
      na.rm = TRUE
    ),
    n_10_15_km = sum(
      hh_size_estimate[drv_dist >= 10.0 & drv_dist < 15.0],
      na.rm = TRUE
    ),
    n_15_30_km = sum(
      hh_size_estimate[drv_dist >= 15.0 & drv_dist < 30.0],
      na.rm = TRUE
    ),
    n_30_45_km = sum(
      hh_size_estimate[drv_dist >= 30.0 & drv_dist < 45.0],
      na.rm = TRUE
    ),
    n_45_60_km = sum(
      hh_size_estimate[drv_dist >= 45.0 & drv_dist < 60.0],
      na.rm = TRUE
    ),
    n_60_90_km = sum(
      hh_size_estimate[drv_dist >= 60.0 & drv_dist < 90.0],
      na.rm = TRUE
    ),
    n_90_135_km = sum(
      hh_size_estimate[drv_dist >= 90.0 & drv_dist < 135.0],
      na.rm = TRUE
    ),
    n_135_180_km = sum(
      hh_size_estimate[drv_dist >= 135.0 & drv_dist < 180.0],
      na.rm = TRUE
    ),
    n_180_plus_km = sum(hh_size_estimate[drv_dist >= 180.0], na.rm = TRUE),
    .by = c(assigned)
  )

drive_time_bins <- drivetime_data_focused |>
  mutate(drv_time_min = drv_time_sec / 60) |>
  summarize(
    n_within_0_5_min = sum(hh_size_estimate[drv_time_min < 5], na.rm = TRUE),
    n_5_10_min = sum(
      hh_size_estimate[drv_time_min >= 5 & drv_time_min < 10],
      na.rm = TRUE
    ),
    n_10_15_min = sum(
      hh_size_estimate[drv_time_min >= 10 & drv_time_min < 15],
      na.rm = TRUE
    ),
    n_15_20_min = sum(
      hh_size_estimate[drv_time_min >= 15 & drv_time_min < 20],
      na.rm = TRUE
    ),
    n_20_30_min = sum(
      hh_size_estimate[drv_time_min >= 20 & drv_time_min < 30],
      na.rm = TRUE
    ),
    n_30_40_min = sum(
      hh_size_estimate[drv_time_min >= 30 & drv_time_min < 40],
      na.rm = TRUE
    ),
    n_40_60_min = sum(
      hh_size_estimate[drv_time_min >= 40 & drv_time_min < 60],
      na.rm = TRUE
    ),
    n_60_90_min = sum(
      hh_size_estimate[drv_time_min >= 60 & drv_time_min < 90],
      na.rm = TRUE
    ),
    n_90_120_min = sum(
      hh_size_estimate[drv_time_min >= 90 & drv_time_min < 120],
      na.rm = TRUE
    ),
    n_120_150_min = sum(
      hh_size_estimate[drv_time_min >= 120 & drv_time_min < 150],
      na.rm = TRUE
    ),
    n_over_150_min = sum(hh_size_estimate[drv_time_min >= 150], na.rm = TRUE),
    .by = c(assigned)
  )

#------------------------------------------------------------------------------
# drivetime metrics by assigned SBC catchment
#------------------------------------------------------------------------------

drivetime_metrics <- drivetime_data_focused |>
  summarize(
    n_addresses_served = n(),
    mean_driving_distance = mean(drv_dist, na.rm = TRUE),
    median_driving_distance = median(drv_dist, na.rm = TRUE),
    mean_driving_time = mean(drv_time_sec, na.rm = TRUE) / 60,
    median_driving_time = median(drv_time_sec, na.rm = TRUE) / 60,
    n_csds_served = n_distinct(csdid),
    .by = c(assigned)
  )

#------------------------------------------------------------------------------
# DB population projections ----
# ------------------------------------------------------------------------------

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
  )

#------------------------------------------------------------------------------
# count of age groups by assigned
#------------------------------------------------------------------------------
age_estimates_current_year <- population_estimates_three_year_all |>
  filter(year == 2025) |>
  summarize(
    est_population_0_to_14_yrs = sum(
      population[age >= 0 & age < 15],
      na.rm = TRUE
    ),
    est_population_15_to_24_yrs = sum(
      population[age >= 15 & age < 25],
      na.rm = TRUE
    ),
    est_population_25_to_64_yrs = sum(
      population[age >= 25 & age < 65],
      na.rm = TRUE
    ),
    est_population_over_64_yrs = sum(population[age >= 65], na.rm = TRUE),
    .by = c(assigned)
  )

median_population <- population_estimates_three_year_all |>
  filter(year == 2025) |>
  summarize(
    population = sum(population, na.rm = TRUE),
    .by = c(assigned, age)
  ) |>
  summarize(
    median_age = weighted.median(age, population, na.rm = TRUE),
    mean_age = weighted.mean(age, population, na.rm = TRUE),
    .by = c(assigned)
  )

# =========================================================================== #
# Add proportion rural for service bc location
# =========================================================================== #
popcenter_population2 <- is_in_region_optim2(
  db_shapefiles,
  popcenter_boundaries,
  "dbid",
  "pcname"
)

db_population_estimates_one_year <- db_projections_transformed |>
  filter(dbid %in% (crosswalk |> pull(dbid))) |>
  filter(gender == "T", year == 2025) |>
  summarize(
    population = sum(population, na.rm = TRUE),
    .by = c("dbid", "csdid")
  )

# add flags for urban rural and summarize by csdid
rural_summary <- db_population_estimates_one_year |>
  left_join(popcenter_population, by = "dbid") |>
  mutate(urban_rural = if_else(is.na(pcname), "RURAL", "URBAN")) |>
  left_join(complete_assignments, by = "dbid") |>
  summarise(
    n_rural_residents = sum(population[urban_rural == "RURAL"], na.rm = TRUE),
    n = sum(population, na.rm = TRUE),
    p_rural_residents = if_else(n == 0, 0, 100 * n_rural_residents / n),
    is_rural = if_else(p_rural_residents > 50, "RURAL", "URBAN"),
    .by = assigned
  ) |>
  select(assigned, p_rural_residents)

rural_office <- sbc_locs |>
  distinct(nearest_facility, coord_x, coord_y) |>
  st_as_sf(
    coords = c("coord_x", "coord_y"),
    crs = 3005
  ) |>
  is_in_region_optim(
    regions = popcenter_boundaries,
    id_col = "nearest_facility",
    region_name_col = "pcname"
  ) |>
  right_join(sbc_locs |> distinct(nearest_facility), by = "nearest_facility") |>
  mutate(rural_office = if_else(is.na(pcname), "Y", "N"))

# =========================================================================== #
# All together ----
# =========================================================================== #

# Combined, assuming we want the full set of CSD's
# If we want to rollup to Unincorporated areas then we can group by region_name/clean_csd in aggregations above
combined_stats <- population_estimates_three_year |>
  left_join(age_estimates_current_year, by = c("assigned")) |>
  left_join(median_population, by = c("assigned")) |>
  left_join(drivetime_metrics, by = "assigned") |>
  left_join(drive_distance_bins, by = c("assigned")) |>
  left_join(drive_time_bins, by = c("assigned")) |>
  left_join(rural_summary, by = c("assigned")) |>
  left_join(rural_office, by = c("assigned" = "nearest_facility")) |>
  relocate(n_addresses_served, n_csds_served, .after = assigned) |>
  relocate(mean_driving_time, median_driving_time, .after = n_over_150_min) |>
  rename(
    sbc_location = assigned,
    estimated_population_2025 = `2025`,
    `5_yr_projection_2030` = `2030`,
    `10_year_projection_2035` = `2035`
  )

# =========================================================================== #
# Write output table to CSV ----
# =========================================================================== #

# Create output directory if it doesn't exist
if (!dir.exists(TABLES_OUT)) {
  dir.create(TABLES_OUT, recursive = TRUE)
}

# Write combined statistics table
fn <- paste0(
  "sbc-location-statistics-for-SBC-",
  format(Sys.Date(), "%Y-%m-%d"),
  ".csv"
)

combined_stats |>
  arrange(sbc_location) |>
  mutate(across(where(is.double), ~ round(., 1))) |>
  write_csv(file.path(TABLES_OUT, fn))

# Print summary of what was written
cat("SBC location statistics written to:", file.path(TABLES_OUT, fn), "\n")
cat("Total SBC locations in statistics:", nrow(combined_stats), "\n")
