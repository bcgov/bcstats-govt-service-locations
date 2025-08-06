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
# =============================================== #

crosswalk <- read_csv(glue("{SRC_DATA_FOLDER}/csd-da-db-loc-correspondance.csv"), 
   col_types = cols(.default = "c"))

## census populations - 745 census subdivisions found in our data
pop_db <- read_csv(
  glue("{SRC_DATA_FOLDER}/full-population-db.csv"),
  col_types = cols(.default = "c")
) |>
  clean_names() |>
  mutate(across(c(area_sq_km, population, dwellings, households), as.numeric)) |>
  inner_join(crosswalk, by = join_by(dbid))

drivetime_data <- read_csv(
    glue("{SRC_DATA_FOLDER}/full-processed-drivetime-data.csv"),
    col_types = cols(.default = "c")
) |>
  clean_names() |>
  mutate(across(c(drv_time_sec, drv_dist), as.numeric)) |>
  inner_join (crosswalk, join_by(dbid, daid))

db_projections_transformed_raw <- readRDS(glue("{SRC_DATA_FOLDER}/full-db-projections-transformed.rds")) |> 
  filter(dbid %in% (crosswalk |> pull(dbid))) |> 
  filter(gender == 'T', year %in% c(2025, 2030, 2035)) 

# =========================================================================== #
# DB population projections, current, 5yr, 10yr
# number of addresses in each CSD (from our data)
# number of offices in each CSD (from our data)
# =========================================================================== #

popultion_estimates_three_year <- db_projections_transformed_raw  |>
  summarise(population = sum(population, na.rm = TRUE), .by = c(year, region_name)) |>
  pivot_wider(names_from = year, values_from = population, values_fill = 0)

age_estimates_current_year <- db_projections_transformed_raw |> 
  mutate(age_grp = case_when(
    age < 19 ~ "0-19",
    age >= 19 & age < 65 ~ "19-64",
    age >= 65 ~ "65+"
  )) |>
  summarise(
    population = sum(population, na.rm = TRUE), 
    .by = c(region_name, age_grp)) |>
  pivot_wider(
    names_from = age_grp,
    values_from = population, 
    values_fill = 0)

addresses_serviced <- drivetime_data |>
  summarise(n_address = n(), .by = c(csd_name))

offices_serviced <- drivetime_data |>
  summarise(n_offices = n_distinct(nearest_facility), .by = c(csd_name))

#------------------------------------------------------------------------------
# bin the data by driving distance
#------------------------------------------------------------------------------

drive_distance_bins <- drivetime_data |>
  mutate(
    age_bin = case_when(
      drv_dist < 5 ~ "Under 5 km",
      between(drv_dist, 5, 20) ~ "5 to 20 km",
      TRUE ~ "20+ km"
    )
  ) |>
  summarise(total_count = n(),
    .by = c(csd_name, age_bin)) |>
  pivot_wider(
    names_from = age_bin,
    values_from = total_count,
    values_fill = 0
  )

# =========================================================================== #
# All together ----
# =========================================================================== #

# doesn't work - db populations are based on 191 regions, not the full set of ~750 CSD's
popultion_estimates_three_year |>
  left_join(addresses_serviced, by = "csd_name") |>
  left_join(offices_serviced, by = "csd_name") |>
  left_join(age_estimates_current_year, by = "csd_name") |>
  left_join(drive_distance_bins, by = "csd_name") 
