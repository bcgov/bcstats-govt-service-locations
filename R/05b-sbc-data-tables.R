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

drivetime_data <-
  read_csv(
    glue("{SRC_DATA_FOLDER}/full-processed-drivetime-data.csv"),
    col_types = cols(.default = "c")
  ) |>
  clean_names() |>
  mutate(across(c(drv_time_sec, drv_dist), as.numeric))

complete_assignments <- read_csv(
    glue("{SRC_DATA_FOLDER}/complete-db-assignments.csv")
    ) |> 
    mutate(dbid = as.character(dbid))

sbc_locs <- read_csv(SBCLOC_FILEPATH) |>
  filter(csd_name %in% CSD_NAMES)

## crosswalk (entire province - excludes dguid's found in our data)
crosswalk <-
  read_csv(
    glue("{SRC_DATA_FOLDER}/csd-da-db-loc-correspondance.csv"),
    col_types = cols(.default = "c")
  ) |>
  clean_names()

# --------------------------------------------------------------------------------
# make drivetime data   
# --------------------------------------------------------------------------------

# Join with drivetime data
drivetime_data_full <- complete_assignments |> 
  left_join(drivetime_data, by = "dbid")
  drivetime_data_reduced <- drivetime_data_full |> 
    # fix the daid column to have no NAs
    mutate(daid = str_sub(dbid, 1, 8)) |> 
    filter(
    assigned %in%
      (sbc_locs |>
         filter(csd_name %in% CSD_NAMES) |>
         pull(nearest_facility))
    ) |>
    filter(
    nearest_facility %in%
        (sbc_locs |>
            filter(csd_name %in% CSD_NAMES) |>
            pull(nearest_facility)) | is.na(nearest_facility)
    ) |>
    left_join(crosswalk, by = c("dbid", "daid"))

  
#------------------------------------------------------------------------------
# bin the data by driving distance
#------------------------------------------------------------------------------

drivetime_data_full |>
    mutate(
        drv_dist_bin = case_when(
        drv_dist < 5 ~ "<5km",
        drv_dist >= 5 & drv_dist < 20 ~ "5-20km",
        drv_dist >= 20 ~ "20+km"
        )
    )  |>
    summarise(total_count = n(), .by = c(assigned, drv_dist_bin)) |>
  pivot_wider(
    names_from = drv_dist_bin,
    values_from = total_count,
    values_fill = 0
  ) |>
  filter(assigned %in% sbc_locs$nearest_facility) 

#------------------------------------------------------------------------------
# count of addresses by assigned
#------------------------------------------------------------------------------

# drive measures
drivetime_data_full |>
  group_by(assigned) |>
  summarize(
    n_addresses = n(),
    mean_drv_dist = mean(drv_dist, na.rm = TRUE)
  ) |>
  ungroup() |>
  filter(assigned %in% sbc_locs$nearest_facility) 

#------------------------------------------------------------------------------
# DB population projections ----
#------------------------------------------------------------------------------
db_projections_transformed <- readRDS(glue("{SRC_DATA_FOLDER}/full-db-projections-transformed.rds"))

drivetime_data_reduced |>
group_by(assigned) |>
  summarize(n_under_5 = sum(drv_dist < 5.0, na.rm=TRUE), 
            n_5_to_20 = sum(drv_dist >= 5.0 & drv_dist < 20.0, na.rm=TRUE),
            n_20_plus = sum(drv_dist >= 20.0, na.rm=TRUE))

df <- drivetime_data_reduced |>
  distinct(assigned, assignment_method, csdid, dbid) |>
  # expand drivetime data to include all years of interest for each row
    expand_grid(
        tibble(year = rep(unique(db_projections_transformed$year)))
    ) |>
  left_join(
    db_projections_transformed |> 
    filter(gender=='T') |> 
    select(dbid, year, age, population, total, area_sq_km), 
    by = c("dbid", 'year')
    ) |> 
  filter(!is.na(csdid))

df |>
  group_by(assigned, year) |> 
  summarize(pop = sum(population, na.rm=TRUE)) |>
  pivot_wider(names_from = year, values_from = pop, values_fill = 0) 

#------------------------------------------------------------------------------
# count of age groups by assigned
#------------------------------------------------------------------------------

df |> mutate(age_grp = case_when(
    age < 19 ~ "0-19",
    age >= 19 & age < 65 ~ "19-64",
    age >= 65 ~ "65+"
  )) |>
  filter(year == 2025) |>
  group_by(assigned, age_grp) |> 
  summarize(pop = sum(population, na.rm=TRUE)) |>
  pivot_wider(names_from = age_grp, values_from = pop, values_fill = 0)

