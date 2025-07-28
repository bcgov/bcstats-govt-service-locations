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
source("R/fxns/csd-plots.R")

# =========================================================================== #
# Read required data
# =============================================== #

## census populations
pop_db <- read_csv(
  glue("{SRC_DATA_FOLDER}/reduced-population-db.csv"),
  col_types = cols(.default = "c")
) %>%
  clean_names() %>%
  mutate(across(c(area_sq_km, population, dwellings, households), as.numeric))

drivetime_data <- read_csv(
    glue("{SRC_DATA_FOLDER}/reduced-drivetime-data.csv"),
    col_types = cols(.default = "c")
) %>%
  clean_names() %>%
  mutate(across(c(drv_time_sec, drv_dist), as.numeric))


# =========================================================================== #
# DB population projections, 5yr, 10yr
# =========================================================================== #

db_projections_transformed_raw <- readRDS(glue("{SRC_DATA_FOLDER}/full-db-projections-transformed.rds"))
db_projections_transformed <- db_projections_transformed_raw %>% 
  filter(csdid %in% CSDIDS) # this way brings in estimates from db's that we have no drive time data on.  
  # filter(dbid %in% (pop_db %>% pull(dbid))) # this is more accurate

db_projections_transformed |> filter(gender == 'T') |>
group_by(year, region_name) %>%
  summarise(population = sum(population, na.rm = TRUE)) %>%
  pivot_wider(names_from = year, values_from = population, values_fill = 0)

db_projections_transformed |> filter(gender == 'T', year == 2025) |>
mutate(age_grp = case_when(
    age < 19 ~ "0-19",
    age >= 19 & age < 65 ~ "19-64",
    age >= 65 ~ "65+"
  )) %>%
  group_by(region_name, age_grp) %>%
  summarise(population = sum(population, na.rm = TRUE)) %>%
  pivot_wider(names_from = age_grp, values_from = population, values_fill = 0)

#------------------------------------------------------------------------------
# count of addresses by CSD
#------------------------------------------------------------------------------

drivetime_data %>%
  group_by(csd_name) %>%
  summarise(n_address = n())


#------------------------------------------------------------------------------
# bin the data by driving distance
#------------------------------------------------------------------------------

drivetime_data %>%
  mutate(
    bin = case_when(
      drv_dist < 5 ~ "Under 5 km",
      between(drv_dist, 5, 20) ~ "5 to 20 km",
      TRUE ~ "20+ km"
    )
  ) %>%
  summarise(total_count = n(), .by = c(csd_name, bin)) %>%
  pivot_wider(
    names_from = bin,
    values_from = total_count,
    values_fill = 0
  ) 


