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

library(tidyverse)
library(glue)
library(janitor)
library(e1071)

source("R/settings.R")
source("R/fxns/calculations.R")

#------------------------------------------------------------------------------
# Read data from source folder
#------------------------------------------------------------------------------

crosswalk <-
  read_csv(glue("{SRC_DATA_FOLDER}/temp/csd-da-db-loc-crosswalk.csv"), col_types = cols(.default = "c")) %>%
  clean_names() %>%
  mutate(across(c(db_pop, db_n_dwelling, db_n_dwelling_resident, db_area), as.numeric))

drivetime_data <-
  read_csv(glue("{SRC_DATA_FOLDER}/temp/processed-drivetime-data.csv"), col_types = cols(.default = "c")) %>%
  clean_names() %>%
  mutate(across(c(drv_time_sec, drv_dist), as.numeric))

drivetime_data <- drivetime_data %>% 
  inner_join(crosswalk, by = c("dbid", "daid", "locid"))

pop_da <- read_csv(glue("{SRC_DATA_FOLDER}/temp/population-da.csv"), col_types = cols(.default = "c")) %>%
  clean_names() %>%
  mutate(across(c(area_sq_km, population, dwellings, households), as.numeric))

pop_db <- read_csv(glue("{SRC_DATA_FOLDER}/temp/population-db.csv"), col_types = cols(.default = "c")) %>%
  clean_names() %>%
  mutate(across(c(area_sq_km, population, dwellings, households), as.numeric))

pop_csd <- read_csv(glue("{SRC_DATA_FOLDER}/temp/population-csd.csv"), col_types = cols(.default = "c")) %>%
  clean_names() %>%
  mutate(across(c(area_sq_km, population, dwellings, households), as.numeric))

#------------------------------------------------------------------------------
# Create CSD, DA and DB-level summary statistics table
#------------------------------------------------------------------------------
drivetime_stats_da <- calculate_drivetime_stats(drivetime_data, group_cols = c("csd_name", "daid"))
drivetime_stats_db <- calculate_drivetime_stats(drivetime_data, group_cols = c("csd_name", "dbid"))
drivetime_stats_csd <- calculate_drivetime_stats(drivetime_data, group_cols = c("csd_name"))

drivetime_stats_da <- drivetime_stats_da %>%
  inner_join(pop_da, by = c("daid"))

drivetime_stats_db <- drivetime_stats_db %>%
  left_join(pop_db, by = c("dbid"))

drivetime_stats_csd <- drivetime_stats_csd %>%
  left_join(pop_csd, by = c("csd_name"))

#------------------------------------------------------------------------------
# Data checks - come back to this as maybe some of these need to be looked at
#------------------------------------------------------------------------------
na_prop <- sum(is.na(drivetime_stats_da$n_address))/ nrow(drivetime_stats_da)
message(glue("({percent(na_prop)}) of NAs in DA map data"))

low_counts_prop <- sum(drivetime_stats_da$n_address < 5) / nrow(drivetime_stats_da)
message(glue("({percent(low_counts_prop)}) of DA regions contain fewer than 5 observations"))

investigate_da <- drivetime_stats_da %>% 
  filter(dwellings > 0 & n_address > 4 & n_address/as.numeric(dwellings) > 0.01)

na_prop <- sum(is.na(drivetime_stats_db$n_address))/ nrow(drivetime_stats_db)
message(glue("({percent(na_prop)}) of NAs in DB map data"))

low_counts_prop <- sum(drivetime_stats_db$n_address < 5) / nrow(drivetime_stats_db)
message(glue("({percent(low_counts_prop)}) of DB regions have fewer than 5 observations"))

investigate_db <- drivetime_stats_db %>% 
  filter(dwellings > 0 & n_address > 4 & n_address/as.numeric(dwellings) > 0.01)

# check the extra da's outside of CSD

#------------------------------------------------------------------------------
# Write descriptive tables to source folder
#------------------------------------------------------------------------------
write_csv(drivetime_stats_da, glue("{SRC_DATA_FOLDER}/temp/da_average_times_dist_all_locs.csv"))
write_csv(drivetime_stats_db, glue("{SRC_DATA_FOLDER}/temp/db_average_times_dist_all_locs.csv"))
write_csv(drivetime_stats_csd, glue("{SRC_DATA_FOLDER}/temp/csd_average_times_dist_all_locs.csv"))

# clean up the environment
rm(list = ls())
gc()
