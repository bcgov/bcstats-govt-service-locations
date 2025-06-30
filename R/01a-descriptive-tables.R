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
  read_csv(glue("{SRC_DATA_FOLDER}/csd-da-db-loc-crosswalk.csv"), col_types = cols(.default = "c")) %>%
  clean_names()

drivetime_data <-
  read_csv(glue("{SRC_DATA_FOLDER}/reduced-drivetime-data.csv"), col_types = cols(.default = "c")) %>%
  clean_names() %>%
  mutate(across(c(drv_time_sec, drv_dist), as.numeric))

pop_da <- read_csv(glue("{SRC_DATA_FOLDER}/population-da.csv"), col_types = cols(.default = "c")) %>%
  clean_names() %>%
  mutate(across(c(area_sq_km, population, dwellings, households), as.numeric))

pop_db <- read_csv(glue("{SRC_DATA_FOLDER}/population-db.csv"), col_types = cols(.default = "c")) %>%
  clean_names() %>%
  mutate(across(c(area_sq_km, population, dwellings, households), as.numeric))

pop_csd <- read_csv(glue("{SRC_DATA_FOLDER}/population-csd.csv"), col_types = cols(.default = "c")) %>%
  clean_names() %>%
  mutate(across(c(area_sq_km, population, dwellings, households), as.numeric)) |> 
  select(-csd_name) # this CSD name doesn't match, so join on ids

#------------------------------------------------------------------------------
# Create CSD, DA and DB-level summary statistics table
#------------------------------------------------------------------------------
drivetime_stats_da <- calculate_drivetime_stats(drivetime_data, group_cols = c("csd_name", "csdid", "daid"))
drivetime_stats_db <- calculate_drivetime_stats(drivetime_data, group_cols = c("csd_name", "csdid", "dbid"))
drivetime_stats_csd <- calculate_drivetime_stats(drivetime_data, group_cols = c("csd_name", "csdid"))

drivetime_stats_da <- drivetime_stats_da %>%
  left_join(pop_da, by = c("daid"))

drivetime_stats_db <- drivetime_stats_db %>%
  left_join(pop_db, by = c("dbid"))

drivetime_stats_csd <- drivetime_stats_csd %>%
  left_join(pop_csd, by = c("csdid"))

#------------------------------------------------------------------------------
# Data checks - come back to this as maybe some of these need to be looked at
#------------------------------------------------------------------------------
na_prop <- sum(is.na(drivetime_stats_da$n_address))/ nrow(drivetime_stats_da)
message(glue("({scales::percent(na_prop)}) of NAs in DA map data"))

low_counts_prop <- sum(drivetime_stats_da$n_address < 5) / nrow(drivetime_stats_da)
message(glue("({scales::percent(low_counts_prop)}) of DA regions contain fewer than 5 observations"))

investigate_da <- drivetime_stats_da %>%
  filter(dwellings > 0 & n_address > 4 & n_address/as.numeric(dwellings) > 0.01)

na_prop <- sum(is.na(drivetime_stats_db$n_address))/ nrow(drivetime_stats_db)
message(glue("({scales::percent(na_prop)}) of NAs in DB map data"))

low_counts_prop <- sum(drivetime_stats_db$n_address < 5) / nrow(drivetime_stats_db)
message(glue("({scales::percent(low_counts_prop)}) of DB regions have fewer than 5 observations"))

low_counts <- drivetime_stats_db %>%
  group_by(csd_name, csdid) %>%
  summarise(n_db_blocks = n(),
            n_under_5_addresses = sum(n_address < 5, na.rm = TRUE),
            p_under_5_addresses = scales::percent(ifelse(n_db_blocks == 0, 0, n_under_5_addresses / n_db_blocks),
            accuracy = 0.1,
            trim = TRUE)) %>%
  ungroup()

# calculate the number of service BC locations in each CSD
servicebc_counts <- drivetime_data %>%
  distinct(csd_name, csdid, nearest_facility, coord_x, coord_y) %>% # keep the coords to gaurd against multiple locations per label
  group_by(csd_name, csdid, nearest_facility, coord_x, coord_y) %>% 
  summarise(n_service_bc = n()) %>%
  ungroup() %>%
  select(-c(coord_x, coord_y))


drivetime_stats_csd  <- drivetime_stats_csd %>%
  left_join(low_counts, by = c("csd_name", "csdid")) %>%
  left_join(servicebc_counts, by = c("csd_name", "csdid")) 

#------------------------------------------------------------------------------
# Write descriptive tables to source folder
#------------------------------------------------------------------------------
write_csv(drivetime_stats_da, glue("{SRC_DATA_FOLDER}/reduced_da_average_times_dist_all_locs.csv"))
write_csv(drivetime_stats_db, glue("{SRC_DATA_FOLDER}/reduced_db_average_times_dist_all_locs.csv"))
write_csv(drivetime_stats_csd, glue("{SRC_DATA_FOLDER}/reduced_csd_average_times_dist_all_locs.csv"))
write_csv(drivetime_stats_csd, glue("{TABLES_OUT}/reduced_csd_average_times_dist_all_locs.csv"))
write_csv(low_counts, glue("{TABLES_OUT}/reduced_csd_low_counts.csv"))
write_csv(servicebc_counts, glue("{TABLES_OUT}/reduced_csd_service_bc_counts.csv"))

# clean up the environment
rm(list = ls())
gc()
