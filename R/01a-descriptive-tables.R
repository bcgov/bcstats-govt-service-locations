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

source("R/settings.R")
source("R/fxns/calculations.R")

#------------------------------------------------------------------------------
# Read data from source folder
#------------------------------------------------------------------------------

drivetime_data <-
  read_csv(glue("{SRC_DATA_FOLDER}/reduced-drivetime-data.csv"), col_types = cols(.default = "c")) %>%
  clean_names() %>%
  mutate(across(c(drv_time_sec, drv_dist), as.numeric))

pop_db <- read_csv(glue("{SRC_DATA_FOLDER}/reduced-population-db.csv"), col_types = cols(.default = "c")) %>%
  clean_names() %>%
  mutate(across(c(area_sq_km, population, dwellings, households), as.numeric))

pop_csd <- read_csv(glue("{SRC_DATA_FOLDER}/reduced-population-csd.csv"), col_types = cols(.default = "c")) %>%
  clean_names() %>%
  mutate(across(c(area_sq_km, population, dwellings, households), as.numeric)) |> 
  select(-csd_name) # this CSD name doesn't match, so join on ids

#------------------------------------------------------------------------------
# Create CSD, DA and DB-level summary statistics table
#------------------------------------------------------------------------------
drivetime_stats_db <- calculate_drivetime_stats(drivetime_data, group_cols = c("csd_name", "csdid", "dbid"))
drivetime_stats_csd <- calculate_drivetime_stats(drivetime_data, group_cols = c("csd_name", "csdid"))

# FLAG TODO: in the case of reduced data, we can use the same csd name.  Double check this
# as csd name from cansensus may not match the csd name in the drivetime data
drivetime_stats_db <- drivetime_stats_db %>%
  left_join(pop_db, by = c("dbid", "csdid", "csd_name"))

drivetime_stats_csd <- drivetime_stats_csd %>%
  left_join(pop_csd, by = c("csdid"))

#------------------------------------------------------------------------------
# Data checks - come back to this as maybe some of these need to be looked at
#------------------------------------------------------------------------------
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
  distinct(csd_name, csdid, nearest_facility, coord_x, coord_y) %>% # keep the coords to guard against multiple locations per label
  group_by(csd_name, csdid, nearest_facility, coord_x, coord_y) %>%
  summarise(n_service_bc = n()) %>%
  ungroup() %>%
  select(-c(coord_x, coord_y))

drivetime_stats_csd  <- drivetime_stats_csd %>%
  left_join(low_counts, by = c("csd_name", "csdid")) %>%
  left_join(servicebc_counts, by = c("csd_name", "csdid"))

#------------------------------------------------------------------------------
# Write descriptive tables to source folder
# Notes: drivetime_stats_db contains descriptive results AND is input in future scripts
#------------------------------------------------------------------------------

write_csv(drivetime_stats_db, glue("{SRC_DATA_FOLDER}/reduced-db-average-times-dist-all-locs.csv"))
write_csv(drivetime_stats_db, glue("{TABLES_OUT}/reduced-db-average-times-dist-all-locs.csv"))
write_csv(drivetime_stats_csd, glue("{TABLES_OUT}/reduced-csd-average-times-dist-all-locs.csv"))
write_csv(low_counts, glue("{TABLES_OUT}/reduced-csd-low-counts.csv"))
write_csv(servicebc_counts, glue("{TABLES_OUT}/reduced-csd-service-bc-counts.csv"))

# clean up the environment
rm(list = ls())
gc()
