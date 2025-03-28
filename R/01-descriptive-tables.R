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

source("configuration.R") # load libraries and other settings

#------------------------------------------------------------------------------
# load prepared data for service bc with unique id
# Langford: locality 909
# Smithers: locality 227
# Dawson Creek: locality 213
# Kamploops: locality 420
#------------------------------------------------------------------------------

data_folder <- safepaths::use_network_path()
loc <- "213" # Dawson Creek: locality 213
outfolder <- glue::glue("{data_folder}/data/processed/locality_{loc}")

#------------------------------------------------------------------------------
# Create a DA level summary table: average drive time and distance
# and number of address. No row missing distance value
# all the addresses and DA information are from geodata team by sampling,
# therefore not full picture.
# To do: is this the full sample of data?
#------------------------------------------------------------------------------

avg_dist_drvtime_by_db_service <- address_sf_with_da %>%
  group_by(dissemination_block_id, daid) %>%
  summarise(
    mn_drv_time_sec = mean(drv_time_sec, na.rm = TRUE),
    mn_drv_dist = mean(drv_dist, na.rm = TRUE),
    qnt0_drv_time_sec = quantile(drv_time_sec, probs = 0, na.rm = TRUE),
    qnt1_drv_time_sec = quantile(drv_time_sec, probs = 0.25, na.rm = TRUE),
    qnt2_drv_time_sec = quantile(drv_time_sec, probs = 0.5, na.rm = TRUE),
    qnt3_drv_time_sec = quantile(drv_time_sec, probs = 0.75, na.rm = TRUE),
    qnt4_drv_time_sec = quantile(drv_time_sec, probs = 1, na.rm = TRUE),
    qnt0_drv_dist = quantile(drv_dist, probs = 0, na.rm = TRUE),
    qnt1_drv_dist = quantile(drv_dist, probs = 0.25, na.rm = TRUE),
    qnt2_drv_dist = quantile(drv_dist, probs = 0.5, na.rm = TRUE),
    qnt3_drv_dist = quantile(drv_dist, probs = 0.75, na.rm = TRUE),
    qnt4_drv_dist = quantile(drv_dist, probs = 1, na.rm = TRUE),
    var_drv_time_sec = var(drv_time_sec, na.rm = TRUE),
    var_drv_dist = var(drv_dist, na.rm = TRUE),
    skw_drv_time_sec = skewness(drv_time_sec, na.rm = TRUE, type = 1),
    skw_drv_dist = skewness(drv_dist, na.rm = TRUE, type = 1),
    kurt_drv_time_sec = kurtosis(drv_time_sec, na.rm = TRUE, type = 1),
    kurt_drv_dist = kurtosis(drv_dist, na.rm = TRUE, type = 1),
    n_address = n_distinct(fid)
  ) %>%
  ungroup()

avg_dist_drvtime_by_da_service <- address_sf_with_da %>%
  group_by(daid) %>%
  summarise(
    mn_drv_time_sec = mean(drv_time_sec, na.rm = TRUE),
    mn_drv_dist = mean(drv_dist, na.rm = TRUE),
    qnt0_drv_time_sec = quantile(drv_time_sec, probs = 0, na.rm = TRUE),
    qnt1_drv_time_sec = quantile(drv_time_sec, probs = 0.25, na.rm = TRUE),
    qnt2_drv_time_sec = quantile(drv_time_sec, probs = 0.5, na.rm = TRUE),
    qnt3_drv_time_sec = quantile(drv_time_sec, probs = 0.75, na.rm = TRUE),
    qnt4_drv_time_sec = quantile(drv_time_sec, probs = 1, na.rm = TRUE),
    qnt0_drv_dist = quantile(drv_dist, probs = 0, na.rm = TRUE),
    qnt1_drv_dist = quantile(drv_dist, probs = 0.25, na.rm = TRUE),
    qnt2_drv_dist = quantile(drv_dist, probs = 0.5, na.rm = TRUE),
    qnt3_drv_dist = quantile(drv_dist, probs = 0.75, na.rm = TRUE),
    qnt4_drv_dist = quantile(drv_dist, probs = 1, na.rm = TRUE),
    var_drv_time_sec = var(drv_time_sec, na.rm = TRUE),
    var_drv_dist = var(drv_dist, na.rm = TRUE),
    skw_drv_time_sec = skewness(drv_time_sec, na.rm = TRUE, type = 1),
    skw_drv_dist = skewness(drv_dist, na.rm = TRUE, type = 1),
    kurt_drv_time_sec = kurtosis(drv_time_sec, na.rm = TRUE, type = 1),
    kurt_drv_dist = kurtosis(drv_dist, na.rm = TRUE, type = 1),
    n_address = n_distinct(fid)
  ) %>%
  ungroup()

#------------------------------------------------------------------------------
# Add in population data
#------------------------------------------------------------------------------


avg_dist_drvtime_by_db_service %>%
  write_csv(glue::glue("{outfolder}/db_average_times_dist_loc_{loc}.csv"))

avg_dist_drvtime_by_da_service %>%
  write_csv(glue::glue("{outfolder}/da_average_times_dist_loc_{loc}.csv"))
