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
# This script loads csv files containing drive times to nearest service bc
# facility for all addresses within a municipality, for four municipalities.
# The municipalities are defined by "locality id" (more clarification needed)
# and mapped to municipality:
# Langford: locality 909
# Smithers: locality 227
# Dawson Creek: locality 213
# Kamploops: Locality 420
# Each row in the data is identified by a unique id in place of civic address;
# geodata team has removed duplicate rows.

# Basic descriptive statisics are calculated at the dissemination area 
# and dissemination block level. Population statistics from 
# Statistics Canada are appended

source("R/configuration.R") # load libraries and other settings

#------------------------------------------------------------------------------
# load prepared data for service bc with unique id
# Langford: locality 909
# Smithers: locality 227
# Dawson Creek: locality 213
# Kamploops: locality 420
#------------------------------------------------------------------------------

lan_folder <- safepaths::use_network_path()
src_data_folder <- glue::glue("{lan_folder}/data/source/")
raw_data_folder <- glue::glue("{lan_folder}/data/raw/")

loc <- "227" # hard coded for now
data <- read_csv(glue::glue("{src_data_folder}/locality_{loc}/address_with_da_loc_{loc}.csv"))

#------------------------------------------------------------------------------
# Create a DA level summary table: average drive time and distance
# and number of address. 
# TODO:Check no row missing distance value?
#------------------------------------------------------------------------------

avg_dist_drvtime_by_db_service <- data %>%
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

avg_dist_drvtime_by_da_service <- data %>%
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
# Add in population data from Statistics Canada
#------------------------------------------------------------------------------

pop <- read_csv(glue::glue("{raw_data_folder}/statscan/98100015-eng/98100015.csv")) %>% # nolint
  janitor::clean_names() %>%
  select(-c(geo, ref_date, coordinate, starts_with("symbols"))) %>%
  setNames(gsub("population_and_dwelling_counts_5","",names(.))) %>%
  setNames(gsub("_[0-9]$","",names(.))) %>%
  filter(grepl("^2021S", dguid)) %>%
  filter(grepl("^59", daid)) %>%
  mutate(daid = as.numeric(gsub("^2021S[0-9][0-9][0-9][0-9]", "", dguid)))

#------------------------------------------------------------------------------
# write prepared data files to source folder
#------------------------------------------------------------------------------

avg_dist_drvtime_by_db_service %>%
  write_csv(glue::glue("{out_folder}/db_average_times_dist_loc_{loc}.csv"))

avg_dist_drvtime_by_da_service %>%
  left_join(pop, by = "daid") %>%
  write_csv(glue::glue("{out_folder}/da_average_times_dist_loc_{loc}.csv"))
