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

#------------------------------------------------------------------------------
# Load Reqd Libraries
#------------------------------------------------------------------------------

library(tidyverse)
library(safepaths)
library(glue)
library(janitor)
library(e1071)
library(sf)

source("R/settings.R")  # load constants and other settings (including temporary placement of library calls)


fls <- list.files(src_data_folder, full.names = TRUE, pattern = "address_with_da.*", recursive = TRUE)

read_all_locs(fls)

data <- bind_rows(lapply(fls, read_all))

avg_dist_drvtime_by_db_service <- data %>%
  group_by(dissemination_block_id, daid, loc) %>% # every daid belongs to a single locality
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

  # TODO: add a check to see if the file already exists and warn if overwriting
avg_dist_drvtime_by_db_service %>%
  write_csv(glue("{src_data_folder}/db_average_times_dist_loc_all.csv"))

#------------------------------------------------------------------------------
# Create a DA-level summary table with variables:
# average drive time
# distance
# number of addresses
# quartiles, etc.
# TODO: missing data checks - should this be done in 00-make-data?
#------------------------------------------------------------------------------

avg_dist_drvtime_by_da_service <- data %>%
  group_by(daid, loc) %>% # every daid belongs to a single locality
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

pop <- read_csv(glue("{raw_data_folder}/statscan/98100015-eng/98100015.csv")) %>% # nolint
  clean_names() %>%
  select(-c(geo, ref_date, coordinate, starts_with("symbols"))) %>%
  setNames(gsub("population_and_dwelling_counts_5","",names(.))) %>%
  setNames(gsub("_[0-9]$","",names(.))) %>%
  mutate(daid = as.numeric(gsub("^2021S[0-9][0-9][0-9][0-9]", "", dguid))) %>%
  #FIXME: logic introduced NA's which are removed in the next subsequent step.
  filter(grepl("^2021S", dguid)) %>%
  filter(grepl("^59", daid))

#------------------------------------------------------------------------------
# write prepared data files to source folder
#------------------------------------------------------------------------------
# TODO: add a check to see if the file already exists and warn if overwriting
avg_dist_drvtime_by_da_service %>%
  left_join(pop, by = "daid") %>%
  write_csv(glue("{src_data_folder}/da_average_times_dist_loc_all.csv"))