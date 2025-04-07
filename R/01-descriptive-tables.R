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
source("R/fxns/calculate-stats.R") 
source("R/fxns/pre-processing.R") 


#------------------------------------------------------------------------------
# Read drive time data from source folder
#------------------------------------------------------------------------------
fls <- list.files(SRC_DATA_FOLDER, full.names = TRUE, pattern = "address_with_da.*", recursive = TRUE)

# map_dfr automatically handles NULLs from read_all_locs
data <- map_dfr(fls, read_all_locs)

if (nrow(data) == 0) {
  stop("No data successfully loaded. Check input files.")
}

#------------------------------------------------------------------------------
# Read in population data from Statistics Canada
#------------------------------------------------------------------------------

pop <- read_csv(glue("{raw_data_folder}/statscan/98100015-eng/98100015.csv")) %>% # nolint
  clean_names() %>%
  select(-c(geo, ref_date, coordinate, starts_with("symbols"))) %>%
  setNames(gsub("population_and_dwelling_counts_5", "", names(.))) %>%
  setNames(gsub("_[0-9]$", "", names(.))) %>%
  mutate(daid = as.numeric(gsub("^2021S[0-9][0-9][0-9][0-9]", "", dguid))) %>%
  #FIXME: logic introduced NA's which are removed in the next subsequent step.
  filter(grepl("^2021S", dguid)) %>%
  filter(grepl("^59", daid))

#------------------------------------------------------------------------------
# Create a DB-level summary table with variables:
# average drive time
# distance
# number of addresses
# quartiles, etc.
# TODO: missing data checks - should this be done in 00-make-data?
#------------------------------------------------------------------------------
# TODO: add a check to see if the file already exists and warn if overwriting
calculate_drivetime_stats(data, group_cols = c("loc", "dissemination_block_id")) %>%
  write_csv(glue("{src_data_folder}/drivetime_stats_by_loc_db.csv"))

#------------------------------------------------------------------------------
# Create a DA-level summary table with variables:
# average drive time
# distance
# number of addresses
# quartiles, etc.
# TODO: missing data checks - should this be done in 00-make-data?
#------------------------------------------------------------------------------
# TODO: add a check to see if the file already exists and warn if overwriting
calculate_drivetime_stats(data, group_cols = c("loc", "daid")) %>%
  left_join(pop, by = "daid") %>%
  write_csv(glue("{src_data_folder}/drivetime_stats_by_loc_da.csv"))



