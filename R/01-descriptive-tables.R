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
fls <- list.files(SRC_DATA_FOLDER, full.names = TRUE, pattern = INPUT_ADDR_DA_PATTERN, recursive = TRUE)

# map_dfr automatically handles NULLs from read_all_locs
data <- map_dfr(fls, read_all_locs)

if (nrow(data) == 0) {
  stop("No data successfully loaded. Check input files.")
}

#------------------------------------------------------------------------------
# Read in population data from Statistics Canada
#------------------------------------------------------------------------------

pop <- read_csv(glue("{RAW_POP_FILEPATH}"), show_col_types = FALSE) %>%
  clean_names() %>%
  select(-c(geo, ref_date, coordinate, starts_with(POP_COL_SELECT_PATTERN))) %>%
  rename_with(~ str_remove(.x, POP_COL_STRIP_PATTERN1), matches(POP_COL_REMOVE_PATTERN1)) %>%
  rename_with(~ str_remove(.x, POP_COL_STRIP_PATTERN2), matches(POP_COL_REMOVE_PATTERN2)) %>%
  filter(str_detect(dguid, POP_DAID_BC_PATTERN)) %>%
  mutate(daid = as.numeric(str_replace(dguid, POP_DAID_BC_PREFIX_PATTERN, "")))

# Check if pop data frame is valid
if (nrow(pop) == 0) {
  warning("Population data is empty after cleaning.")
}

#------------------------------------------------------------------------------
# Create a DB-level summary statistics table with variables:
#------------------------------------------------------------------------------
# TODO: missing data checks - should this be done in 00-make-data?
out_file <- glue("{SRC_DATA_FOLDER}/{OUTPUT_DB_STATS_FILENAME}")

if (file.exists(out_file)) {
  warning(glue("Overwriting existing file: {output_file}"))
}

calculate_drivetime_stats(data, group_cols = c("loc", "dissemination_block_id")) %>%
  write_csv(out_file)

#------------------------------------------------------------------------------
# Create a DA-level summary statistics table with variables
#------------------------------------------------------------------------------
# TODO: missing data checks - should this be done in 00-make-data?
# TODO: add a check to see if the file already exists and warn if overwriting
out_file <- glue("{SRC_DATA_FOLDER}/{OUTPUT_DA_STATS_FILENAME}")

if (file.exists(out_file)) {
  warning(glue("Overwriting existing file: {output_file}"))
}
calculate_drivetime_stats(data, group_cols = c("loc", "daid")) %>%
  left_join(pop, by = "daid") %>%
  write_csv(out_file)
