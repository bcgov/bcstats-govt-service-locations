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


# -------------------------------------------------------------------------------
# explicit checks for datatypes, missing values - could be moved to process_locs?
# ------------------------------------------------------------------------------
data <- data %>%
  mutate(
    drv_time_sec = as.numeric(drv_time_sec),
    drv_dist = as.numeric(drv_dist)
  )

nas <- data %>% filter(if_any(everything(), is.na))
if (nrow(nas) > 0) {
  warning("NA's in drive time data.")
}

invalid <- data %>% filter(if_any(c("drv_time_sec", "drv_dist"), ~ .x < 0))
if (nrow(invalid) > 0) {
  warning("Negative values in drive time data.")
}

data <- data %>%
  filter(drv_time_sec >= 0, drv_dist >= 0)

drivetime_stats_db <- calculate_drivetime_stats(data, group_cols = c("loc", "dissemination_block_id"))
drivetime_stats_da <- calculate_drivetime_stats(data, group_cols = c("loc", "daid"))

#------------------------------------------------------------------------------
# Read in population data from Statistics Canada
#------------------------------------------------------------------------------

pop <- read_csv(glue("{RAW_POP_FILEPATH}"), show_col_types = FALSE) %>%
  clean_names() %>%
  select(-c(geo, ref_date, coordinate, starts_with(POP_COL_SELECT_PATTERN))) %>%
  rename_with(~ str_remove(.x, POP_COL_STRIP_PATTERN1), matches(POP_COL_STRIP_PATTERN1)) %>%
  rename_with(~ str_remove(.x, POP_COL_STRIP_PATTERN2), matches(POP_COL_STRIP_PATTERN2)) %>%
  filter(str_detect(dguid, POP_DAID_BC_PATTERN)) %>%
  mutate(daid = as.numeric(str_replace(dguid, POP_DAID_BC_PREFIX_PATTERN, ""))) %>%
  filter(!is.na(daid))

# Check if pop data frame is valid
if (nrow(pop) == 0) {
  warning("Population data is empty after cleaning.")
}

#------------------------------------------------------------------------------
# Create a DB-level summary statistics table with variables:
#------------------------------------------------------------------------------
# TODO: missing data checks - should this be done in 00-make-data?
outfile <- glue("{SRC_DATA_FOLDER}/{OUTPUT_DB_STATS_FILENAME}")

if (file.exists(outfile)) {
  warning(glue("Overwriting existing file: {outfile}"))
}

drivetime_stats_db %>%
  write_csv(outfile)

#------------------------------------------------------------------------------
# Create a DA-level summary statistics table with variables
#------------------------------------------------------------------------------
# TODO: missing data checks - should this be done in 00-make-data?
outfile <- glue("{SRC_DATA_FOLDER}/{OUTPUT_DA_STATS_FILENAME}")

if (file.exists(outfile)) {
  warning(glue("Overwriting existing file: {outfile}"))
}

drivetime_stats_da %>%
  left_join(pop, by = "daid") %>%
  write_csv(outfile)
