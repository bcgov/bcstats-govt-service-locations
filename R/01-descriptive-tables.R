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

# ------------------------------------------------------------------------
# Script: 01-descriptive-tables.R

# Description: Reads previously processed drive time data files for multiple
# localities, performs data quality checks (type conversion, NA handling,
# negative value removal), calculates summary statistics at the
# Dissemination Block (DB) and Dissemination Area (DA) level.
# Merges DA stats with population data, and writes the final DB and DA summary files.

# Requirements:
#   - Requires R packages: `tidyverse`, `glue`, `janitor`, `e1071`, etc.
#   - Depends on `settings.R` for configuration constants.
#   - Depends on `calculate_drivetime_stats` to calculate various statistics.
#   - Depends on functions `read_all_locs` to read drive time data files.
#   - Requires appropriately named input files in the raw and source data folders
#     and read/write access to the relevant data folders.

# Side Effects/Outputs:
#   - Writes CSV files with DB-level and DA-level summary statistics to source data folder.
#   - Prints status messages, warnings (e.g., data quality issues,
#     overwriting files), or errors to the console.
# ------------------------------------------------------------------------


#------------------------------------------------------------------------------
# Load Reqd Libraries and source constants and other settings
#------------------------------------------------------------------------------

library(tidyverse)
library(safepaths)
library(glue)
library(janitor)
library(e1071)

source("R/settings.R")  
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
# Check if drivetime cols are numeric - convert if not
data <- data %>%
  mutate(across(c(drv_time_sec, drv_dist), as.numeric))

# broadly check for missing data and remove these rows with a warning
# this could be more targeted to specific columns.
nas <- data %>% filter(if_any(everything(), is.na))
if (nrow(nas) > 0) {
  warning("Removing NA's in drive time data.")
  data <- data %>%
    filter(!if_any(everything(), is.na))
}

# Check for negative values in drive time data
invalid <- data %>% filter(if_any(c("drv_time_sec", "drv_dist"), ~ .x < 0))
if (nrow(invalid) > 0) {
  warning("Removing negative values in drive time data.")
  data <- data %>%
    filter(drv_time_sec >= 0, drv_dist >= 0)
}

#------------------------------------------------------------------------------
# Create DA and DB-level summary statistics table
#------------------------------------------------------------------------------
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
# Write output files to source folder
#------------------------------------------------------------------------------
# DB-level stats
outfile <- glue("{SRC_DATA_FOLDER}/{OUTPUT_DB_STATS_FILENAME}")

if (file.exists(outfile)) {
  warning(glue("Overwriting existing file: {outfile}"))
}

drivetime_stats_db %>% 
  write_csv(outfile)

# DA-level stats
outfile <- glue("{SRC_DATA_FOLDER}/{OUTPUT_DA_STATS_FILENAME}")

if (file.exists(outfile)) {
  warning(glue("Overwriting existing file: {outfile}"))
}

drivetime_stats_da %>%
  left_join(pop, by = "daid") %>%
  write_csv(outfile)
