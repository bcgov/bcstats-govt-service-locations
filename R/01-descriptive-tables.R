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
# Load req'd libraries and source constants and other settings
#------------------------------------------------------------------------------

library(tidyverse)
library(glue)
library(janitor)
library(e1071)

source("R/settings.R")
source("R/fxns/pre-processing.R")
source("R/fxns/calculations.R")

#------------------------------------------------------------------------------
# Read drive time data from source folder
#------------------------------------------------------------------------------
fls <- list.files(SRC_DATA_FOLDER, full.names = TRUE, pattern = INPUT_ADDR_DA_PATTERN, recursive = TRUE)

# map_dfr automatically handles NULLs from read_all_locs
data <- map_dfr(.x = fls, .f = read_all_locs)

if (nrow(data) == 0) {
  stop("No data successfully loaded. Check input files.")
}

#------------------------------------------------------------------------------
# Create DA and DB-level summary statistics table
#------------------------------------------------------------------------------
drivetime_stats_db <- calculate_drivetime_stats(data, group_cols = c("loc", "dissemination_block_id"))
drivetime_stats_da <- calculate_drivetime_stats(data, group_cols = c("loc", "daid"))
drivetime_stats_loc <- calculate_drivetime_stats(data, group_cols = c("loc"))

#------------------------------------------------------------------------------
# Read in population data from Statistics Canada
#------------------------------------------------------------------------------
pop <- read_csv(glue("{RAW_POP_FILEPATH}"), show_col_types = FALSE) %>%
  clean_names() %>%
  select(-c(geo, ref_date, coordinate, starts_with(POP_COL_SELECT_PATTERN))) %>%
  rename_with(~ str_remove(.x, POP_COL_STRIP_PATTERN1), matches(POP_COL_STRIP_PATTERN1)) %>%
  rename_with(~ str_remove(.x, POP_COL_STRIP_PATTERN2), matches(POP_COL_STRIP_PATTERN2)) %>%
  filter(str_detect(dguid, POP_GUI_BC_PATTERN)) %>%
  mutate(daid = str_replace(dguid, POP_GUI_PREFIX_PATTERN, "")) %>%
  filter(!is.na(daid))

# Check if pop data frame is empty after filtering
if (nrow(pop) == 0) {
  warning("Population data is empty after cleaning.")
}

#------------------------------------------------------------------------------
# Write DB-level statistics data to source folder
#------------------------------------------------------------------------------
outfile <- glue("{SRC_DATA_FOLDER}/{OUTPUT_DB_STATS_FILENAME}")

if (file.exists(outfile)) {
  warning(glue("Overwriting existing file: {outfile}"))
}

tryCatch({
  write_csv(drivetime_stats_db, outfile)
}, error = function(e) {
  message(glue("Error writing file {outfile}:  {e$message}"))
})


#------------------------------------------------------------------------------
# Write DB-level statistics data to source folder
#------------------------------------------------------------------------------
outfile <- glue("{SRC_DATA_FOLDER}/{OUTPUT_DA_STATS_FILENAME}")

if (file.exists(outfile)) {
  warning(glue("Overwriting existing file: {outfile}"))
}

drivetime_stats_da <- drivetime_stats_da %>%
  left_join(pop, by = "daid")

tryCatch({
  write_csv(drivetime_stats_da, outfile)
}, error = function(e) {
  message(glue("Error writing file {outfile}:  {e$message}"))
})

# clean up the environment
rm(list = ls())
gc()
