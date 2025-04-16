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
# Script: 00-make-data.R

# Description: Finds the most recent drive time data file for each locality,
# and loads the data into R. It relies on the `preprocess_locs` function
# to perform preprocessing steps, including filtering for facility records.
# The cleaned data is written back to the source data directory for use in 
# further analytics.

# Requirements:
#   - Requires necessary R packages (e.g., `tidyverse`, `purrr`, `glue`).
#   - Depends on `settings.R` for configuration constants.
#   - Depends on the `preprocess_locs` function to perform preprocessing steps.
#   - Requires appropriately named input files in the raw data folder and
#     read/write access to the relevant data folders.

# Side Effects/Outputs:
#   - Writes processed CSV files (one per locality) to the source data folder.
#   - Prints status messages, warnings (e.g., locality mismatches, overwrites),
#     or errors to the console during execution.
# ------------------------------------------------------------------------

#------------------------------------------------------------------------------
# Load req'd libraries and source constants and other settings
#------------------------------------------------------------------------------
source("R/settings.R")
source("R/fxns/pre-processing.R")

library(tidyverse)
library(glue)
library(janitor)
library(e1071)
library(sf)

#------------------------------------------------------------------------------
# Get the most recent drive time files for each locality
#------------------------------------------------------------------------------
# TODO: Make more robust to handle different file structures and patterns.
file_paths <- file.info(list.files(RAW_DATA_FOLDER,
                                   full.names = TRUE,
                                   pattern = NO_ERRS_FILE_PATTERN,
                                   recursive = TRUE)) %>%
  rownames_to_column("fn") %>%
  mutate(loc = gsub(glue("({RAW_DATA_FOLDER})(.*)(/locality_)({LOCALITY_REGEX_PATTERN})(.*)"), "\\4", fn)) %>%
  group_by(loc) %>%
  arrange(loc, desc(mtime)) %>%
  slice_head(n = 1) %>%
  select(fn, loc)

# Warn if localities are not as expected
missing_localities <- setdiff(EXPECTED_LOCALITIES, unique(file_paths$loc))
extra_localities <- setdiff(unique(file_paths$loc), EXPECTED_LOCALITIES)

if (length(missing_localities) > 0) {
  warning("Expected localities not found: ", paste(missing_localities, collapse = ", "))
}
if (length(extra_localities) > 0) {
  warning("Unexpected localities found: ", paste(extra_localities, collapse = ", "))
}

#------------------------------------------------------------------------------
# Run the preprocessing function to perform data cleaning steps for each file
#------------------------------------------------------------------------------

processed_files <- purrr::walk2(
  .x = file_paths$fn,
  .y = file_paths$loc,
  .f = preprocess_locs,
  output_folder = SRC_DATA_FOLDER,
  reqd_cols = REQUIRED_COLS,
  facility_tag = FACILITY_TAG
)

#------------------------------------------------------------------------------
# Create a crosswalk for daid, duid and locality
#------------------------------------------------------------------------------
crosswalk_list <- purrr::map2(
  .x = file_paths$fn,
  .y = file_paths$loc,
  .f = create_crosswalk
)

crosswalk <- bind_rows(crosswalk_list)
outfile <- glue("{SRC_DATA_FOLDER}/da-db-loc-crosswalk.csv")
tryCatch({
  write_csv(crosswalk, outfile)
}, error = function(e) {
  message(glue("Error writing file {outfile}:  {e$message}"))
})


# clean up the environment
rm(list = ls())
gc()
