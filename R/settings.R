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
# Script: settings.R

# Description: This script centralizes project-wide configuration. When sourced,
#  it sets global R options (e.g., timeout) and defines numerous global constants
# used throughout the project for file paths, filenames, expected values,
# data cleaning patterns, required column names, and specific tags.

# Requirements:
#   - Implicit dependency on the `glue` package for constructing paths/patterns.
#   - Implicit dependency on the `safepaths` package .
#   - Relies on the network environment/path accessible via `use_network_path`.

# Side Effects/Outputs:
#   - Modifies the global R environment by setting the `timeout` option.
#   - Defines multiple global variables (constants) in the calling environment
# ------------------------------------------------------------------------

#------------------------------------------------------------------------------
# Set options
#------------------------------------------------------------------------------

# set timeout on file load process
getOption("timeout")
options(timeout = 600)

library(safepaths)
library(glue)

#------------------------------------------------------------------------------
# Constants declaration
#------------------------------------------------------------------------------
EXPECTED_LOCALITIES <- c("909", "227", "213", "420")

# File paths
LAN_FOLDER <- use_network_path()
SRC_DATA_FOLDER <- glue("{LAN_FOLDER}/2025 Government Service Locations/data/source/")
RAW_DATA_FOLDER <- glue("{LAN_FOLDER}/2025 Government Service Locations/data/raw/")
RAW_POP_FILEPATH <- glue("{RAW_DATA_FOLDER}/statscan/98100015-eng/98100015.csv")

# Output filenames
OUTPUT_DB_STATS_FILENAME <- "db_average_times_dist_loc_all.csv"
OUTPUT_DA_STATS_FILENAME <- "da_average_times_dist_loc_all.csv"

# Patterns for cleaning
POP_GUI_PREFIX_PATTERN <- "^2021S[0-9]{4}"
POP_GUI_BC_PATTERN <- glue("{POP_GUI_PREFIX_PATTERN}59")
POP_COL_STRIP_PATTERN1 <- "population_and_dwelling_counts_5_"
POP_COL_STRIP_PATTERN2 <- "_[0-9]$"
LOCALITY_REGEX_PATTERN <- "[0-9][0-9][0-9]"
POP_COL_SELECT_PATTERN <- "symbols"

# File patterns
INPUT_ADDR_DA_PATTERN <- "address_with_da.*"
NO_ERRS_FILE_PATTERN <- "no_errors.csv"

# Columns
REQUIRED_COLS <- c("site_albers_x", "site_albers_y", "dissemination_block_id", "drv_time_sec", "drv_dist", "tag")
FACILITY_TAG <- "servicebc"