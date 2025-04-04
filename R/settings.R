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

# This script is used for defining variables global to the project.

#------------------------------------------------------------------------------
# Set Options
#------------------------------------------------------------------------------

# set timeout on file load process
getOption("timeout")
options(timeout = 600)

#------------------------------------------------------------------------------
# Declare Global Constants
#------------------------------------------------------------------------------
EXPECTED_LOCALITIES <- c("909", "227", "213", "420")

LAN_FOLDER <- use_network_path()
SRC_DATA_FOLDER <- glue("{lan_folder}/data/source/")
RAW_DATA_FOLDER <- glue("{lan_folder}/data/raw/")

NO_ERRS_FILE_PATTERN <- "no_errors.csv"
LOCALITY_REGEX_PATTERN <- "[0-9][0-9][0-9]"

REQUIRED_COLS <- c("site_albers_x", "site_albers_y", "dissemination_block_id", "drv_time_sec", "drv_dist", "tag")
FACILITY_TAG <- "servicebc"

