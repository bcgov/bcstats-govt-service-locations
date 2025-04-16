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
# Script: 03-sbc-centric-stats.R

# Description: Creates metrics for each individual SBC that was included in the pilot.
# Note that each individual SBC may 'service' more areas than the regions they are assigned to
# So numbers between region averages and SBC averages likely vary. 

# Requirements:
#   - Requires necessary R packages (e.g., `tidyverse`, `purrr`, `glue`).
#   - Depends on `settings.R` for configuration constants.
#   - Depends on the `preprocess_locs` function to perform preprocessing steps.
#   - Requires appropriately named input files in the raw data folder and
#     read/write access to the relevant data folders.

# Side Effects/Outputs:
#   - Writes processed CSV files and plots to the outputs folder.
#   - Prints status messages, warnings (e.g., locality mismatches, overwrites),
#     or errors to the console during execution.
# ------------------------------------------------------------------------

#------------------------------------------------------------------------------
# Load libraries and settings ----
#------------------------------------------------------------------------------
library(tidyverse)
library(glue)
library(janitor)
library(e1071)
library(sf)
library(bcdata)

source("R/settings.R")
source("R/fxns/pre-processing.R")


#------------------------------------------------------------------------------
# Read required data in ----
#------------------------------------------------------------------------------
# drive time data: from BCDS
tmp <- read_csv(RAW_PROVINCE_ADDRESS_FILEPATH)

tmp |> 
  group_by(nearest_facility) |> 
  summarize(
    avg_drv_time_sec = mean(drv_time_sec),
    avg_drv_time_hrs = mean(drv_time_hrs),
    avg_dist = mean(drv_dist),
    median_dist = median(drv_dist),
    min_dist = min(drv_dist),
    max_dist = max(drv_dist),
    address_count = n()
    ) 
  
tmp

# population projections: from catalogue
