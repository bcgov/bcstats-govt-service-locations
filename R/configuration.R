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
# required libraries are defined here as well.

library(tidyverse)
library(safepaths)
library(glue)
library(janitor)
library(e1071)

library(sf)

# library(cancensus) # nolint
# library(cansim) # nolint

# library(bcmaps) # nolint
# library(geojsonsf) # nolint
# library(jsonlite) imported by bcmaps

# library(cowplot) for aligning multiple plots # nolint
# library(patchwork) # nolint
# library(duckdb) # nolint

# Load the rlang package for the bang-bang operator - imported by cowplot
# library(rlang) # nolint


# set timeout on file load process
getOption("timeout")
options(timeout = 600)

loc_list <- c("909", "227", "213", "420")

lan_folder <- safepaths::use_network_path()
src_data_folder <- glue::glue("{lan_folder}/data/source/")
raw_data_folder <- glue::glue("{lan_folder}/data/raw/")



