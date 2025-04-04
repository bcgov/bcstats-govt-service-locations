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

# This script loads raw csv data files containing spatial data for addresses in
# a defined municipality in BC (loc).  The datasets are processed to be ready
# for use for data analytics.

library(tidyverse)
library(safepaths)
library(glue)
library(janitor)
library(e1071)
library(sf)

# set timeout on file load process
getOption("timeout")
options(timeout = 600)

# --- Constants ---
LOC_LIST <- c("909", "227", "213", "420")

LAN_FOLDER <- use_network_path()
SRC_DATA_FOLDER <- glue("{lan_folder}/data/source/")
RAW_DATA_FOLDER <- glue("{lan_folder}/data/raw/")

NO_ERRS_FILE_PATTERN <- "no_errors.csv"
LOCALITY_REGEX_PATTERN <- "[0-9][0-9][0-9]"

REQUIRED_COLS <- c("site_albers_x", "site_albers_y", "dissemination_block_id", "drv_time_sec", "drv_dist", "tag")
FILTER_TAG <- "servicebc"

#------------------------------------------------------------------------------

# This script finds the most recent drive time data file for each locality
# and loads the data into R.  The data is lightly pre-processed and
# then written to "data/source" for further analytics.

# GeoData team has prepared drive times to nearest service bc facility for
# all addresses within a municipality, for four municipalities.
# The municipalities are defined by "locality id" (more clarification needed)
# and mapped to municipality:
# Langford: locality 909
# Smithers: locality 227
# Dawson Creek: locality 213
# Kamploops: Locality 420
# Each row in the data is identified by a unique id in place of civic address;
# geodata team has removed duplicate rows.

# The data is provided as a series of .zik files, partitioned into a subfolder
# in the project directory by date of provision.
# .zik files are extracted manually into same folder ("data/raw").

#------------------------------------------------------------------------------


# get the most recent files and check there is one per locality.
# TODO: The regex below assumes a very specific path structure.
# If the structure varies, it might fail or extract the wrong thing.
file_paths <- file.info(list.files(RAW_DATA_FOLDER,  full.names = TRUE, pattern = NO_ERRS_FILE_PATTERN, recursive = TRUE)) %>% # nolint
  rownames_to_column("fn") %>%
  mutate(loc = gsub(glue("({RAW_DATA_FOLDER})(.*)(/locality_){LOCALITY_REGEX_PATTERN}(.*)"), "\\4", fn)) %>% # nolint
  group_by(loc) %>%
  arrange(loc, desc(mtime)) %>%
  slice_head(n = 1) %>%
  select(fn, loc)

# TODO: add warning if the locs are not the same as the expected list

# function to process each locality
preprocess_locs <- function(fl, loc, fld = data_folder) {

  data <- read_csv(fl, col_types = cols(.default = "c")) %>%
    clean_names()

  #add some data checks in here for colnames
  reqd_cols <- c("site_albers_x", "site_albers_y", "dissemination_block_id",
                 "drv_time_sec", "drv_dist", "tag")

  if (!all(reqd_cols %in% colnames(data))) {
    message(glue("error processing locality {loc}: not all required columns are found in data"))
    return(NULL)
  }

  data <- data%>%
    filter(tag == "servicebc") %>% 
    rename(address_albers_x = site_albers_x,
           address_albers_y = site_albers_y) %>%
    mutate(daid = str_sub(dissemination_block_id, 1, 8),
           drv_time_sec = as.numeric(drv_time_sec),
           drv_dist = as.numeric(drv_dist),
           address_albers_x = as.numeric(address_albers_x),
           address_albers_y = as.numeric(address_albers_y))

  # write to output folder TODO output warning message if overwriting files

  out_folder <- src_data_folder
  if (!dir.exists(out_folder)) {
    dir.create(out_folder)
  }

  data %>%
    write_csv(glue("{out_folder}/address_with_da_locality_{loc}.csv"))

}

# process each locality
map2(.x = file_paths$fn, .y = file_paths$loc, .f = preprocess_locs)