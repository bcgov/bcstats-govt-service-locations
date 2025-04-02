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

source("R/configuration.R") # load libraries and other settings

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

# set paths for input (raw) and output (source) data
data_folder <- safepaths::use_network_path()
data_path <- glue::glue("{data_folder}/data/raw/")

loc_list <- c("909", "227", "213", "420")

# get the most recent files and check there is one per locality.
file_paths <- file.info(list.files(data_path, full.names = TRUE, pattern = "no_errors.csv", recursive = TRUE)) %>%
  rownames_to_column("fn") %>%
  mutate(loc = gsub(glue::glue("({data_path})(.*)(/locality_)([0-9][0-9][0-9])(.*)"), "\\4", fn)) %>%
  group_by(loc) %>%
  dplyr::arrange(desc(mtime)) %>%
  slice(1) %>%
  select(fn, loc)

# TODO: add warning if the locs are not the same as the expected list

# function to process each locality
preprocess_locs <- function(fl, loc, fld = data_folder) {

  out_folder <- glue::glue("{fld}/data/source/locality_{loc}")
  if (!dir.exists(out_folder)) {
    dir.create(out_folder)
  }

  data <- read_csv(fl, col_types = cols(.default = "c")) %>%
    janitor::clean_names()

  #add some data checks in here for colnames
  reqd_cols <- c("site_albers_x", "site_albers_y", "dissemination_block_id",
                 "drv_time_sec", "drv_dist", "tag")

  if (!all(reqd_cols %in% colnames(data))) {
    message(glue::glue("error processing locality {loc}: not all required columns are found in data"))
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
  data %>%
    write_csv(glue::glue("{out_folder}/address_with_da_loc_{loc}.csv"))

}

# process each locality
purrr::map2(.x = file_paths$fn, .y = file_paths$loc, .f = preprocess_locs)