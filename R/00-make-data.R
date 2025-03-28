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

source("R/configuration.R") # load libraries and other settings

#------------------------------------------------------------------------------
# geodata team creates drive times files for service bc
# with unique id in place of civic address
# zik files are just zip files, so read_csv can handle them directly.
# currently only service bc locations are needed
# geodata team has removed duplicate rows
# Langford: locality 909
# Smithers: locality 227
# Dawson Creek: locality 213
# Kamploops: Locality 420
#------------------------------------------------------------------------------

# data for service bc with unique id
data_folder <- safepaths::use_network_path()
data_path <- glue::glue("{data_folder}/data/raw/")

# get the most recent file.  This could be used to process all of them
file_path <- file.info(list.files(data_path, full.names = TRUE, pattern = "no_errors.csv", recursive = TRUE)) %>%
  dplyr::arrange(desc(mtime)) %>%
  slice(2) %>%
  rownames()

# get the locality number from the file path
loc <- gsub(glue::glue("({data_path})(.*)(/locality_)([0-9][0-9][0-9])(.*)"), "\\4", file_path) # nolint

out_folder <- glue::glue("{data_folder}/data/source/locality_{loc}")

if (!dir.exists(out_folder)) {
  dir.create(out_folder)
}

new_da_servicebc_df <- read_csv(file_path, col_types = cols(.default = "c"))

address_sf_with_da <- new_da_servicebc_df %>%
  janitor::clean_names() %>%
  filter(tag == "servicebc") %>% # rows for distance to nearest service bc only
  rename(address_albers_x = site_albers_x, address_albers_y = site_albers_y) %>%
  mutate(daid = str_sub(dissemination_block_id, 1, 8),
         drv_time_sec = as.numeric(drv_time_sec),
         drv_dist = as.numeric(drv_dist),
         address_albers_x = as.numeric(address_albers_x),
         address_albers_y = as.numeric(address_albers_y)) %>%
  st_as_sf(coords = c("address_albers_x", "address_albers_y"), crs = 3005)

# drop the address coordinates
address_sf_with_da %>%
  st_drop_geometry() %>%
  write_csv(glue::glue("{out_folder}/address_with_da_loc_{loc}.csv"))
