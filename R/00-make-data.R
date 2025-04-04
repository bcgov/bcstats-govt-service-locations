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

#------------------------------------------------------------------------------
# Description:
# This script finds the most recent drive time data file for each locality
# and loads the data into R.  The data is lightly pre-processed and
# then written to "data/source" for further analytics.

# Context:
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

source("R/configuration.R") # load libraries and other settings

# get the most recent drive time files for each locality
# TODO: Make more robust to handle different file structures and patterns.
file_paths <- file.info(list.files(RAW_DATA_FOLDER,  full.names = TRUE, pattern = NO_ERRS_FILE_PATTERN, recursive = TRUE)) %>% # nolint
  rownames_to_column("fn") %>%
  mutate(loc = gsub(glue("({RAW_DATA_FOLDER})(.*)(/locality_)({LOCALITY_REGEX_PATTERN})(.*)"), "\\4", fn)) %>% # nolint
  group_by(loc) %>%
  arrange(loc, desc(mtime)) %>%
  slice_head(n = 1) %>%
  select(fn, loc)

# add warning if the locs are not the same as the expected list
missing_localities <- setdiff(EXPECTED_LOCALITIES, unique(file_paths$loc))
extra_localities <- setdiff(unique(file_paths$loc), EXPECTED_LOCALITIES)

if (length(missing_localities) > 0) {
  warning("Expected localities not found: ", paste(missing_localities, collapse = ", "))
}
if (length(extra_localities) > 0) {
  warning("Unexpected localities found: ", paste(extra_localities, collapse = ", "))
}

# Run the preprocessing function for each file
processed_files <- purrr::walk2(
  .x = file_paths$fn,
  .y = file_paths$loc,
  .f = preprocess_locs,
  data_folder = RAW_DATA_FOLDER,
  output_folder = SRC_DATA_FOLDER,
  reqd_cols = REQUIRED_COLS,
  facility_tag = FACILITY_TAG
)
