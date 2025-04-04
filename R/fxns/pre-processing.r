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
# Function: preprocess_locs

# Description: This function processes raw data for a specific locality,
# validates that required columns are present, and performs preprocessing steps.
# It writes the processed data to a new CSV file in a specified output directory
# named using the locality identifier.

# Inputs:
#   - The file path to the input CSV data for the locality.
#   - An identifier for the locality being processed.
#   - The path to the directory where the processed output file should be saved.
#   - A vector of required column names that must be present in the input file.
#   - The specific tag value used to filter for facility records.

# Outputs:
#   - Writes a CSV file containing the processed data (side effect).
#   - Returns TRUE if the input file was successfully read, processed,
#     and the output file written;
#   - Returns NULL if required columns were missing or if there was an error
#     writing the output file.
# ------------------------------------------------------------------------

preprocess_locs <- function(fl, loc, output_folder, reqd_cols, facility_tag) {

  data <- read_csv(fl, col_types = cols(.default = "c")) %>%
    clean_names()

  if (!all(reqd_cols %in% colnames(data))) {
    message(glue("Error: Required columns missing in data (locality {loc})"))
    return(NULL)
  }

  data <- data%>%
    filter(tag == FACILITY_TAG) %>% 
    rename(address_albers_x = site_albers_x,
           address_albers_y = site_albers_y) %>%
    mutate(daid = str_sub(dissemination_block_id, 1, 8),
           drv_time_sec = as.numeric(drv_time_sec),
           drv_dist = as.numeric(drv_dist),
           address_albers_x = as.numeric(address_albers_x),
           address_albers_y = as.numeric(address_albers_y))


  # write output to a csv file
  output_filename <- glue("{output_folder}/address_with_da_locality_{loc}.csv")

  # Check if the file already exists and warn if overwriting
  if (file.exists(output_filename)) {
  message(glue::glue("Overwriting existing file for locality {loc}: {output_filename}")) # nolint
  }

  # Attempt to write the CSV file
  tryCatch({
    write_csv(data, output_filename)
  }, error = function(e) {
    message(glue("Error: {e$message}"))
    return(NULL)
  })

  return(TRUE)
}


read_all_locs <- function(f){

  # Extract the locality ID from the filename using regex
  match_result <- stringr::str_match(f, "locality_([0-9]{3})")

  # Extract the locality ID if the pattern matched
  locality_id <- if (!is.na(match_result[1, 1])) {
    match_result[1, 2]
  } else {
    message(glue("Warning: Could not extract locality from filename: '{f}'. Skipping file."))
    return(NULL)
  }

  # Attempt to read the CSV file with error handling
  data <- tryCatch({
    f %>% read_csv(col_types = cols(.default = "c"), show_col_types = FALSE)
  }, error = function(e) {
    message(glue::glue("ERROR reading file: '{f}'. Error: {e$message}"))
    return(NULL)
  })

  if (is.null(data)) {
    return(NULL)
  }

  data <- data %>%
    dplyr::mutate(loc = locality_id)

  return(data)
}
