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

  # ------------------------------------------------------------------------
  # Try to read the CSV file.  Check for file read errors, missing data
  # ------------------------------------------------------------------------

  if (!file.exists(fl)) {
    message(glue("Error: File {fl} does not exist."))
    return(NULL)
  }

  tryCatch({
    data <- read_csv(fl, col_types = cols(.default = "c")) %>%
      clean_names()
    }, error = function(e) {
      message(glue("Error reading file {fl}: {e$message}"))
      return(NULL)
  })

  if (!all(reqd_cols %in% colnames(data))) {
    message(glue("Error: Required columns missing in data (locality {loc})"))
    return(NULL)
  }

  # ------------------------------------------------------------------------
  # do light data cleaning and filtering - may wnat to check for missing data
  # ------------------------------------------------------------------------
  data <- data %>%
    filter(tag == facility_tag) %>%
    rename(address_albers_x = site_albers_x,
           address_albers_y = site_albers_y) %>%
    mutate(daid = str_sub(dissemination_block_id, 1, 8))

  # ------------------------------------------------------------------------
  # write output to a csv file
  # ------------------------------------------------------------------------
  outfile <- glue("{output_folder}/address_with_da_locality_{loc}.csv")

  # Check if the file already exists and warn if overwriting
  if (file.exists(outfile)) {
  message(glue("Overwriting existing file for locality {loc}: {outfile}"))
  }

  # Attempt to write the CSV file
  tryCatch({
    write_csv(data, outfile)
  }, error = function(e) {
    message(glue("Error writing file {outfile}:  {e$message}"))
    return(NULL)
  })

  return(TRUE)
}

# ------------------------------------------------------------------------
# Function: read_all_locs
#
# Description: This function reads data from a single CSV file,
# attempts to extract a three-digit locality identifier from the
# filename (expected pattern: "locality_XXX"), and adds this
# identifier as a new  'loc' column to the data frame.
#
# Inputs:
#   - f: The file path (string) to a single input CSV file whose name is
#        expected to contain the pattern "locality_XXX".
#
# Outputs:
#   - Returns a data frame containing the data read from the CSV file,
#     with an added 'loc' column for the three-digit locality ID.
#   - Returns NULL if the locality ID cannot be extracted
#             or if there is an error reading the file.
# 
# Notes: coordinates and geographic unit identifiers are
#  returned as character strings.  Drive time and distance
# are returned as numeric values, no data is explicitly converted.
# ------------------------------------------------------------------------

read_all_locs <- function(fn){

  # ------------------------------------------------------------------------
  # Extract the locality ID from filename and attmept to read the CSV file. 
  # ------------------------------------------------------------------------
  
  match_result <- stringr::str_match(fn, "locality_([0-9]{3})")

  locality_id <- if (!is.na(match_result[1, 1])) {
    match_result[1, 2]
  } else {
    message(glue("Warning: Could not extract locality from filename: '{fn}'. Skipping file."))
    return(NULL)
  }

  data <- tryCatch({
     read_csv(fn, col_types = cols(.default = "c"), show_col_types = FALSE)
  }, error = function(e) {
    message(glue::glue("Error reading file {fn}: {e$message}"))
    return(NULL)
  })

  if (is.null(data)) {
    return(NULL)
  }

  # -------------------------------------------------------------------------------
  # explicitly convert datatypes and check for invalid/missing data
  # ------------------------------------------------------------------------------

  data <- data %>%
    mutate(drv_time_sec = as.numeric(drv_time_sec), 
           drv_dist = as.numeric(drv_dist),
           daid = as.character(daid),
           loc = as.character(locality_id))

  # broadly check for missing data and throw a warning
  nas <- data %>% 
    filter(if_any(everything(), ~ is.na(.x)))

  if (nrow(nas) > 0) {
    warning("Warning: NA's in drive time data.")
  }

  # Check for negative values in drive time data
  invalid <- data %>% 
    filter(if_any(c("drv_time_sec", "drv_dist"), ~ .x < 0))

  if (nrow(invalid) > 0) {
    warning("Warning: negative values in drive time data.")
  }

  return(data)
}
