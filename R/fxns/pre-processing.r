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


# function to process each locality
preprocess_locs <- function(fl, loc, output_folder, reqd_cols, facility_tag) {

  data <- read_csv(fl, col_types = cols(.default = "c")) %>%
    clean_names()

  if (!all(reqd_cols %in% colnames(data))) {
    message(glue("error processing locality {loc}: not all required columns are found in data"))
    return(NULL)
  }

  data <- data%>%
    filter(tag == facility_tag) %>% 
    rename(address_albers_x = site_albers_x,
           address_albers_y = site_albers_y) %>%
    mutate(daid = str_sub(dissemination_block_id, 1, 8),
           drv_time_sec = as.numeric(drv_time_sec),
           drv_dist = as.numeric(drv_dist),
           address_albers_x = as.numeric(address_albers_x),
           address_albers_y = as.numeric(address_albers_y))

  # check if the data frame is empty after filtering
  if (nrow(data) == 0) {
    message(glue("WARNING processing locality {loc}: No data rows remaining after filtering for tag '{facility_tag}'. No file written."))
    return(NULL) 
  }

  # write output to a csv file
  output_filename <- glue("{output_folder}/address_with_da_locality_{loc}.csv") # could be passed to function

  if (!dir.exists(output_folder)) {
      message(glue("Creating output directory: {output_folder}"))
      dir.create(output_folder, showWarnings = FALSE, recursive = TRUE)
  }

  # Check if the file already exists and warn if overwriting
  if (file.exists(output_filename)) {
      message(glue("Overwriting existing file for locality {loc}: {output_filename}"))
  } else {
      message(glue("Creating new file for locality {loc}: {output_filename}"))
  }
  
  data %>% 
    write_csv(output_filename)
  
  return(TRUE)

}


read_all_locs <- function(f){
# function reads in a file, f and returns a modified version.
# Locality_id is extracted from the file name and added to the data frame.
  loc <- gsub("(.*locality_)([0-9][0-9][0-9])(.*)", "\\2", f)
  data <- read_csv(f) %>%
    mutate(loc = loc) 
  return(data)
}