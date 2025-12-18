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
# Load req'd libraries and source constants and other settings
#------------------------------------------------------------------------------

source("R/settings.R")

#------------------------------------------------------------------------------
# Read data from source folder
#------------------------------------------------------------------------------

# Add error handling for file loading
drivetime_file <- glue("{SRC_DATA_FOLDER}/reduced-drivetime-data.csv")

drivetime_data <-
  read_csv(drivetime_file, col_types = cols(.default = "c")) |>
  clean_names() |>
  mutate(across(c(drv_time_sec, drv_dist), as.numeric))

#------------------------------------------------------------------------------
# bin the data by driving distance and add a column for the bin
#------------------------------------------------------------------------------

binned_data <- drivetime_data |>
  mutate(
    bin = case_when(
      drv_dist <= 1 ~ "Under 1 km",
      drv_dist > 1 & drv_dist <= 5 ~ "1 to 5 km",
      drv_dist > 5 & drv_dist <= 10 ~ "5 to 10 km",
      drv_dist > 10 & drv_dist <= 20 ~ "10 to 20 km",
      TRUE ~ "20+ km"
    )
  ) |>
  summarise(total_count = n(), .by = c(csd_name, bin)) |>
  mutate(total_address = sum(total_count), .by = c(csd_name)) |>
  ungroup() |>
  mutate(percent = total_count / total_address)


#------------------------------------------------------------------------------
# Transform data for output
#------------------------------------------------------------------------------

# Create long format in one step
binned_data_long <- binned_data |>
  pivot_longer(
    cols = c(total_count, percent),
    names_to = "metric",
    values_to = "value"
  ) |>
  mutate(metric = factor(metric, levels = c("total_count", "percent")))

# Create wide format for reporting
binned_data_pivot <- binned_data_long |>
  pivot_wider(
    names_from = bin,
    values_from = value,
    values_fill = 0
  ) |>
  arrange(csd_name, metric)

# Check output directory exists
output_file <- glue("{TABLES_OUT}/reduced-csd-binned-drivetime-data.csv")
output_dir <- dirname(output_file)

# save the binned data to a csv file
write_csv(binned_data_pivot, output_file)
