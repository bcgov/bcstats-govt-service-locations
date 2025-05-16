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

library(tidyverse)
library(glue)
library(janitor)
library(e1071)

source("R/settings.R")

#------------------------------------------------------------------------------
# Read data from source folder
#------------------------------------------------------------------------------

crosswalk <-
  read_csv(glue("{SRC_DATA_FOLDER}/csd-da-db-loc-crosswalk.csv"), col_types = cols(.default = "c")) %>%
  clean_names()

drivetime_data <-
  read_csv(glue("{SRC_DATA_FOLDER}/reduced-drivetime-data.csv"), col_types = cols(.default = "c")) %>%
  clean_names() %>%
  mutate(across(c(drv_time_sec, drv_dist), as.numeric))

# bin the data by driving distance and add a column for the bin
binned_data <- drivetime_data %>%
  mutate(
    bin = case_when(
      drv_dist <= 1 ~ "Under 1 km",
      drv_dist > 1 & drv_dist <= 5 ~ "1 to 5 km",
      drv_dist > 5 & drv_dist <= 10 ~ "5 to 10 km",
      drv_dist > 10 & drv_dist <= 20 ~ "10 to 20 km",
      TRUE ~ "20+ km"
    )
  ) %>%
  summarise(total_count = n(),  .by = c(csd_name, bin)) %>%
  # add a column for the total count of addresses per bin and the total address count
  mutate(total_address = sum(total_count), .by = c(csd_name)) %>%
  # add a column for the percentage of addresses in each bin and cummulative percentage
  mutate(
    percent = total_count / total_address,
    bin = factor(bin, levels = c("Under 1 km", "1 to 5 km", "5 to 10 km", "10 to 20 km", "20+ km"))
  ) %>%
  select(-total_address)

# transform the binned data to a long format
binned_data_long <- binned_data %>%
  pivot_longer(
    cols = c(total_count, percent),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(
    metric = factor(metric, levels = c("total_count", "percent")),
    bin = factor(bin, levels = c("Under 1 km", "1 to 5 km", "5 to 10 km", "10 to 20 km", "20+ km"))
  )

# pivot the binned data so that the bins are the columns and arrange by csd_name
binned_data_pivot <- binned_data_long %>%
  pivot_wider(
    names_from = bin,
    values_from = c(value),
    values_fill = 0
  ) %>%
  select(csd_name, metric, "Under 1 km", "1 to 5 km", "5 to 10 km", "10 to 20 km", "20+ km") %>%
  arrange(csd_name)

# save the binned data to a csv file
write_csv(
  binned_data_pivot,
    glue("{TABLES_OUT}/reduced-csd-binned-drivetime-data.csv"))
