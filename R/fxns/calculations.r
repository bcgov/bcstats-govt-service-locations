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
# Function: calculate_drivetime_stats

# Description: Calculates various summary statistics such as mean, variance,
# quantiles, skewness, kurtosis for drive distance time, grouped by specified 
# grouping columns.
#
# Inputs:
#   - df: A data frame with numeric columns `drv_dist`, `drv_time_sec`
#     and an identifier column `fid`.
#   - group_cols: A character vector specifying column names present in `df`
#     to use for grouping the summary calculations.
#
# Outputs:
#   - Returns a data frame containing the calculated statistics
#     for `drv_dist` and `drv_time_sec`, aggregated by specified grouping
#     variables.  Also includes the count of distinct addresses.
# 
# Assumptions:
#   - Each row in the data is identified by a unique id in place of civic address
#   - Geodata team has removed duplicate rows.
# ------------------------------------------------------------------------

calculate_drivetime_stats <- function(df, group_cols) {
  
  # ------------------------------------------------------------------------
  # define drive time statistics to calculate
  # ------------------------------------------------------------------------
  
  numeric_stats <- list(
    mean    = ~ mean(.x, na.rm = TRUE),
    var     = ~ var(.x, na.rm = TRUE),
    kurt    = ~ e1071::kurtosis(.x, na.rm = TRUE, type = 1),
    skew    = ~ e1071::skewness(.x, na.rm = TRUE, type = 1),
    qnt0    = ~ quantile(.x, probs = 0.00, na.rm = TRUE), # or min(.x, na.rm = TRUE)
    qnt25   = ~ quantile(.x, probs = 0.25, na.rm = TRUE),
    qnt50   = ~ quantile(.x, probs = 0.50, na.rm = TRUE), # or median(.x, na.rm = TRUE)
    qnt75   = ~ quantile(.x, probs = 0.75, na.rm = TRUE),
    qnt100  = ~ quantile(.x, probs = 1.00, na.rm = TRUE) # or max(.x, na.rm = TRUE)
  )

  # ------------------------------------------------------------------------
  # aggregate and apply calculations as defined above
  # ------------------------------------------------------------------------
  
  # add time in minutes 
  df <- df %>%
    mutate(drv_time_min = drv_time_sec / 60) # convert to minutes

  data <- df %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      # Apply stats to specified numeric variables
      across(c(drv_dist, drv_time_sec, drv_time_min), numeric_stats, .names = "{col}_{fn}"),
      # add calculations that apply to the whole group
      n_address = n_distinct(fid),
      .groups = "drop")

  return(data)
}
