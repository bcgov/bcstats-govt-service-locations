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

# ------------------------------------------------------------------------
# Function: assign_nearest_facility
#
# Description: Assigns each dissemination block (DB) to its nearest facility
# based on drive time data.
#
# Inputs:
#   - drivetime_data: A data frame containing drive time information with
#     columns `dbid` and `nearest_facility`.
#
# Outputs:
#   - Returns a data frame with columns `dbid` and `assigned`, where `assigned`
#     is the nearest facility for each DB.
#
# Assumptions:
#   - Each DB may have multiple rows with different nearest facilities.
#   - The function selects the most frequently occurring facility for each DB.
# ------------------------------------------------------------------------

assign_nearest_facility <- function(drivetime_data) {
  assigned_facility <- drivetime_data |>
    count(dbid, nearest_facility) |>
    arrange(dbid, desc(n)) |>
    group_by(dbid) |>
    slice_head(n = 1) |>
    rename(assigned = nearest_facility) |>
    select(-n) |>
    ungroup()
  
  return(assigned_facility)
}

# ------------------------------------------------------------------------
# Function: assign_unassigned_dbs
#
# Description: Assigns each unassigned dissemination block (DB) directly to the 
# nearest Service BC facility location based on centroid distance.
#
# Inputs:
#   - db_shapefile: An sf object containing all DB geometries with `dbid` column
#   - assigned_facility: Data frame with columns `dbid` and `assigned` for already
#     assigned DBs (from drive time data)
#   - facility_locations: An sf object containing Service BC facility locations
#   - batch_size: Number of unassigned DBs to process in each batch (default 1000)
#   - verbose: Whether to print progress messages (default TRUE)
#
# Outputs:
#   - Returns an updated data frame with assignments for previously unassigned DBs
#
# Assumptions:
#   - db_shapefile must be an sf object with spatial geometries
#   - facility_locations must be an sf object with point geometries and a 
#     'nearest_facility' column identifying each location
# ------------------------------------------------------------------------

assign_unassigned_dbs <- function(db_shapefile, assigned_facility, 
                                 facility_locations,
                                 batch_size = 1000,
                                 verbose = TRUE) {
  # Create distinct datasets for assigned and unassigned DBs
  db_shapefile_with_assignment <- db_shapefile |>
    left_join(assigned_facility, by = "dbid")

  unassigned_dbs <- db_shapefile_with_assignment |>
    filter(is.na(assigned)) |>
    select(-assigned)  # Remove NA assigned column
 
  # If no unassigned DBs, just return the original assignments
  if(nrow(unassigned_dbs) == 0) {
    if(verbose) message("No unassigned DBs found.")
    return(assigned_facility)
  }

  # If no facilities (unlikely), return empty assignments
  if(nrow(facility_locations) == 0) {
    warning("No facility locations provided.")
    return(assigned_facility)
  }

  # make sure that the facility locations are unique
  facility_locations <- facility_locations %>% 
    distinct(nearest_facility, .keep_all = TRUE)
  
  # Report the scale of the task
  if(verbose) {
    message("Processing ", nrow(unassigned_dbs), " unassigned DBs")
    message("Finding nearest facility among ", nrow(facility_locations), " locations")
  }
  
  # Ensure consistent CRS
  if(!identical(st_crs(unassigned_dbs), st_crs(facility_locations))) {
    if(verbose) message("Transforming geometries to ensure consistent CRS...")
    facility_locations <- st_transform(facility_locations, st_crs(unassigned_dbs))
  }
  
  # Calculate centroids for all unassigned DBs
  if(verbose) message("Calculating centroids for unassigned DBs...")
  unassigned_centroids <- st_centroid(unassigned_dbs)
  
  # Prepare result container
  new_assignments <- tibble()
  
  # Process in batches to avoid memory issues
  n_unassigned <- nrow(unassigned_dbs)
  n_batches <- ceiling(n_unassigned / batch_size)
  
  if(verbose && n_batches > 1) {
    message("Processing in ", n_batches, " batches of up to ", batch_size, " DBs each")
  }
  
  for(batch in 1:n_batches) {
    if(verbose) message("Processing batch ", batch, " of ", n_batches)
    
    # Get the current batch
    start_idx <- (batch - 1) * batch_size + 1
    end_idx <- min(batch * batch_size, n_unassigned)
    
    current_batch_centroids <- unassigned_centroids[start_idx:end_idx, ]
    
    # Find nearest facility for each DB in the batch
    batch_assignments <- purrr::map_dfr(1:nrow(current_batch_centroids), function(i) {
      # Find the nearest facility location
      nearest_idx <- st_nearest_feature(current_batch_centroids[i, ], facility_locations)
      
      # Get the distance to the nearest facility
      min_dist <- as.numeric(st_distance(
        current_batch_centroids[i, ], 
        facility_locations[nearest_idx, ], 
        by_element = TRUE
      )[1])
      
      # Get the name of the nearest facility
      facility_name <- facility_locations$nearest_facility[nearest_idx]
      
      # Return new assignment
      tibble(
        dbid = unassigned_dbs$dbid[start_idx + i - 1],
        assigned = facility_name,
        assignment_method = "nearest_facility",
        min_distance = min_dist
      )
    })
    
    # Append batch results to the overall result
    new_assignments <- bind_rows(new_assignments, batch_assignments)
  }
  
  # Combine original assignments with new ones
  complete_assignments <- assigned_facility |>
    mutate(
      assignment_method = "drive_time",
      min_distance = NA_real_
    ) |>
    bind_rows(new_assignments)
  
  return(complete_assignments)
}
