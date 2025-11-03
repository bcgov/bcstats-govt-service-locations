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

get_fully_contained_cases <- function(locations, regions, id_col, region_name_col) {
  contains_matrix <- sf::st_contains(regions, locations, sparse = FALSE)
  contains_indices <- which(contains_matrix, arr.ind = TRUE)

  if (length(contains_indices) == 0) {
    # No locations fully contained
    res <- data.frame(
      temp_id = character(0),
      temp_region = character(0),
      stringsAsFactors = FALSE
    )
  } else {
    # Create data.frame with proper column names
    res <- data.frame(
      temp_id = locations[[id_col]][contains_indices[, 2]],
      temp_region = regions[[region_name_col]][contains_indices[, 1]],
      stringsAsFactors = FALSE
    )
  }
  # Set correct column names
  names(res) <- c(id_col, region_name_col)

  return(res)
}

get_intersect_cases <- function(locations, regions, id_col, region_name_col) {
  intersects_matrix <- sf::st_intersects(regions, locations, sparse = FALSE)
  intersects_indices <- which(intersects_matrix, arr.ind = TRUE)

  if (length(intersects_indices) == 0) {
    # No intersections found
    res <- data.frame(
      temp_id = character(0),
      temp_region = character(0),
      stringsAsFactors = FALSE
    )
  } else {
    # Create data.frame with proper column names
    res <- data.frame(
      temp_id = locations[[id_col]][intersects_indices[, 2]],
      temp_region = regions[[region_name_col]][intersects_indices[, 1]],
      stringsAsFactors = FALSE
    )
  }
  # Set correct column names
  names(res) <- c(id_col, region_name_col)

  return(res)
}

get_single_intersect_cases <- function(cases, id_col) {

    counts <- table(cases[[id_col]])
    singles <- names(counts)[counts == 1]
    res <- cases[cases[[id_col]] %in% singles, ]

    return(res)
}

get_multiple_intersect_cases <- function(cases, remaining_locations, regions, id_col, region_name_col, area_threshold) {

   intersection_counts <- table(cases[[id_col]])
   multiple_intersections <- names(intersection_counts)[intersection_counts > 1]
  
   # Do we need to handle the case in which there are no multiple intersections?
   #if (length(multiple_intersections) > 0) {
    multiple_candidates <- cases[cases[[id_col]] %in% multiple_intersections, ]

    # Get geometries only for locations/regions involved in multiple intersections
    multi_loc_ids <- unique(multiple_candidates[[id_col]])
    multi_region_names <- unique(multiple_candidates[[region_name_col]])

    loc_geoms <- remaining_locations[remaining_locations[[id_col]] %in% multi_loc_ids, ] |>
      select(all_of(id_col), geometry) |>
      mutate(loc_area = st_area(geometry))

    reg_geoms <- regions[regions[[region_name_col]] %in% multi_region_names, ] |>
      select(all_of(region_name_col), geometry)

    # Calculate intersection areas and select best match
    res <- multiple_candidates |>
      left_join(loc_geoms, by = id_col, suffix = c("", "_loc")) |>
      left_join(reg_geoms, by = region_name_col, suffix = c("", "_reg")) |>
      mutate(
        int_area = map2_dbl(geometry, geometry_reg,
                           ~ st_area(st_intersection(.x, .y))),
        area_ratio = as.numeric(int_area) / as.numeric(loc_area)
      ) |>
      filter(area_ratio > area_threshold) |>
      slice_max(int_area, n = 1, with_ties = FALSE, by = all_of(id_col)) |>
      select(all_of(c(id_col, region_name_col)))
  #}

  return(res)
}

is_in_region_optim <- function(locations, regions, id_col, region_name_col, area_threshold = 0.3) {

  message(sprintf("Processing %d %s's against %d %s regions...",
                  nrow(locations), id_col, nrow(regions), region_name_col))

  # 1: Check for full containment first
  fully_contained_cases <- get_fully_contained_cases(locations, regions, id_col, region_name_col)

  # 2: Check intersections, but only for locations NOT fully contained)
  contained_ids <- unique(fully_contained_cases[[id_col]]) #IDs of fully contained locations
  remaining_locations <- locations[!locations[[id_col]] %in% contained_ids, ]
  # Do we want to return fully contained here, in the event there are no remaining locations?
  # i.e. if (nrow(remaining_locations) == 0) {return(fully_contained)}
  message(sprintf("%d locations fully contained. Checking intersections for remaining %d locations...",
                  length(contained_ids), nrow(remaining_locations)))

  all_intersect_cases <- get_intersect_cases(remaining_locations, regions, id_col, region_name_col)
  # Do we need to return fully contained here, in the event there are no intersecting locations?
  # i.e. if (nrow(intersect_cases) == 0) {return(fully_contained)}

  # 3: Handle different types of intersection cases (single, multiple, none)
  # 3a) Single intersections - these occur when only the boundaries touch so we can accept directly

  single_intersect_cases <- get_single_intersect_cases(all_intersect_cases, id_col)

  # Do we need to handle the case in which there are no single intersections?

  # 3b) Multiple intersections - need area calculations
  
  # Do we need to handle the case in which there are no multiple intersections?
  multiple_intersect_cases <- get_multiple_intersect_cases (all_intersect_cases, remaining_locations, regions, id_col, region_name_col, area_threshold)

  # 4. Combine all results and return
  final_result <- bind_rows(fully_contained_cases, single_intersect_cases, multiple_intersect_cases)

  message(sprintf("%d (%.1f%%) of %s's were matched to %s regions.",
                 nrow(final_result), 100 * nrow(final_result) / nrow(locations), id_col, region_name_col))
  message(sprintf("  - %d from full containment", nrow(fully_contained_cases)))
  message(sprintf("  - %d from intersection analysis", nrow(final_result) - nrow(fully_contained_cases)))

  return(final_result)
}

# --- Create a reusable plotting function for urban/rural maps
plot_urban_rural <- function(data, title = "Rural and Urban Areas - BC") {

  data |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(aes(color = rural), size = 0.5, alpha = 0.1) +
    ggplot2::scale_color_manual(values = c("URBAN" = "#074607", "RURAL" = "#8888f5"), name = "Rural") +
    ggplot2::labs(title = title, subtitle = "Colored by Rural Flag and Method", x = "Longitude", y = "Latitude") +
    ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(shape = 15, size = 5, alpha = 1))) +
    ggplot2::theme_minimal()
}