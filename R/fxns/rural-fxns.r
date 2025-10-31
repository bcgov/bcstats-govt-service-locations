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

get_fully_contained <- function(locations, regions, id_col, region_name_col) {
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

get_intersection_candidates <- function(locations, regions, id_col, region_name_col) {
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

is_in_region_optim <- function(locations, regions, id_col, region_name_col, area_threshold = 0.3) {

  # Input validation
  message(sprintf("Processing %d %s's against %d %s regions...",
                  nrow(locations), id_col, nrow(regions), region_name_col))

  # 1: Check for full containment first
  fully_contained <- get_fully_contained(locations, regions, id_col, region_name_col)

  # 2: Check intersections only for locations NOT fully contained
  contained_ids <- unique(fully_contained[[id_col]]) #IDs of fully contained locations
  remaining_locations <- locations[!locations[[id_col]] %in% contained_ids, ]
  # Do we want to return fully contained here, in the event there are no remaining locations?
  # i.e. if (nrow(remaining_locations) == 0) {return(fully_contained)}
  message(sprintf("%d locations fully contained. Checking intersections for remaining %d locations...",
                  length(contained_ids), nrow(remaining_locations)))

  intersection_candidates <- get_intersection_candidates(remaining_locations, regions, id_col, region_name_col)
  # Do we need to return fully contained here, in the event there are no intersecting locations?
  # i.e. if (nrow(intersection_candidates) == 0) {return(fully_contained)}

  # 3: Handle different types of intersection cases (single, multiple, none)
  # Count how many regions each location intersects with
  intersection_counts <- table(intersection_candidates[[id_col]])
  single_intersections <- names(intersection_counts)[intersection_counts == 1]
  multiple_intersections <- names(intersection_counts)[intersection_counts > 1]

  # a) Single intersections - accept directly
  single_intersect_cases <- intersection_candidates[intersection_candidates[[id_col]] %in% single_intersections, ]

  # b) Multiple intersections - need area calculations
  if (length(multiple_intersections) > 0) {
    multiple_candidates <- intersection_candidates[intersection_candidates[[id_col]] %in% multiple_intersections, ]

    # Get geometries only for locations/regions involved in multiple intersections
    multi_loc_ids <- unique(multiple_candidates[[id_col]])
    multi_region_names <- unique(multiple_candidates[[region_name_col]])

    loc_geoms <- remaining_locations[remaining_locations[[id_col]] %in% multi_loc_ids, ] |>
      select(all_of(id_col), geometry) |>
      mutate(loc_area = st_area(geometry))

    reg_geoms <- regions[regions[[region_name_col]] %in% multi_region_names, ] |>
      select(all_of(region_name_col), geometry)

    # Calculate intersection areas and select best match
    resolved_multiple <- multiple_candidates |>
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

    # Combine all results
    final_result <- bind_rows(fully_contained, single_intersect_cases, resolved_multiple)
  } else {
    # No multiple intersections
    final_result <- bind_rows(fully_contained, single_intersect_cases)
  }

  # Final statistics
  n_total <- nrow(locations)
  n_matched <- nrow(final_result)
  n_from_contains <- nrow(fully_contained)
  n_from_intersects <- n_matched - n_from_contains

  message(sprintf("%d (%.1f%%) of %s's were matched to %s regions.",
                  n_matched, 100 * n_matched / n_total, id_col, region_name_col))
  message(sprintf("  - %d from full containment", n_from_contains))
  message(sprintf("  - %d from intersection analysis", n_from_intersects))

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