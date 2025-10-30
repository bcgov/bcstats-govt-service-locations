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


is_in_region <- function(locations, regions, id_col, region_name_col) {

  results <- st_within(locations, regions, sparse = FALSE)

  colnames(results) <- regions[[region_name_col]]
  rownames(results) <- locations[[id_col]]

  # print a message saying how many locations were matched
  n_matched <- sum(rowSums(results) > 0)
  n_multi_matched <- sum(rowSums(results) > 1)

  message(glue("{nrow(results)} {id_col}'s processed."))
  message(glue("{n_matched} ({round(100*n_matched/nrow(results), 1)}%) of {id_col}'s were found within at least one {region_name_col} region.")) # nolint
  message(glue("{n_multi_matched} ({round(100*n_multi_matched/nrow(results), 1)}%) of {id_col}'s were found within more than one {region_name_col} region.")) # nolint

  # collapse the results matrix and join to drive data
  results |>
    as.data.frame() |>
    rownames_to_column(id_col) |>
    pivot_longer(-all_of(id_col), names_to = region_name_col, values_to = "in_region") |>
    filter(in_region) |>
    select(-in_region)

}

is_in_region_optim <- function(locations, regions, id_col, region_name_col, area_threshold = 0.3) {

  # Input validation
  message(sprintf("Processing %d %s's against %d %s regions...", 
                  nrow(locations), id_col, nrow(regions), region_name_col))

  # STEP 1: Find locations that are fully contained within regions
  contains_matrix <- st_contains(regions, locations, sparse = FALSE)
  contains_indices <- which(contains_matrix, arr.ind = TRUE)

  if (length(contains_indices) > 0) {
    # Create data.frame with proper column names
    fully_contained <- data.frame(
      temp_id = locations[[id_col]][contains_indices[, 2]],
      temp_region = regions[[region_name_col]][contains_indices[, 1]],
      stringsAsFactors = FALSE
    )
    # Set correct column names
    names(fully_contained) <- c(id_col, region_name_col)

    # Get IDs of locations that were fully contained
    contained_ids <- unique(fully_contained[[id_col]])
  } else {
    # Create empty data.frame with correct column names
    fully_contained <- data.frame(
      temp_id = character(0),
      temp_region = character(0),
      stringsAsFactors = FALSE
    )
    names(fully_contained) <- c(id_col, region_name_col)
    contained_ids <- character(0)
  }

  # STEP 2: Only check intersections for locations NOT fully contained
  remaining_locations <- locations[!locations[[id_col]] %in% contained_ids, ]

  if (nrow(remaining_locations) == 0) {
    # All locations were fully contained, no intersection checks needed
    message(sprintf("All %d locations were fully contained within regions.",
                    length(contained_ids)))
    return(fully_contained)
  }

  message(sprintf("%d locations fully contained. Checking intersections for remaining %d locations...",
                    length(contained_ids), nrow(remaining_locations)))

  # STEP 3: Check intersections only for remaining locations
  intersects_matrix <- st_intersects(regions, remaining_locations, sparse = FALSE)
  intersects_indices <- which(intersects_matrix, arr.ind = TRUE)

  if (length(intersects_indices) == 0) {
    # No intersections found
    n_total <- nrow(locations)
    n_matched <- length(contained_ids)
    message(sprintf("%d (%.1f%%) of %s's were matched to %s regions.",
                    n_matched, 100 * n_matched / n_total, id_col, region_name_col))
    return(fully_contained)
  }

  # Create intersection candidates
  intersection_candidates <- data.frame(
    temp_id = remaining_locations[[id_col]][intersects_indices[, 2]],
    temp_region = regions[[region_name_col]][intersects_indices[, 1]],
    stringsAsFactors = FALSE
  )
  names(intersection_candidates) <- c(id_col, region_name_col)

  # STEP 4: Handle intersection cases
  # Count how many regions each location intersects with
  intersection_counts <- table(intersection_candidates[[id_col]])
  single_intersections <- names(intersection_counts)[intersection_counts == 1]
  multiple_intersections <- names(intersection_counts)[intersection_counts > 1]

  # Single intersections - accept directly
  single_intersect_cases <- intersection_candidates[intersection_candidates[[id_col]] %in% single_intersections, ]

  # Multiple intersections - need area calculations
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