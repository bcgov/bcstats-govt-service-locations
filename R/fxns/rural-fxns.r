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
  # Returns an empty, initialized dataframe if there are no fully contained locations.
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
  # Returns an empty, initialized dataframe if there are no intersecting locations.
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

assign_area <- function(data, locs, regs, id_col, reg_col) {
  # join region and locs geometries, add only the geometry columns.
  # relabel columns to avoid confusion
  res <- data |>
    left_join(locs |>
        select(all_of(id_col), geometry) |>
        mutate(area_loc = st_area(geometry)) |>
        rename(geom_loc = geometry)
      , by = id_col) |>
    left_join(regs |>
        select(all_of(reg_col), geometry) |>
        mutate(area_reg = st_area(geometry)) |>
        rename(geom_reg = geometry)
      , by = reg_col)

  # calculate intersection area between geoms (location and region) for each observation
  # calculate the 'rurality' of each location: that is, the proportion of area "found' in the region
  res <- res |>
    mutate(
      area_int = map2_dbl(geom_loc, geom_reg, ~ as.numeric(st_area(st_intersection(.x, .y)))),
      area_ratio = area_int / area_loc
    )

  # return only the relevent columns. After testing this function, we can remove geometry cols and optimize performance.
  res <- res |>
    select(all_of(c(names(data), "geom_loc", "geom_reg", "area_loc", "area_reg", "area_int", "area_ratio")))

  return(res)
}

is_in_region_optim <- function(locations, regions, id_col, region_name_col, area_threshold = 0.3) {
  # Returns an empty, initialized dataframe if there are no contained or intersecting locations.

  message(sprintf("Processing %d %s's against %d %s regions...",
                  nrow(locations), id_col, nrow(regions), region_name_col))

  # 1: Check for full containment first; done in a seperate step thus increasing efficiency substantially
  fully_contained_cases <- get_fully_contained_cases(locations, regions, id_col, region_name_col)

  # 2: Check intersections, but only for locations NOT fully contained
  # IDs of fully contained locations, or empty character string if none
  contained_ids <- unique(fully_contained_cases[[id_col]])
  remaining_locations <- locations[!locations[[id_col]] %in% contained_ids, ]

  message(sprintf("%d locations fully contained. Checking intersections for remaining %d locations...",
                  length(contained_ids), nrow(remaining_locations)))

  intersect_cases <- get_intersect_cases(remaining_locations, regions, id_col, region_name_col)
  intersect_cases <- assign_area(intersect_cases, remaining_locations, regions, id_col, region_name_col)
  intersect_cases <- intersect_cases |>
    group_by(dbid) |>
    slice_max(area_ratio, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select (region_name_col, id_col)

  # 4. Combine all results and return
  final_result <- bind_rows(fully_contained_cases, intersect_cases)

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