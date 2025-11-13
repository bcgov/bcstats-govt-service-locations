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

assign_area <- function(data, locs, regs, id_col, reg_col) {
  # join region and locs geometries, add only the geometry columns.
  # relabel columns to avoid confusion
  res <- data |>
    left_join(
      locs |>
        select(all_of(id_col), geometry) |>
        mutate(area_loc = st_area(geometry)) |>
        rename(geom_loc = geometry),
      by = id_col
    ) |>
    left_join(
      regs |>
        select(all_of(reg_col), geometry) |>
        mutate(area_reg = st_area(geometry)) |>
        rename(geom_reg = geometry),
      by = reg_col
    )

  # calculate intersection area between geoms (location and region) for each observation
  # calculate the 'rurality' of each location: that is, the proportion of area "found' in the region
  res <- res |>
    mutate(
      area_int = map2_dbl(
        geom_loc,
        geom_reg,
        ~ as.numeric(st_area(st_intersection(.x, .y)))
      ),
      area_ratio = area_int / area_loc
    )

  # return only the relevent columns. After testing this function, remove geometry cols for performance.
  res <- res |>
    select(all_of(c(
      names(data),
      "geom_loc",
      "geom_reg",
      "area_loc",
      "area_reg",
      "area_int",
      "area_ratio"
    )))

  return(res)
}


is_in_region_optim <- function(
  locations,
  regions,
  id_col,
  region_name_col,
  area_threshold = 0.3
) {
  fully_contained_cases <- st_join(
    regions,
    locations,
    join = st_contains,
    left = TRUE
  ) |>
    st_drop_geometry() |>
    select(all_of(c(id_col, region_name_col)))

  # assign area stats to each location
  fully_contained_cases <- assign_area(
    fully_contained_cases,
    locations,
    regions,
    id_col,
    region_name_col
  )
  # Check intersections, for those locations NOT fully contained
  # get a list of ids that are in locations, but not contained_ids
  unprocessed_locations <- locations |>
    anti_join(fully_contained_cases, by = id_col)

  intersect_cases <- st_join(
    regions,
    unprocessed_locations,
    join = st_intersects
  ) |>
    st_drop_geometry() |>
    select(all_of(c(id_col, region_name_col)))

  intersect_cases <- assign_area(
    intersect_cases,
    unprocessed_locations,
    regions,
    id_col,
    region_name_col
  )

  intersect_cases <- intersect_cases |>
    group_by(dbid) |>
    slice_max(area_ratio, n = 1, with_ties = FALSE) |>
    ungroup()

  # 5. Create similar subset of DB's completely outside of the popcenter region (all the rest)
  unprocessed_locations <- locations |>
    anti_join(fully_contained_cases, by = id_col) |>
    anti_join(intersect_cases, by = id_col)

  outside_cases <- unprocessed_locations |>
    rename(geom_loc = "geometry") |>
    st_join(regions, join = st_nearest_feature, left = FALSE) |>
    select(all_of(c(id_col, region_name_col))) |>
    st_drop_geometry()

  outside_cases <- assign_area(
    outside_cases,
    unprocessed_locations,
    regions,
    id_col,
    region_name_col
  )

  final <- bind_rows(
    fully_contained_cases |> mutate(predicate = "fully_contained"),
    intersect_cases |> mutate(predicate = "intersects"),
    outside_cases |> mutate(predicate = "exterior")
  )

  return(final)
}

# --- Create a reusable plotting function for urban/rural maps
plot_urban_rural <- function(data, title = "Rural and Urban Areas - BC") {
  data |>
    ggplot2::ggplot() +
    ggplot2::geom_sf(aes(color = rural), size = 0.5, alpha = 0.1) +
    ggplot2::scale_color_manual(
      values = c("URBAN" = "#074607", "RURAL" = "#8888f5"),
      name = "Rural"
    ) +
    ggplot2::labs(
      title = title,
      subtitle = "Colored by Rural Flag and Method",
      x = "Longitude",
      y = "Latitude"
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        override.aes = list(shape = 15, size = 5, alpha = 1)
      )
    ) +
    ggplot2::theme_minimal()
}
