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
  # Joins two simple features (SF) objects (`locs` and `regs`) to an initial dataset (`data`)
  # calculates the geometric intersection area between the location and region geometries,
  # and determines the area ratio (the proportion of the location's area that overlaps with the region).

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
      area_ratio = as.numeric(area_int / area_loc)
    )

  # return only the relevent columns. After testing this function, remove geometry cols for performance.
  res <- res |>
    select(all_of(c(
      names(data),
      "area_ratio"
    )))

  return(res)
}


assign_region <- function(
  #This function takes two spatial datasets, locations (the features to be assigned) and
  # regions (the areas to assign them to), and assigns each location to a specific region
  # based on its geospatial relationship (within, intersecting, or exterior/nearest).

  # Location is categorized based on the first condition it satisfies:

  # 1.  Direct Assignment: locations are completely contained within a single region.
  # 2.  Intersection Assignment: Locations intersect with a region of maximum overlap
  #     The area overlap must be above a specified threshold
  # 3.  Nearest Assignment: Exterior locations are assigned to the closest region.
  # The function returns a tibble with columns for the location ID,
  # the assigned region name, the assignment predicate and the area ratio/

  locations,
  regions,
  id_col,
  region_name_col
) {
  cat(glue::glue(
    "Analyzing geospatial relationship between {nrow(locations)} {id_col}'s and {nrow(regions)} {region_name_col}'s... "
  ))
  cat("\n")

  within_cases <- st_join(
    locations,
    regions,
    join = st_within,
    left = FALSE
  ) |>
    st_drop_geometry() |>
    select(all_of(c(id_col, region_name_col)))

  # assign area stats to each location
  within_cases <- within_cases |>
    mutate(area_ratio = 1)

  # Check intersections, for those locations NOT fully contained
  # get a list of ids that are in locations, but not contained_ids
  unprocessed <- locations |>
    anti_join(within_cases, by = id_col)

  cat(glue::glue("Analyzing remaining {nrow(unprocessed)} {id_col}'s..."))
  cat("\n")

  intersect_cases <- st_join(
    unprocessed,
    regions,
    join = st_intersects,
    left = FALSE
  ) |>
    st_drop_geometry() |>
    select(all_of(c(id_col, region_name_col)))

  cat("\t")
  cat(glue::glue("...{nrow(intersect_cases)} intersections found."))
  cat("\n")
  cat(glue::glue(
    "Calculating area stats and maximum overlaps..."
  ))
  cat("\n")

  intersect_cases <- assign_area(
    intersect_cases,
    unprocessed,
    regions,
    id_col,
    region_name_col
  )

  intersect_cases <- intersect_cases |>
    group_by(across(all_of(id_col))) |>
    slice_max(area_ratio, n = 1, with_ties = FALSE) |>
    ungroup()

  cat("\t")
  cat(glue::glue(
    "...intersections for {nrow(intersect_cases)} {id_col}'s assigned."
  ))
  cat("\n")

  # 5. Create similar subset of DB's completely outside of the popcenter region (all the rest)
  exterior_cases <- locations |>
    anti_join(within_cases, by = id_col) |>
    anti_join(intersect_cases, by = id_col)

  cat(glue::glue("Analyzing remaining {nrow(exterior_cases)} {id_col}'s..."))
  cat("\n")
  cat("\t")
  cat(glue::glue(
    "...{nrow(exterior_cases)} {id_col}'s assumed exterior to any {region_name_col}'s."
  ))

  exterior_cases <- exterior_cases |>
    rename(geom_loc = "geometry") |>
    st_join(regions, join = st_nearest_feature, left = FALSE) |>
    select(all_of(c(id_col, region_name_col))) |>
    st_drop_geometry()

  exterior_cases <- exterior_cases |>
    mutate(area_ratio = 0)

  final <- bind_rows(
    within_cases |> mutate(predicate = "within"),
    intersect_cases |> mutate(predicate = "intersects"),
    exterior_cases |> mutate(predicate = "exterior")
  ) |>
    select(all_of(c(id_col, region_name_col, "predicate", "area_ratio")))

  cat(glue::glue("Done...{nrow(final)} {id_col}'s processed."))
  cat("\n")

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
