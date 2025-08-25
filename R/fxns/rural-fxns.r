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
  n_unmatched <- sum(rowSums(results) == 0)
  n_multi_matched <- sum(rowSums(results) > 1)

  message(glue("{nrow(results)} {id_col}'s processed."))
  message(glue("{n_unmatched} ({round(100*n_unmatched/nrow(results), 1)}%) of {id_col}'s were found within a(n) {region_name_col} region."))
  message(glue("{n_multi_matched} ({round(100*n_multi_matched/nrow(results), 1)}%) of {id_col}'s were found within more than one {region_name_col} region."))

  # collapse the results matrix and join to drive data
  results |>
    as.data.frame() |>
    rownames_to_column(id_col) |>
    pivot_longer(-all_of(id_col), names_to = region_name_col, values_to = "in_region") |>
    filter(in_region) |>
    select(-in_region)

}


# --- Create a reusable plotting function for urban/rural maps
plot_urban_rural <- function(data, title = "Rural and Urban Areas - BC") {

  data |>
    ggplot() +
    geom_sf(aes(color = rural), size = 0.5, alpha = 0.1) +
    scale_color_manual(values = c("URBAN" = "#074607", "RURAL" = "#8888f5"), name = "Rural") +
    labs(title = title,
         subtitle = "Colored by Rural Flag and Method",
         x = "Longitude", y = "Latitude") + 
    guides(color = guide_legend(override.aes = list(shape = 15, size = 5, alpha = 1))) +
    theme_minimal()
}