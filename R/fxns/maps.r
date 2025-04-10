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

build_map <- function(
    data,
    varname,
    loc_id,
    loc_col = "loc",
    plot_title = "",
    legend_title = "",
    map_theme = theme_minimal(),
    fill_scale = scale_fill_viridis_c(option = "viridis")
) {

  # --- Prepare dynamic arguments as symbols ---
  varname_sym <- rlang::sym(varname)
  loc_col_sym <- rlang::sym(loc_col)

  # --- Prepare titles as strings --
  default_title <- glue::glue("{varname} for Locality {loc_id}")
  plot_title <- plot_title %||% default_title
  legend_title <- legend_title %||% varname

  map_data <- data %>%
  filter(!!loc_col_sym == loc_id)

  # Check if filtering resulted in data
  if (nrow(map_data) == 0) {
    warning(glue("Warning: No data found for loc_id '{loc_id}'"))
    return(ggplot() + theme_void() + labs(title = glue("No data for {loc_id}")))
  }

  ## Build the ggplot object
  map <-  map_data %>%
    ggplot() +
    geom_sf(
        aes(fill = !!varname_sym),
        color = "gray50",
        lwd = 0.1
    ) +
   fill_scale + 
    labs(
      title = plot_title,
      fill = legend_title,
      x = "\nLongitude",
      y = "Latitude\n"
    ) +
    map_theme

  return(map)

}
