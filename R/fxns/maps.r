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
    plot_title = NULL,
    legend_title = NULL,
    map_theme = map_theme, 
    geo_level = "DA",
    fill_scale = scale_fill_viridis_c(option = "viridis"),
    missing_color = "grey80"
) {


  # Handle titles
  default_title <- glue::glue("{varname} for Locality {loc_id} by {geo_level}")
  plot_title <- plot_title %||% default_title # Use custom title if provided
  legend_title <- legend_title %||% varname # Use custom legend title if provided
  varname <- rlang::as_string(rlang::ensym(varname)) # Get varname as string

  ## Build the ggplot object
 data %>%
  filter(location_id == "227") %>%
  ggplot() +
  geom_sf(
    aes(fill = landarea),
    color = "gray50",
    lwd = 0.1
  ) +
  #  fill_scale() + # Apply the chosen fill scale
    labs(
      title = plot_title,
      fill = legend_title, # Set legend title via labs()
      x = "Longitude", # Default axis labels
      y = "Latitude"
    ) +
    map_theme
}