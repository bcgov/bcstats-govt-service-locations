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
# Function: build_map

# Description: Generates a thematic map (ggplot object) from an input spatial
# data frame (`sf` object).  Allows customization of titles, theme, and 
# fill color scale.

# Inputs:
#   - data: An sf data frame containing geometries and associated data,
#   - varname: Character string specifying the name of the column in `data`
#     to use for the map's fill aesthetic.
#   - loc_id: The specific locality which we are mapping. 
#   - loc_col: The name of the column in `data` that contains the locality identifiers. 
#     Defaults to "loc".
#   - plot_title: Optional character string for the main plot title. Defaults
#     to empty string.
#   - legend_title: Optional character string for the fill legend title.
#     Defaults to empty string.
#   - map_theme: Optional ggplot theme object. Defaults to `theme_minimal()`.
#   - fill_scale: Optional ggplot continuous fill scale function. Defaults
#                 to `scale_fill_viridis_c(option = "viridis")`.

# Outputs:
#   - Returns a ggplot object representing the thematic map for the specified
#     locality and variable.
#   - If no data remains after filtering by `loc_id`, returns an empty ggplot
#     object with a title indicating no data, and prints a warning.
# ------------------------------------------------------------------------

build_map <- function(
    data,
    servicebc_data,
    varname,
    csd_name,
    csd_col = "",
    plot_title = "",
    plot_subtitle = "",
    legend_title = "",
    map_theme = theme_minimal(),
    fill_scale = scale_fill_viridis_c(option = "viridis"),
    scale_limits = NULL
) {

  # --- Prepare arguments as symbols ---
  varname_sym <- rlang::sym(varname)
  csd_col_sym <- rlang::sym(csd_col)

  # --- Prepare titles as strings --
  default_title <- glue::glue("{varname} for {csd_name}")
  plot_title <- plot_title %||% default_title
  legend_title <- legend_title %||% varname

  # dynamically set limits
  fill_theme <- fill_scale$clone()
  fill_theme$limits <- range(data[[varname_sym]])
  fill_theme$oob <- scales::squish

  map_data <- map_data <- data[data[[csd_col_sym]] == csd_name,]

  points_data <- servicebc_data[servicebc_data[[csd_col_sym]] == csd_name,]

  # Check if filtering resulted in data
  if (nrow(map_data) == 0) {
    warning(glue("Warning: No data found for loc_id '{loc_id}'"))
    return(ggplot() + theme_void() + labs(title = glue("No data for {csd_name}")))
  }

  ## Build the ggplot object
  map <-  ggplot() +
    geom_sf(
        data = map_data,
        aes(fill = !!varname_sym),
        color = "gray50",
        lwd = 0.1
    ) +
    fill_theme +
    geom_sf(data = points_data,
      aes(shape = "Nearest Service BC Location"),
      fill = 'yellow',
      color = 'black',
      size = 2,
      stroke = 1.1) +
    scale_shape_manual(
      name = NULL,
      values = c("Nearest Service BC Location" = 23) 
    ) +
    guides(
      shape = guide_legend(
        override.aes = list(
          fill = "yellow", 
          size = 4)
      )
    ) +
    labs(
      title = plot_title,
      subtitle = plot_subtitle,
      fill = legend_title,
      x = "\nLongitude",
      y = "Latitude\n"
    ) +
    map_theme

  return(map)

}



# ------------------------------------------------------------------------
# Function: build_boxplot

# Description: Generates a ggplot boxplot from an input data frame.
# Allows customization of titles, theme, x and y variables, and fill color.

# Inputs:
#   - data: A data frame containing data for the boxplot.
#   - x_var: Character string specifying the name of the column in `data`
#     to use for the x-axis aesthetic.
#   - y_var: Character string specifying the name of the column in `data`
#     to use for the y-axis aesthetic.
# Optional Inputs:
#   - plot_title, plot_subtitle, x_title, y_title: Defaults to empty string.
#   - boxplot_theme: Defaults to `theme_minimal()`.
#   - fill_scale: Defaults to a discrete color fill with a viridis pallete.

# Outputs:
#   - Returns a ggplot object representing the boxplot.
# ------------------------------------------------------------------------
build_boxplot <- function(
  data,
  x_var,
  y_var,
  plot_title = "",
  plot_subtitle = "",
  x_title = "",
  y_title = "",
  plot_theme = theme_minimal(),
  fill_scale = scale_fill_viridis_d(option = "viridis") #Discrete Colour function scale
) {

  # --- Prepare arguments as symbols ---
  x_var_sym <- rlang::sym(x_var)
  y_var_sym <- rlang::sym(y_var)

  # --- Prepare titles as strings --
  default_title <- glue::glue("Distribution of {y_var} per {x_var}")
  plot_title <- plot_title %||% default_title
  x_title <- x_title %||% x_var
  y_title <- y_title %||% y_var

  ## Build the ggplot object
  boxplot <- ggplot(data, aes(x = !!x_var_sym, y = !!y_var_sym)) +
    geom_boxplot(
      aes(fill = !!x_var_sym),
      notch = FALSE,
      outlier.shape = NA
    ) +
    geom_jitter(
      width = 0.15,
      height = 0,
      alpha = 0.25,
      size = 0.7,
      color = "grey30"
    ) +
    fill_scale + 
    scale_y_continuous(labels = scales::comma_format()) + 
    labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = x_title,
      y = y_title,
      fill = ""
    ) +
    xlab("") +
    plot_theme

  return(boxplot)
}



# ------------------------------------------------------------------------
# Function: build_violinplot

# Description: Generates a ggplot violin plot from an input data frame.
# Allows customization of titles, theme, x and y variables, and fill color.

# Inputs:
#   - data: A data frame containing data for the violin plot.
#   - x_var: Character string specifying the name of the column in `data`
#     to use for the x-axis aesthetic.
#   - y_var: Character string specifying the name of the column in `data`
#     to use for the y-axis aesthetic.
# Optional Inputs:
#   - plot_title, plot_subtitle, x_title, y_title: Defaults to empty string.
#   - violinplot_theme: Defaults to `theme_minimal()`.
#   - fill_scale: Defaults to a discrete color fill with a mako pallete.

# Outputs:
#   - Returns a ggplot object representing the violin plot.

# ------------------------------------------------------------------------
build_violinplot <- function(
  data,
  x_var,
  y_var,
  plot_title = "",
  plot_subtitle = "",
  x_title = "",
  y_title = "",
  plot_theme = theme_minimal(),
  fill_scale = scale_fill_viridis_d(option = "mako")
) {

  # --- Prepare arguments as symbols ---
  x_var_sym <- rlang::sym(x_var)
  y_var_sym <- rlang::sym(y_var)

  # --- Prepare titles as strings --
  default_title <- glue::glue("Distribution of {y_var} per {x_var}")
  plot_title <- plot_title %||% default_title
  x_title <- x_title %||% x_var
  y_title <- y_title %||% y_var

  ## Build the ggplot object
  violinplot <- ggplot(data, aes(x = !!x_var_sym, y = !!y_var_sym)) +
    geom_violin(aes(fill = !!x_var_sym), trim = FALSE, alpha = 0.7) +
    fill_scale +
        geom_jitter(
      width = 0.15,
      height = 0,
      alpha = 0.25,
      size = 0.7,
      color = "grey30"
    ) +
    scale_y_continuous(labels = scales::comma_format()) +
    labs(
      title = plot_title,
      subtitle = plot_subtitle,
      x = x_title,
      y = y_title,
      fill = ""
    ) +
    xlab("") +
    plot_theme

  return(violinplot)
}
