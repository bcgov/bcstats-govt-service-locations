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

# Function to create population pyramids
create_population_pyramid <- function(data, location_name, years = c(CURRENT_YEAR, CURRENT_YEAR + 5, CURRENT_YEAR + 10)) {
  # Define the correct ordering for age groups
  age_group_order <- c(
    "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
    "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74",
    "75-79", "80-84", "85-89", "90+"
  )
  
  # Filter to only include Males and Females (not totals)
  # Group and calculate population estimates by age group and gender
  pop_by_age_gender <- data |>
    filter(
      assigned == location_name,
      gender %in% c("M", "F")
    ) |>
    # Group by the relevant dimensions
    group_by(assigned, gender, age_group, year) |>
    summarize(
      pop_estimate = sum(population, na.rm = TRUE),
      .groups = "drop"
    ) |>
    # Use the custom order for age groups
    mutate(
      age_group = factor(age_group, levels = age_group_order),
      # Make male population negative for visualization
      pop_estimate = ifelse(gender == "M", -pop_estimate, pop_estimate),
      # Format year for display
      year_label = as.character(year)
    )
  
  # Get the unique years in proper order
  years_ordered <- sort(unique(pop_by_age_gender$year))
  
  # Calculate total population for each year for annotations
  total_pop_by_year <- pop_by_age_gender |>
    group_by(year) |>
    summarize(total_pop = sum(abs(pop_estimate)), .groups = "drop")
  
  # Create named vectors for scales - skip the first year (base year) for lines
  projection_years <- years_ordered[-1]
  
  # Only need two colors for projection years
  year_colors <- c("#7a0177", "#006d2c")[1:length(projection_years)]
  names(year_colors) <- as.character(projection_years)
  
  year_linetypes <- c("dashed", "dotted")[1:length(projection_years)]
  names(year_linetypes) <- as.character(projection_years)
  
  # Determine y-axis range for annotation positioning
  y_min <- min(pop_by_age_gender$pop_estimate)
  y_max <- max(pop_by_age_gender$pop_estimate)
  
  # Create annotation text for total population - vertically stacked in top right
  # First, create a summary label for all years
  pop_summary_label <- "Total population:\n"
  for (i in seq_along(years_ordered)) {
    yr <- years_ordered[i]
    pop <- total_pop_by_year |> filter(year == yr) |> pull(total_pop)
    pop_summary_label <- paste0(
      pop_summary_label, 
      yr, ": ", format(round(pop), big.mark = ","),
      if(i < length(years_ordered)) "\n" else ""
    )
  }
  
  # Calculate numeric positions for step charts (between age groups)
  age_numeric_positions <- seq_along(age_group_order)
  
  # Generate projection data in the right format for step charts
  projection_data <- pop_by_age_gender |>
    filter(year != min(years_ordered)) |>
    mutate(age_numeric = as.numeric(age_group))
  
  # Create expanded data for the edge bins to ensure lines extend to the boundaries
  # For each gender and year combination, add points that extend the first and last bins
  first_bin_extension <- projection_data |>
    filter(age_group == age_group_order[1]) |>
    mutate(age_numeric = 0.5)  # Extend to left edge
  
  last_bin_extension <- projection_data |>
    filter(age_group == age_group_order[length(age_group_order)]) |>
    mutate(age_numeric = length(age_group_order) + 0.5)  # Extend to right edge
  
  # Combine the data
  projection_data_extended <- bind_rows(
    first_bin_extension,
    projection_data,
    last_bin_extension
  ) |>
    arrange(gender, year, age_numeric)
  
  # Create the pyramid plot with bars for current year and step lines for projections
  ggplot() +
    # Add solid filled bars for the current year as the base
    geom_bar(
      data = pop_by_age_gender |> filter(year == min(years_ordered)),
      aes(x = age_group, y = pop_estimate, group = gender),
      stat = "identity",
      fill = "#ADADAD",  # Neutral gray for all genders
      alpha = 0.7,
      color = NA
    ) +
    # Add step lines for future years with extended edges
    geom_step(
      data = projection_data_extended,
      aes(x = age_numeric, y = pop_estimate, color = year_label, linetype = year_label, group = interaction(gender, year_label)),
      direction = "mid",
      linewidth = 1.2
    ) +
    coord_flip() +
    # Ensure the x-axis (age groups) is properly shown
    scale_x_discrete(limits = age_group_order) +
    # Use color only for projection years, not for gender
    scale_color_manual(
      name = "Projection Year",
      values = year_colors
    ) +
    scale_linetype_manual(
      name = "Projection Year",
      values = year_linetypes
    ) +
    labs(
      title = paste0("Population Pyramid for ", location_name),
      subtitle = paste0("Population projections: ", min(years_ordered), " (filled), ", 
                        paste(projection_years, collapse = ", "), " (lines)"),
      x = "Age Group",
      y = "Population"
    ) +
    theme_minimal() +
    # Add line at zero
    geom_hline(yintercept = 0, color = "black") +
    # Custom axis labels to show absolute values
    scale_y_continuous(
      labels = function(x) format(abs(x), big.mark = ","),
      breaks = pretty(c(min(pop_by_age_gender$pop_estimate), max(pop_by_age_gender$pop_estimate)))
    ) +
    # Custom gender labels at the bottom
    annotate("text", x = 1, y = min(pop_by_age_gender$pop_estimate) * 0.9, label = "Male", fontface = "bold") +
    annotate("text", x = 1, y = max(pop_by_age_gender$pop_estimate) * 0.9, label = "Female", fontface = "bold") +
    # Add total population annotation as a single block in top right
    annotate(
      "text", 
      x = max(age_numeric_positions), 
      y = y_max * 0.9, 
      label = pop_summary_label,
      hjust = 1,
      vjust = 1,
      fontface = "bold",
      size = 3.5,
      lineheight = 0.9
    ) +
    # Improve the theme
    theme(
      legend.position = "bottom",
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold", hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5)
    ) +
    # Combine the legends to make them more compact
    guides(
      color = guide_legend(order = 1),
      linetype = guide_legend(order = 1)
    )
}
