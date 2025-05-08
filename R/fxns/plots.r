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
    sbc_col = NULL,  # New parameter for filtering servicebc_data
    plot_title = "",
    plot_subtitle = "",
    legend_title = "",
    map_theme = theme_minimal(),
    fill_scale = scale_fill_viridis_c(option = "viridis"),
    scale_limits = NULL,
    na_color = "#E41A1C"  # Default light red color for NA values
) {

  # --- Prepare arguments as symbols ---
  varname_sym <- rlang::sym(varname)
  csd_col_sym <- rlang::sym(csd_col)
  
  # Use sbc_col if provided, otherwise default to csd_col
  sbc_col_sym <- if (!is.null(sbc_col)) rlang::sym(sbc_col) else csd_col_sym

  # --- Prepare titles as strings --
  default_title <- glue::glue("{varname} for {csd_name}")
  plot_title <- plot_title %||% default_title
  legend_title <- legend_title %||% varname

  # dynamically set limits
  fill_theme <- fill_scale$clone()
  fill_theme$limits <- range(data[[varname_sym]], na.rm = TRUE)
  fill_theme$oob <- scales::squish
  
  # Filter data for the specified location
  map_data <- data[data[[csd_col_sym]] == csd_name,]
  
  # Split the data into those with and without the variable value
  map_data_na <- map_data[is.na(map_data[[varname_sym]]),]
  map_data_with_values <- map_data[!is.na(map_data[[varname_sym]]),]

  # Only filter servicebc_data if the column exists, otherwise use all points
  if (sbc_col %in% names(servicebc_data)) {
    points_data <- servicebc_data[servicebc_data[[sbc_col_sym]] == csd_name,]
  } else {
    # If no column specified or not found, use all service locations
    points_data <- servicebc_data
  }

  # Check if filtering resulted in data
  if (nrow(map_data) == 0) {
    warning(glue::glue("Warning: No data found for csd_name '{csd_name}' in column '{csd_col}'"))
    return(ggplot() + theme_void() + labs(title = glue::glue("No data for {csd_name}")))
  }

  ## Build the ggplot object
  map <- ggplot() +
    # First draw the polygons with NA values in gentle red
    geom_sf(
        data = map_data_na,
        fill = na_color,
        color = "gray50",
        lwd = 0.1,
        alpha = 0.5  # Gentle transparency for the red
    ) +
    # Then draw the polygons with values using the color scale
    geom_sf(
        data = map_data_with_values,
        aes(fill = !!varname_sym),
        color = "gray40",
        lwd = 0.1
    ) +
    fill_theme +
    # Add Service BC locations
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
      ),
      fill = guide_colorbar(
        title = legend_title,
        title.position = "top",
        label.position = "bottom",
        barwidth = 10,
        barheight = 0.5
      )
    ) +
    labs(
      title = plot_title,
      subtitle = plot_subtitle,
      fill = legend_title,
      x = "\nLongitude",
      y = "Latitude\n",
      caption = "Note: Areas in red indicate no drive data available"
    ) +
    map_theme +
    # Adjust the legend position to make room for the caption
    theme(
      plot.caption = element_text(hjust = 0, face = "italic", size = 9)
    )

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

# Function to create drive distance histograms
create_drive_distance_histogram <- function(data, facility_name = NULL, facet = FALSE) {
  # Filter to facility if specified
  plot_data <- data
  if (!is.null(facility_name) && !facet) {
    plot_data <- data |> filter(assigned == facility_name)
  }
  
  # Calculate stats
  plot_data <- plot_data |>
    group_by(assigned) |>
    mutate(
      drv_dist_mean = mean(drv_dist, na.rm = TRUE),
      # Ensure there are no zero values for better visualization
      drv_dist = if_else(drv_dist == 0, 0.01, drv_dist)
    ) |>
    ungroup()
  
  # Create the base plot
  p <- ggplot(plot_data, aes(x = drv_dist)) +
    # Add histogram with semi-transparent fill
    geom_histogram(
      aes(fill = assigned),
      binwidth = 3,
      alpha = 0.7,
      color = "white",
      boundary = 0
    ) +
    # Add vertical line for the mean
    geom_vline(
      aes(xintercept = drv_dist_mean),
      linetype = "dashed",
      color = "red",
      linewidth = 1
    ) +
    # Add text label for the mean value
    geom_text(
      aes(
        x = drv_dist_mean + 5, 
        y = Inf, 
        label = paste0("Mean: ", round(drv_dist_mean, 1), " km")
      ),
      vjust = 1.5,
      hjust = 0,
      color = "red",
      fontface = "bold",
      size = 3.5,
      check_overlap = TRUE
    ) +
    # Set consistent x limit 
    xlim(0, 150) +
    # Add labels
    labs(
      x = "Drive Distance (km)",
      y = "Count"
    ) +
    # Apply a clean theme
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 12),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 12, face = "bold"),
      legend.position = "none",
      panel.grid.minor = element_blank()
    )
  
  # Add faceting if requested
  if (facet) {
    p <- p + 
      facet_wrap(~ assigned, scales = "free_y") +
      labs(
        title = "Driving Distances to Service BC Locations",
        subtitle = "Distribution of driving distances for each facility with mean value"
      ) +
      theme(
        strip.text = element_text(size = 12, face = "bold"),
        panel.spacing = unit(1, "lines")
      )
  } else {
    # Individual facility plot
    p <- p + 
      labs(
        title = paste0("Driving Distances to ", facility_name),
        subtitle = paste0("Distribution of driving distances with mean value of ", 
                          round(mean(plot_data$drv_dist_mean), 1), " km")
      )
  }
  
  return(p)
}
