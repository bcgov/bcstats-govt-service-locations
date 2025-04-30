# ------------------------------------------------------------------------
# Script: 02c-box-violin-plots.R

# Description: Loads processed DA and Locality level summary statistics.
#              Performs comparative analysis across the different localities,
#              generating plots (boxplots, violin plots) and summary tables
#              comparing key accessibility metrics (e.g., drive time).

# Requirements:
#   - Requires R packages: `tidyverse`, `glue`, `ggplot2`, `scales`.
#   - Depends on `settings.R` for constants (paths, locality map, filenames).
#   - Requires output CSV files from `01-descriptive-tables.R` to exist in
#     the `SRC_DATA_FOLDER`.
#   - Requires write access to `OUTPUT_DIR`.

# Side Effects/Outputs:
#   - Creates and saves PNG plots comparing drive time distributions.
#   - Creates and saves a CSV summary table comparing localities.
#   - Prints plots and tables to the console/RStudio Plots pane.
# ------------------------------------------------------------------------

#------------------------------------------------------------------------------
# Load Libraries and Settings
#------------------------------------------------------------------------------
library(tidyverse)
library(glue)
library(ggplot2)
library(scales)
library(snakecase)

source("R/settings.R")
source("R/fxns/plots.r")


# Ensure output directory exists
output_dir <- file.path(VISUALS_OUT, "csd-drive-distance-plots")

#------------------------------------------------------------------------------
# Load DB-level plot data
#------------------------------------------------------------------------------

db_stats_raw <-
  read_csv(glue("{SRC_DATA_FOLDER}/reduced_db_average_times_dist_all_locs.csv")
               , col_types = cols(.default = "c"))

# Identify columns expected to be numeric based on naming convention
numeric_cols_pattern <- "(dist|time|area|households|n_address|population|dwellings)" # Adjust if needed
numeric_cols <- names(db_stats_raw)[str_detect(names(db_stats_raw), numeric_cols_pattern)]

db_stats_raw <- db_stats_raw %>%
  mutate(across(all_of(numeric_cols), as.numeric)) %>%
  mutate(municipality = as.factor(csd_name))

#------------------------------------------------------------------------------
# Comparative Box and Violin Plots (Distribution by region)
#------------------------------------------------------------------------------

region <- "Dissemination Block"
plot_data <- db_stats_raw %>%
  filter(csd_name %in% CSD_NAMES)

# user-defined plot parameters
y_var <- "drv_time_min_mean"  # colnames(plot_data) for other options
x_var <- "municipality"

y_title <- "Mean Driving Time"
y_unit <- "minutes"
x_title <- "Municipality"

plot_title <- glue("Distribution of {y_title} to Nearest Service BC Office")
plot_subtitle <- glue("Comparison across municipalities")

# --- Box Plot (Distribution by region) ---
message("Generating Box Plot...")

outfile <- to_snake_case(glue("box plot {y_var} by {region}"))

box_plot <- build_boxplot(
  data = plot_data,
  x_var = x_var,
  y_var = y_var,
  plot_title = plot_title,
  plot_subtitle = plot_subtitle,
  x_title = x_title,
  y_title = y_title,
  plot_theme = BOX_PLOT_THEME,
  fill_scale = FILL_THEME_D
)

# Save the plot
ggsave(
  filename = glue("{output_dir}/{outfile}.png"),
    plot = box_plot,
  width = 8,
  height = 7,
  device = "png"
)

message(glue("Box plot saved to: {outfile}"))

# --- Violin Plot (Distribution by region) ---
message("Generating Violin Plot...")

outfile <- to_snake_case(glue("violin plot {y_var} by {region}"))

violin_plot <- build_violinplot(
  data = plot_data,
  x_var = x_var,
  y_var = y_var,
  plot_title = plot_title,
  plot_subtitle = plot_subtitle,
  x_title = x_title,
  y_title = y_title,
  plot_theme = VIOLIN_PLOT_THEME,
  fill_scale = FILL_THEME_D
)

# Save the plot
ggsave(
  filename = glue("{output_dir}/{outfile}.png"),
  plot = violin_plot,
  width = 8,
  height = 7,
  device = "png"
)

message(glue("Violin plot saved to: {outfile}"))

rm(list = ls())
gc()
