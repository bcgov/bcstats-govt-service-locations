# ------------------------------------------------------------------------
# ------------------------------------------------------------------------
# Script: 02c-box-violin-plots.R

# Description: Creates statistical visualizations (box plots and violin plots)
# showing the distribution of drive times to Service BC offices across different
# regions of British Columbia. Compares access metrics between census subdivisions
# and demographic groups.

# Requirements:
#   - Requires R packages: `tidyverse`, `glue`, `janitor`, `ggplot2`, `forcats`,
#     `patchwork`, `scales`, `stringr`, `fs`
#   - Depends on `settings.R` for paths and constants.
#   - Requires input CSV data files with drive times, population data,
#     demographic information, and census subdivision metadata.
#   - Requires read/write access to the plot output folder.

# Side Effects/Outputs:
#   - Creates PNG plot showing statistical distribution of drive times via box plots,
#     violin plots, or combined visualizations, grouped by relevant factors
#   - Summary statistics are saved to a CSV file in the same directory
# ------------------------------------------------------------------------

#------------------------------------------------------------------------------
# Load Libraries and Settings
#------------------------------------------------------------------------------

source("R/settings.R")
source("R/fxns/csd-plots.R")


# Ensure output directory exists
output_dir <- file.path(VISUALS_OUT, "csd-drive-distance-plots")

#------------------------------------------------------------------------------
# Load DB-level plot data
#------------------------------------------------------------------------------

db_stats_raw <-
  read_csv(
    glue("{SRC_DATA_FOLDER}/reduced_db_average_times_dist_all_locs.csv"),
    col_types = cols(.default = "c")
  )

# Identify columns expected to be numeric based on naming convention
numeric_cols_pattern <- "(dist|time|area|households|n_address|population|dwellings)" # Adjust if needed
numeric_cols <- names(db_stats_raw)[str_detect(
  names(db_stats_raw),
  numeric_cols_pattern
)]

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
y_var <- "drv_dist_mean" # colnames(plot_data) for other options
x_var <- "municipality"

y_title <- "Mean Driving Distance (km)"
y_unit <- "km"
x_title <- "CSD"

plot_title <- glue("Distribution of {y_title} to Nearest Service BC Office")
plot_subtitle <- glue("Comparison across CSDs")


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
  y_title = glue("{y_title} \n\n"),
  plot_theme = BOX_PLOT_THEME,
  fill_scale = FILL_THEME_D
)

box_plot
# Save the plot
ggsave(
  filename = glue("{output_dir}/{outfile}.png"),
  plot = box_plot,
  width = 12,
  height = 8,
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

rm(list = ls())
gc()
