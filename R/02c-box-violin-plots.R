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


# --- Quantile Bar Plot (Distribution by region) ---

# user-defined plot parameters
y_title <- "Driving Time"
y_unit <- "minutes"
x_title <- "Municipality"

# Create bar plot
plot_title <- glue("{y_title} Quantiles to Nearest Service BC Office")
plot_subtitle <- "Comparison across municipalities"

# Reshape data to long format for quantile plotting
db_quantiles <- db_stats_raw %>%
  filter(csd_name %in% CSD_NAMES) %>%
  select(municipality, matches("drv_time_min_qnt")) %>%
  pivot_longer(
    cols = matches("drv_time_min_qnt"),
    names_to = "quantile",
    values_to = "drive_time_minutes"
  ) %>%
  # Extract the quantile number for better labeling
  mutate(
    quantile = str_extract(quantile, "([0-9]+)"),
    quantile_label = glue("{quantile}%"),
    quantile_label = factor(quantile_label, levels = c("0%", "25%", "50%", "75%", "100%"))
  )

message("Generating Quantile Plot...")

quantile_bar_plot <- ggplot(db_quantiles, aes(x = municipality, y = drive_time_minutes, fill = quantile_label)) +
  geom_col(position = position_dodge(width = 0.9), width = 0.8) +
  scale_fill_viridis_d(option = "mako", name = "Quantile") +
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    x = x_title,
    y = y_title
  ) +
  BOX_PLOT_THEME

outfile <- to_snake_case(glue("quantile plot {y_title} by municipality"))