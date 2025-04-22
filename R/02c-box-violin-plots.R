# ------------------------------------------------------------------------
# Script: 03-comparative-analysis.R

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
library(scales) # For formatting axes/labels

source("R/settings.R")


# Create output directory if it doesn't exist
# if (!dir.exists(OUTPUT_DIR)) {#
#   dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)
# }


#------------------------------------------------------------------------------
# Load DB-Level Data 
#------------------------------------------------------------------------------
db_stats_file <- glue("{SRC_DATA_FOLDER}/{OUTPUT_DB_STATS_FILENAME}")

message(glue("Loading region stats from: {db_stats_file}"))
db_stats_raw <- tryCatch({
  read_csv(db_stats_file, col_types = cols(.default = "c"))
}, error = function(e) {
  stop(glue("Failed to read DB stats file '{stats_file}': {e$message}"))
})

if (nrow(db_stats_raw) == 0) {
  stop("statistics file is empty.")
}

#------------------------------------------------------------------------------
# Prep DB Data for Plotting 
#------------------------------------------------------------------------------

# Identify columns expected to be numeric based on naming convention
numeric_cols_pattern <- "(dist|time|area|households|n_address|population|dwellings)" # Adjust if needed
numeric_cols <- names(db_stats_raw)[str_detect(names(db_stats_raw), numeric_cols_pattern)]

db_stats_plot_data <- db_stats_raw %>%
  mutate(across(all_of(numeric_cols), as.numeric)) %>%
  mutate(across(any_of(c('csd_names', 'dissemination_block_id')), as.character)) %>%
  mutate(municipality = as.factor(csd_names)) %>%
  filter(!is.na(csd_names))

# Check if data remains after processing
if (nrow(db_stats_plot_data) == 0) {
  stop("No data remains after processing and joining municipality names.")
}

#------------------------------------------------------------------------------
# Comparative Box Plot (Distribution by region)
#------------------------------------------------------------------------------

# user-defined plot parameters
y_var <- "median_drv_time_min"  # colnames(map_data) for other options
x_var <- "municipality"
region <- "DB"

y_title <- glue("Median Drive Time (min)")
x_title <- "Municipality"

plot_data  <- db_stats_plot_data
region_title <- "Dissemination Block"

if (region == "DA") {
  region_title <- "Dissemination Area"
  plot_data  <- NULL # replace with DA data if we want this functionality later
}

plot_title <- glue("Distribution of {y_title} to nearest Service BC Location")
plot_subtitle <- glue("Comparison Across Municipalities, by {region_title}")

# Generate plot
message("Generating Box Plot...")

box_plot_out <- build_boxplot(
  data = plot_data,
  x_var = x_var, 
  y_var = "drv_dist_qnt50", 
  plot_title = plot_title,
  plot_subtitle = plot_subtitle,
  x_title = x_title,
  y_title = y_title,
  boxplot_theme = BOX_PLOT_THEME,
  fill_scale = FILL_THEME_D
)

# Save the plot
fn <- to_snake_case(glue("{y_var} by {region}"))
fn <- glue("{fn}.svg")
ggsave(
  filename = fn,
  path = VISUALS_OUT,
  plot = box_plot_out,
  width = 8,
  height = 7,
  device = "svg"
)

message(glue("Box plot saved to: {VISUALS_OUT}/{fn}"))

# ------------------------------------------------------------------------
# Comparative Violin Plot (Distribution by region)
# ------------------------------------------------------------------------

# user-defined plot parameters
y_var <- "drv_time_sec_qnt100"
x_var <- "municipality"
region <- "DB"

y_title <- glue("Median Drive Time (Minutes)")
x_title <- "Municipality"

plot_data  <- db_stats_plot_data
region_title <- "Dissemination Block"

if (region == "DA") {
  region_title <- "Dissemination Area"
  plot_data  <- NULL # replace with DA data if we want this functionality later
}

plot_title <- glue("Distribution of {y_title} to nearest Service BC Location")
plot_subtitle <- glue("Comparison Across Municipalities, by {region_title}")

# Generate plot
message("Generating Violin Plot...")

plot_violin <- build_violinplot(
  data = plot_data,
  x_var = x_var,
  y_var = y_var,
  plot_title = plot_title,
  plot_subtitle = plot_subtitle,
  x_title = x_title,
  y_title = y_title,
  violinplot_theme = theme_minimal(),
  fill_scale = scale_fill_viridis_d(option = "mako") #Made sure the option has 
)

print(plot_violin)

# Save plot
plot_violin_path <- glue("{OUTPUT_DIR}/drive_time_violin.png")
ggsave(plot_violin_path, plot_violin, width = 8, height = 6, dpi = 300, bg = "white")
message(glue("Violin plot saved to: {plot_violin_path}"))