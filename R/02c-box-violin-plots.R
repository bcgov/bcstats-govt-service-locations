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
#Prep DB Data for Plotting 
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
# Comparative Plots (Distribution by region)
#------------------------------------------------------------------------------

# user-defined plot parameters
var <- "median_drv_time_min"  # colnames(map_data) for other options
region <- "DB"
var_title <- glue("Median Drive Time per {region} (Minutes)")

plot_title <- glue("Distribution of Count of Addresses to Service BC by {region}")
plot_subtitle <- "Comparison Across Municipalities"

x_title <- "Municipality"
y_title <- var_title


plot_data  <- db_stats_plot_data

if (region == "DA") {
  region_title <- "Dissemination Area"
  plot_data  <- NULL # replace with DA data if we want this functionality later
}

# --- Box Plot: Median Drive Time ---
message("Generating Box Plot...")

plot_boxplot <- build_boxplot(
  data = plot_data,
  x_var = "municipality", #X axis
  y_var = "drv_dist_qnt50", #Y axis,
  plot_title = plot_title,
  plot_subtitle = plot_subtitle,
  x_title = x_title,
  y_title = y_title,
  boxplot_theme = BOX_PLOT_THEME,
  fill_scale = FILL_THEME_D
)

print(plot_boxplot)

# Save plot
plot_boxplot_path <- glue("{OUTPUT_DIR}/drive_time_boxplot.png")
ggsave(plot_boxplot_path, plot_boxplot, width = 8, height = 6, dpi = 300, bg = "white")
message(glue("Box plot saved to: {plot_boxplot_path}"))


# --- Violin Plot: Median Drive Time ---
message("Generating Violin Plot...")
plot_violin <- ggplot(stats_plot, aes(x = municipality, y = median_drv_time_min)) +
  geom_violin(aes(fill = municipality), trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", outlier.size = 0.8, alpha = 0.8) + # Overlay smaller boxplot
  scale_fill_viridis_d(guide = "none", option = "mako", alpha = 0.75, na.value = "red")+
  scale_y_continuous(labels = scales::comma_format()) +
  labs(
    title = "Distribution of Median Drive Times to Service BC by DB",
    subtitle = "Comparison Across Municipalities (Violin Plot)",
    x = "Municipality",
    y = "Median Drive Time per DB (Minutes)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1),
    plot.title = element_text(face = "bold"),
    panel.grid.major.x = element_blank()
  )

print(plot_violin)

# Save plot
plot_violin_path <- glue("{OUTPUT_DIR}/drive_time_violin.png")
ggsave(plot_violin_path, plot_violin, width = 8, height = 6, dpi = 300, bg = "white")
message(glue("Violin plot saved to: {plot_violin_path}"))