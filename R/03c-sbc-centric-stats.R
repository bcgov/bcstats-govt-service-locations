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

# =========================================================================== #
# Script: 03c-sbc-centric-stats.R

# Description: Creates metrics for each individual Service BC location.
# Using the pre-computed catchment assignments, this script generates
# population projections, driving distance metrics, and visualizations
# for each facility.

# Requirements:
#   - Requires R packages: `tidyverse`, `glue`, `janitor`, `sf`, `bcdata`
#   - Depends on `settings.R` for paths and constants
#   - Requires output from 03a-create-catchments.R
#   - Requires various data files and auxiliary functions

# Side Effects/Outputs:
#   - Writes processed CSV files and plots to the outputs folder
#   - Creates various visualizations in the MAP_OUT directory
# =========================================================================== #

# =========================================================================== #
# Load libraries and settings ----
# =========================================================================== #

source("R/settings.R")
source("R/fxns/sbc-plots.R")

# =========================================================================== #
# Read required data in ----
# =========================================================================== #
## drive time data ----
## from BCDS
drivetime_data <-
  read_csv(
    glue("{SRC_DATA_FOLDER}/full-processed-drivetime-data.csv"),
    col_types = cols(.default = "c")
  ) %>%
  clean_names() %>%
  mutate(across(c(drv_time_sec, drv_dist), as.numeric))

## SBC locations to include ----
## from source folder
sbc_locs <- read_csv(SBCLOC_FILEPATH) |>
  st_as_sf(coords = c("coord_x", "coord_y"), crs = 3005) |>
  filter(csd_name %in% CSD_NAMES)

## Read the pre-computed assignment data ----
## from source folder
complete_assignments <- read_csv(
    glue("{SRC_DATA_FOLDER}/complete-db-assignments.csv")
    ) %>% 
    mutate(dbid = as.character(dbid))

## population projections from catalogue
pop_projections <- read_csv(glue("{SRC_DATA_FOLDER}/full-population-projections.csv")) %>%
  mutate(region = as.character(region))


## census populations ----
## from statscan
pop_db <- read_csv(
  glue("{SRC_DATA_FOLDER}/full-population-db.csv"),
  col_types = cols(.default = "c")
) %>%
  clean_names() %>%
  mutate(across(c(area_sq_km, population, dwellings, households), as.numeric))

## db shapefiles
db_shapefile <-
  st_read(glue("{SHAPEFILE_OUT}/full-db_with_location.gpkg")) %>%
  mutate(across(c(landarea), as.numeric))

## crosswalk (entire province, not filtered)
crosswalk <-
  read_csv(
    glue("{SRC_DATA_FOLDER}/csd-da-db-loc-correspondance.csv"),
    col_types = cols(.default = "c")
  ) %>%
  clean_names()

# DB population projections ----
# this table has a pct of csd column that we can use
# to multiply by any other population projections to get estimates
db_projections_transformed <- readRDS(glue("{SRC_DATA_FOLDER}/full-db-projections-transformed.rds"))

# =========================================================================== #
# Metrics for SBC locations of interest ----
# metrics of interest include:
# - histograms or ridgelines of drive times/distances
# - map that has all the 'closest' dbs
# - demographic estimates at DB level
# =========================================================================== #

# Join with drivetime data
drivetime_data_full <- complete_assignments |> 
  left_join(drivetime_data, by = "dbid")

drivetime_data %>% 
    # fix the daid column to have no NAs
    mutate(daid = str_sub(dbid, 1, 8)) %>% 
    left_join(crosswalk, by = c("dbid", "daid"))  %>% 
    filter(csd_name == 'Kamloops') %>% 
    summarize(max = max(drv_dist, na.rm = TRUE),
    max2 = quantile(drv_dist, probs = 1.00, na.rm = TRUE))

drivetime_data_reduced <- drivetime_data_full %>% 
    # fix the daid column to have no NAs
    mutate(daid = str_sub(dbid, 1, 8)) %>% 
    filter(
    assigned %in%
      (sbc_locs |>
         filter(csd_name %in% CSD_NAMES) |>
         pull(nearest_facility))
    ) |>
    filter(
    nearest_facility %in%
        (sbc_locs |>
            filter(csd_name %in% CSD_NAMES) |>
            pull(nearest_facility)) | is.na(nearest_facility)
    ) |>
    left_join(crosswalk, by = c("dbid", "daid"))

## SBC centric measures (table form) ----
# drive measures
drive_measures <- drivetime_data_reduced |>
  group_by(assigned) |>
  summarize(
    n_addresses = n(),
    mean_drv_time = mean(drv_time_sec, na.rm = TRUE),
    sd_drv_time = sd(drv_time_sec, na.rm = TRUE),
    var_drv_time = var(drv_time_sec, na.rm = TRUE),# or min(drv_time_sec, na.rm = TRUE)
    qnt25_drv_time = quantile(drv_time_sec, probs = 0.25, na.rm = TRUE),  
    qnt50_drv_time = quantile(drv_time_sec, probs = 0.50, na.rm = TRUE), # or median(drv_time_sec, na.rm = TRUE)
    qnt75_drv_time = quantile(drv_time_sec, probs = 0.75, na.rm = TRUE),  
    min_drv_time = min(drv_time_sec, na.rm = TRUE),
    max_drv_time = max(drv_time_sec, na.rm = TRUE),
    mean_drv_dist = mean(drv_dist, na.rm = TRUE),
    sd_drv_dist = sd(drv_dist, na.rm = TRUE),
    var_drv_dist = var(drv_dist, na.rm = TRUE),# or min(drv_time_sec, na.rm = TRUE)
    qnt25_drv_dist = quantile(drv_dist, probs = 0.25, na.rm = TRUE),  
    qnt50_drv_dist = quantile(drv_dist, probs = 0.50, na.rm = TRUE), # or median(drv_time_sec, na.rm = TRUE)
    qnt75_drv_dist = quantile(drv_dist, probs = 0.75, na.rm = TRUE),  
    min_drv_dist = min(drv_dist, na.rm = TRUE),
    max_drv_dist = max(drv_dist, na.rm = TRUE)
  ) |>
  ungroup()

drive_measures

# population measures
# csds served by each SBC location
csds_serviced <- drivetime_data_reduced |>
  distinct(assigned, csdid, csd_name, dbid) |>
  left_join(
    db_projections_transformed  %>% distinct(dbid, pct_of_csd), 
    by = "dbid"
    ) |>
  filter(!is.na(csdid)) |>
  group_by(assigned, csdid, csd_name) |>
  summarize(
    pct_of_csd = sum(pct_of_csd, na.rm=TRUE)
  ) |>
  ungroup() |>
  arrange(assigned, desc(pct_of_csd))

csds_serviced

# total population, average/median age
pop_measures <- drivetime_data_reduced |>
  distinct(assigned, assignment_method, csdid, dbid) |>
  # expand drivetime data to include all years of interest for each row
    expand_grid(
        tibble(year = rep(unique(db_projections_transformed$year)))
    ) |>
  left_join(
    db_projections_transformed %>% 
    filter(gender=='T') %>% 
    select(dbid, year, age, population, total, area_sq_km), 
    by = c("dbid", 'year')
    ) |> 
  filter(!is.na(csdid)) %>% 
  group_by(assigned, year) %>% 
  summarize(
    n = n(),
    n_dbs = n_distinct(dbid),
    n_dbs_from_drive_time = n_distinct(dbid[assignment_method == "drive_time"]),
    n_dbs_from_spatial = n_distinct(dbid[assignment_method == "nearest_facility"]),
    pct_from_drive_time = n_dbs_from_drive_time / n_dbs,
    n_csds = n_distinct(csdid),
    est_pop = sum(population, na.rm=TRUE),
    # don't double count areas as there are multiple ages for each DB
    est_area = sum(if_else(age==0, area_sq_km, 0), na.rm=TRUE),
    avg_age = weighted.mean(age, population, na.rm=TRUE),
    # because of mismatch in number of dbids between pop and shapefile
    # need to make sure that the age/population columns are populated here
    median_age = median(
        rep(
            if_else(is.na(age), 0 , age),
            times = round(
                if_else(is.na(population), 0, population)
                )
                )
                ), 
    .groups = "drop"
  )  %>% 
  left_join(
    csds_serviced  %>% 
    filter(pct_of_csd > 0.5)  %>% 
    group_by(assigned) %>% 
    summarize(n_majority_csds = n()),
    by = "assigned"
  ) %>% 
  select(
    assigned, year,
    n_dbs, n_dbs_from_drive_time, n_dbs_from_spatial, pct_from_drive_time,
    n_csds, n_majority_csds, 
    est_pop, est_area, avg_age, median_age
  )

pop_measures

# look at some weird percents, eg esquimalt
# likely due to missing DBs in the data?
drivetime_data_full |>
  left_join(crosswalk, by = c("dbid", "daid")) |>
  filter(csd_name == "Esquimalt") |>
  distinct(nearest_facility)

crosswalk |>
  left_join(drivetime_data_full, by = c("dbid", "daid")) |>
  filter(csd_name == "Esquimalt") |>
  filter(is.na(nearest_facility)) %>% 
  pull(dbid)

db_shapefile  %>% filter(dbid=='59170315002')

# there is a mismatch between the population data number of DBs and the
# shapefile number of DBs
db_shapefile  %>% distinct(dbid)  %>% nrow() # 52423
pop_db  %>% distinct(dbid)  %>% nrow() # 52387

# SAVE ALL TABLES ----
write_csv(
  drive_measures, 
  glue("{TABLES_OUT}/service_bc_centered_drive_metrics.csv")
)

write_csv(
  csds_serviced, 
  glue("{TABLES_OUT}/service_bc_centered_csd_counts.csv")
)

write_csv(
  pop_measures, 
  glue("{TABLES_OUT}/service_bc_centered_population_metrics.csv")
)

# also save csd populations for reference
write_csv(
  pop_projections  %>% filter(region_name %in% CSD_NAMES)  %>% filter(year %in% c(CURRENT_YEAR, CURRENT_YEAR + 5, CURRENT_YEAR + 10)),
  glue("{TABLES_OUT}/csd_populations.csv")
)

# plots ----
# Create individual histograms for each facility 
# Create a folder specifically for the histograms
plot_folder <- file.path(MAP_OUT, "sbc_drive_distance_plots")
if (!dir.exists(plot_folder)) {
  dir.create(plot_folder, recursive = TRUE)
}

# Create and save histograms for each facility
facilities <- unique(drivetime_data_reduced$assigned)
for (facility in facilities) {
  p <- create_drive_distance_histogram(
    data = drivetime_data_reduced, 
    facility_name = facility, 
    facet = FALSE
  )
  
  # Save the individual plot
  ggsave(
    filename = glue("{plot_folder}/{facility}_drive_distance.png"),
    plot = p,
    width = 10,
    height = 6,
    dpi = 300
  )
}

# example
print(p)

# Create the faceted plot with all facilities
faceted_plot <- create_drive_distance_histogram(
  data = drivetime_data_reduced,
  facet = TRUE
)

# display
print(faceted_plot)

# Save the faceted plot
ggsave(
  filename = glue("{plot_folder}/all_facilities_drive_distance.png"),
  plot = faceted_plot,
  width = 16,
  height = 12,
  dpi = 300
)

# create a box plot for all facilities
box_plot <- build_boxplot(
  data = drivetime_data_reduced %>% 
          group_by(dbid, assigned) %>% 
          summarize(drv_dist = mean(drv_dist, na.rm = TRUE)),
  x_var = 'assigned',
  y_var = 'drv_dist',
  plot_title = 'Distribution of Driving Distances',
  plot_subtitle = 'Comparison across Service BC Catchments',
  x_title = 'Service BC Location',
  y_title = 'Driving Distance (km)\n\n',
  plot_theme = BOX_PLOT_THEME,
  fill_scale = FILL_THEME_D
)

box_plot
ggsave(
    filename = glue("{plot_folder}/drive_distance_box_plot.png"),
    plot = box_plot,
    width = 15,
    height = 8,
    dpi = 300
)

# step 2: map of all closest DBs/average drive distances
map_data <- drivetime_data_reduced |>
  mutate(dbid = as.character(dbid)) |>
  group_by(dbid, assigned) |>
  summarize(drive_dist_mean = mean(drv_dist, na.rm = TRUE)) |>
  ungroup() |>
  left_join(db_shapefile, by = c("dbid")) |>
  st_as_sf(crs = 3005)

# Create a folder for drive distance maps
map_folder <- file.path(MAP_OUT, "sbc_drive_distance_maps")
if (!dir.exists(map_folder)) {
  dir.create(map_folder, recursive = TRUE)
}

# Generate a map for each Service BC location
for (loc_id in unique(map_data$assigned)) {
  var <- "drive_dist_mean"
  loc_col <- "assigned"
  plot_title <- glue("Driving Distances for {loc_id}")
  var_title <- "Driving Distance (km)"
  
  map_plot <- build_map(
    data = map_data,
    servicebc_data = sbc_locs,
    varname = var,
    csd_name = loc_id,
    csd_col = loc_col,
    sbc_col = "nearest_facility",  # Add the column name to use for filtering
    map_theme = MAP_THEME,
    fill_scale = FILL_THEME,
    plot_title = plot_title,
    legend_title = var_title
  )
  
  # Save the map
  ggsave(
    filename = glue("{map_folder}/{loc_id}_drive_distance_map.png"),
    plot = map_plot,
    width = 10,
    height = 8,
    dpi = 300
  )
}

# Display the map for a specific location as an example
var <- "drive_dist_mean"
loc_col <- "assigned"
plot_title <- glue("Driving Distances for {loc_id}")
var_title <- "Driving Distance (km)"

loc_id <- unique(map_data$assigned)[1]
example_map <- build_map(
  data = map_data,
  servicebc_data = sbc_locs,
  varname = var,
  csd_name = loc_id,
  csd_col = loc_col,
  sbc_col = "nearest_facility",  # Specify the column to use in sbc_locs
  map_theme = MAP_THEME,
  fill_scale = FILL_THEME,
  plot_title = plot_title,
  legend_title = var_title
)

print(example_map)

# =========================================================================== #
# Population Pyramid Creation ----
# =========================================================================== #

# Prepare data for population pyramids by joining with location assignment data
pyramid_data <- complete_assignments |>
  select(dbid, assigned, assignment_method) |>
  inner_join(
    db_projections_transformed |>
      filter(year %in% c(CURRENT_YEAR, CURRENT_YEAR + 5, CURRENT_YEAR + 10)),
    by = "dbid"
  )  %>% 
  filter(
    assigned %in% (
        sbc_locs |>
            filter(csd_name %in% CSD_NAMES) |>
            pull(nearest_facility)))

# Get unique facility names to generate pyramids for
facilities <- unique(pyramid_data$assigned)

# Generate population pyramids for each facility
population_pyramids <- list()
for (facility in facilities) {
  population_pyramids[[facility]] <- create_population_pyramid(
    data = pyramid_data,
    location_name = facility
  )
}

# Display a sample pyramid for the first facility
if (length(facilities) > 0) {
  print(population_pyramids[[facilities[2]]])
}

# Create a folder for drive distance maps
pyramid_folder <- file.path(MAP_OUT, "sbc_population_pyramids")
if (!dir.exists(pyramid_folder)) {
  dir.create(pyramid_folder, recursive = TRUE)
}

for (facility in facilities) {
  ggsave(
    filename = glue("{pyramid_folder}/{facility}_population_pyramid.png"),
    plot = population_pyramids[[facility]],
    width = 10,
    height = 8,
    dpi = 300
  )
}

# Create a combined view with multiple pyramids
# Take up to 4 facilities for a nice grid layout
if (length(facilities) > 1) {
  combined_pyramids <- cowplot::plot_grid(
    plotlist = population_pyramids[seq_len(min(4, length(facilities)))],
    ncol = 2
  )
  
  ggsave(
    filename = glue("{pyramid_folder}/combined_population_pyramids.png"),
    plot = combined_pyramids,
    width = 16,
    height = 12,
    dpi = 300
  )
}

combined_pyramids

rm(list=ls())
gc()
