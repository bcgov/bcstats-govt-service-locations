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
library(tidyverse)
library(glue)
library(janitor)
library(sf)
library(bcdata)

source("R/settings.R")
source("R/fxns/plots.R")

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
    glue("{SRC_DATA_FOLDER}/complete_db_assignments.csv")
    ) %>% 
    mutate(dbid = as.character(dbid))

## population projections ----
## from catalogue

# figure out ID of file we want
bcdc_search("sub-provincial projections")
# population projections: 86839277-986a-4a29-9f70-fa9b1166f6cb
#       - csd resource: 0e15d04d-127c-457a-b999-20800c929927
# household estimates: 2a8ddf6c-dfb9-4187-a66d-9bb15b15ea83
# note that these don't have gender/age breakdowns, which we will want

pop_projections <- bcdc_get_data(
  "86839277-986a-4a29-9f70-fa9b1166f6cb",
  resource = "0e15d04d-127c-457a-b999-20800c929927"
) |>
  janitor::clean_names() |>
  mutate(
    region = paste0(
      "59",
      str_pad(as.character(region), width = 5, side = "left", pad = "0")
    )
  )

pop_projections

## census populations ----
## from statscan
pop_db <- read_csv(
  glue("{SRC_DATA_FOLDER}/population-db.csv"),
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

# =========================================================================== #
# DB population projections ----
# =========================================================================== #

# we want to convert the CSD population projections into DB projections
# we do this by approximation,
# and assume that the proportion of the DB that makes
# up the CSD each year doesn't change
# while not perfect, it will give reasonable estimates

# first, we create a new 'csd_clean' label to match the population projections
# as some CSDs are rolled up in the projections
get_clean_csd <- pop_db |>
  left_join(crosswalk, by = "dbid") |>
  left_join(
    pop_projections |>
      distinct(region) |>
      mutate(in_projections = 1),
    by = c("csdid" = "region")
  ) |>
  # if rolled up, last 3 digits replaced with '999'
  mutate(
    csd_clean = if_else(
      is.na(in_projections),
      paste0(str_sub(csdid, 1, 4), "999"),
      csdid
    )
  )

# check to see if any regions no longer align (should be empty)
get_clean_csd |>
  left_join(
    pop_projections |>
      distinct(region) |>
      mutate(test_in = 1),
    by = c("csd_clean" = "region")
  ) |>
  filter(is.na(test_in))

# now get pct of each 'clean' csd that is taken up by each DB
prop_of_csd <- get_clean_csd |>
  group_by(csd_clean) |>
  mutate(csd_population = sum(population, na.rm=TRUE)) |>
  ungroup() |>
  mutate(pct_of_csd = if_else(population==0, 0, population / csd_population))

# join back to projections to get yearly estimates
# for each age, gender, year of interest
db_projections <- prop_of_csd |>
  select(
    dbid,
    daid,
    csdid,
    csd_clean,
    csd_name,
    csd_desc,
    area_sq_km,
    population,
    csd_population,
    dwellings,
    households,
    pct_of_csd
  ) |>
  left_join(
    pop_projections |>
      filter(year %in% c(CURRENT_YEAR, CURRENT_YEAR + 5, CURRENT_YEAR + 10)),
    by = c("csd_clean" = "region"),
    relationship = "many-to-many"
  )

# Preprocess db_projections data to transform age columns into rows
# The columns x0, x1, etc. represent different age groups
db_projections_transformed <- db_projections |>
  # Get all column names that start with 'x' followed by digits (age columns)
  pivot_longer(
    cols = starts_with("x") & matches("^x\\d+$"),
    names_to = "age_column",
    values_to = "population_by_age"
  ) |>
  # Extract age values from column names (remove 'x' prefix)
  mutate(
    age = as.numeric(str_replace(age_column, "^x", "")),
    # Create age groups (0-4, 5-9, etc.)
    age_group = case_when(
      age < 5 ~ "0-4",
      age < 10 ~ "5-9",
      age < 15 ~ "10-14",
      age < 20 ~ "15-19",
      age < 25 ~ "20-24",
      age < 30 ~ "25-29",
      age < 35 ~ "30-34",
      age < 40 ~ "35-39",
      age < 45 ~ "40-44",
      age < 50 ~ "45-49",
      age < 55 ~ "50-54",
      age < 60 ~ "55-59",
      age < 65 ~ "60-64",
      age < 70 ~ "65-69",
      age < 75 ~ "70-74",
      age < 80 ~ "75-79",
      age < 85 ~ "80-84",
      age < 90 ~ "85-89",
      TRUE ~ "90+"
    ),
    # Calculate population estimate for each DB
    population = population_by_age * pct_of_csd,
    total = total * pct_of_csd
  )

# this table now has a pct of csd column that we can use
# to multiply by any other population projections to get estimates
db_projections_transformed

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
    min_drv_time = min(drv_time_sec, na.rm = TRUE),
    max_drv_time = max(drv_time_sec, na.rm = TRUE),
    mean_drv_dist = mean(drv_dist, na.rm = TRUE),
    sd_drv_dist = sd(drv_dist, na.rm = TRUE),
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
    select(dbid, year, age, population, total), 
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
    est_pop, avg_age, median_age
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

# plots ----
# Create individual histograms for each facility 
# Create a folder specifically for the histograms
histogram_folder <- file.path(MAP_OUT, "sbc_drive_distance_histograms")
if (!dir.exists(histogram_folder)) {
  dir.create(histogram_folder, recursive = TRUE)
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
    filename = glue("{histogram_folder}/{facility}_drive_distance.png"),
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
  filename = glue("{histogram_folder}/all_facilities_drive_distance.png"),
  plot = faceted_plot,
  width = 16,
  height = 12,
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
