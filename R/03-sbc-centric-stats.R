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
# Script: 03-sbc-centric-stats.R

# Description: Creates metrics for each individual SBC
# Note that each individual SBC may 'service' more areas
# than the regions they are assigned to
# So numbers between region averages and SBC averages likely vary.

# Requirements:
#   - Requires necessary R packages (e.g., `tidyverse`, `purrr`, `glue`).
#   - Depends on `settings.R` for configuration constants.
#   - Depends on the `preprocess_locs` function to perform preprocessing steps.
#   - Requires appropriately named input files in the raw data folder and
#     read/write access to the relevant data folders.

# Side Effects/Outputs:
#   - Writes processed CSV files and plots to the outputs folder.
#   - Prints status messages, warnings (e.g., locality mismatches, overwrites),
#     or errors to the console during execution.
# =========================================================================== #

# =========================================================================== #
# Load libraries and settings ----
# =========================================================================== #
library(tidyverse)
library(glue)
library(janitor)
library(e1071)
library(sf)
library(bcdata)
library(ggridges)

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
  mutate(csd_population = sum(population)) |>
  ungroup() |>
  mutate(pct_of_csd = population / csd_population)

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
# to multiple by any other population projections to get estimates
db_projections_transformed

# =========================================================================== #
# Metrics for SBC locations of interest ----
# metrics of interest include:
# - histograms or ridgelines of drive times/distances
# - map that has all the 'closest' dbs
# - demographic estimates at DB level
# =========================================================================== #

# first assign every dissemination block a 'nearest facility'
assigned_facility <- drivetime_data |>
  count(dbid, nearest_facility) |>
  arrange(dbid, desc(n)) |>
  group_by(dbid) |>
  slice_head(n = 1) |>
  rename(assigned = nearest_facility) |>
  select(-n) |>
  ungroup()

# filter to only a couple of facilities
drivetime_data <- drivetime_data |> left_join(assigned_facility, by = "dbid")

drivetime_data_reduced <- drivetime_data |>
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
         pull(nearest_facility))
  ) |>
  left_join(crosswalk, by = c("dbid", "daid"))

## SBC centric measures (table form) ----
# drive measures
drive_measures <- drivetime_data_reduced |>
  group_by(assigned) |>
  summarize(
    n_addresses = n(),
    mean_drv_time = mean(drv_time_sec),
    sd_drv_time = sd(drv_time_sec),
    min_drv_time = min(drv_time_sec),
    max_drv_time = max(drv_time_sec),
    mean_drv_dist = mean(drv_dist),
    sd_drv_dist = sd(drv_dist),
    min_drv_dist = min(drv_dist),
    max_drv_dist = max(drv_dist)
  ) |>
  ungroup()

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
    pct_of_csd = sum(pct_of_csd)
  ) |>
  ungroup() |>
  arrange(assigned, desc(pct_of_csd))

csds_serviced

# total population, average/median age
pop_measures <- drivetime_data_reduced |>
  distinct(assigned, csdid, dbid) |>
  left_join(
    db_projections_transformed %>% 
    filter(gender=='T') %>% 
    select(dbid, year, age, population, total), 
    by = "dbid"
    ) |>
  filter(!is.na(csdid)) %>% 
  group_by(assigned, year) %>% 
  summarize(
    n_dbs = n_distinct(dbid),
    n_csds = n_distinct(csdid),
    est_pop = sum(population),
    avg_age = weighted.mean(age, population),
    median_age = median(rep(age, times = round(population))), # Use exact ages instead of bins
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
    n_dbs, n_csds, n_majority_csds, 
    est_pop, avg_age, median_age
  )

# look at some weird percents, eg esquimalt
# likely due to missing DBs in the data?
drivetime_data |>
  left_join(crosswalk, by = c("dbid", "daid")) |>
  filter(csd_name == "Esquimalt") |>
  distinct(nearest_facility)

crosswalk |>
  left_join(drivetime_data, by = c("dbid", "daid")) |>
  filter(csd_name == "Esquimalt") |>
  filter(is.na(nearest_facility))


# SAVE ALL TABLES
drive_measures
csds_serviced
pop_measures

# plots ----
# step 1: create a histogram/density plot for each facility
ggplot(
  drivetime_data_reduced |>
    group_by(assigned) |>
    mutate(drv_dist_mean = mean(drv_dist)) |>
    ungroup() |>
    mutate(
      assigned = fct_reorder(assigned, desc(drv_dist_mean)),
      drv_dist = if_else(drv_dist == 0, 0.01, drv_dist)
      ),
  aes(x = drv_dist, y = assigned, fill = assigned, height = after_stat(density))
) +
  geom_density_ridges(
    stat = "binline",
    draw_baseline = FALSE,
    alpha = 0.3,
    binwidth = 3
  ) +
  xlim(0, 150) +
  labs(
    x = "Drive Distance (km)",
    title = "Driving Distances to Service BC Locations"
  ) +
  theme_ridges(center_axis_labels=TRUE) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 14),
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.text.y = element_text(),
    plot.title.position = "plot"
  )

# step 2: map of all closest DBs/average drive distances
map_data <- drivetime_data_reduced |>
  mutate(dbid = as.character(dbid)) |>
  group_by(dbid, nearest_facility) |>
  summarize(drive_dist_mean = mean(drv_dist)) |>
  ungroup() |>
  left_join(db_shapefiles, by = c("dbid")) |>
  st_as_sf(crs = 3005)

var <- "drive_dist_mean"
loc_id <- "Service BC - Victoria"
loc_col <- "nearest_facility"
plot_title <- glue("Driving Distances for {loc_id}")
var_title <- "Driving Distance (km)"

map_plot <- build_map(
  data = map_data,
  servicebc_data = sbc_locs,
  varname = var,
  csd_name = loc_id,
  csd_col = loc_col,
  map_theme = MAP_THEME,
  fill_scale = FILL_THEME,
  plot_title = plot_title,
  # plot_subtitle = plot_subtitle,
  legend_title = var_title
)

map_plot

# =========================================================================== #
# Population Pyramid Creation ----
# =========================================================================== #

# Prepare data for population pyramids by joining with location assignment data
pyramid_data <- drivetime_data_reduced |>
  distinct(assigned, dbid) |>
  inner_join(
    db_projections_transformed |>
      filter(year %in% c(CURRENT_YEAR, CURRENT_YEAR + 5, CURRENT_YEAR + 10)),
    by = "dbid"
  )

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

# Save all pyramids to files
if (!dir.exists("outputs/population_pyramids")) {
  dir.create("outputs/population_pyramids", recursive = TRUE)
}

for (facility in facilities) {
  ggsave(
    filename = glue("outputs/population_pyramids/{facility}_population_pyramid.png"),
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
    plotlist = population_pyramids[1:min(4, length(facilities))],
    ncol = 2
  )
  
  ggsave(
    filename = "outputs/population_pyramids/combined_population_pyramids.png",
    plot = combined_pyramids,
    width = 16,
    height = 12,
    dpi = 300
  )
}
