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

# Description: Creates metrics for each individual SBC that was included in the pilot.
# Note that each individual SBC may 'service' more areas than the regions they are assigned to
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
source("R/fxns/pre-processing.R")
source("R/fxns/plots.R")


# =========================================================================== #
# Read required data in ----
# =========================================================================== #
## drive time data ----
## from BCDS
drive_data <- read_csv(RAW_PROVINCE_ADDRESS_FILEPATH) |> clean_names()

## SBC locations to include ----
## from source folder
sbc_locs <- read_csv(SBCLOC_FILEPATH) |> 
  mutate(across(c(loc), as.character)) |>  # Explictly declare data types on join columns
  st_as_sf(coords = c("coord_x", "coord_y"), crs = 3005)

## population projections ----
## from catalogue

# figure out ID of file we want
bcdc_search('sub-provincial projections')
# population projections: 86839277-986a-4a29-9f70-fa9b1166f6cb
#       - csd resource: 0e15d04d-127c-457a-b999-20800c929927
# household estimates: 2a8ddf6c-dfb9-4187-a66d-9bb15b15ea83
# note that these don't have gender/age breakdowns, which we will want 

pop_projections <- bcdc_get_data(
  '86839277-986a-4a29-9f70-fa9b1166f6cb',
  resource = '0e15d04d-127c-457a-b999-20800c929927'
  ) |> 
  janitor::clean_names() |> 
  mutate(region = 
           paste0(
             '59',
              str_pad(as.character(region), width=5, side='left', pad='0')
             )
  )

pop_projections

## census populations ----
## from statscan 
pop_db <- cancensus::get_census(
  dataset = "CA21", # 2021 census
  regions = list(PR = "59"), # grab only BC
  level = 'DB' # at dissemination block level 
) %>%
  clean_names()

## db shapefiles 
db_shapefiles <- bcdc_query_geodata('76909e49-8ba8-44b1-b69e-dba1fe9ecfba') |> 
  collect() |> 
  clean_names() |> 
  select(dissemination_block_id, geometry)

# =========================================================================== #
# DB population projections ----
# =========================================================================== #

# we want to convert the CSD population projections into DB projections
# we do this by approximation, and assume that the proportion of the DB that makes
# up the CSD each year doesn't change
# while not perfect, it will give reasonable estimates

# first, we create a new 'csd_clean' label to match the population projections
# as some CSDs are rolled up in the projections
get_clean_csd <- pop_db |> 
  left_join(pop_projections |> distinct(region) |> mutate(in_projections=1), by=c('csd_uid'='region')) |> 
  # if rolled up, last 3 digits replaced with '999'
  mutate(
    csd_clean = if_else(
      is.na(in_projections), paste0(str_sub(csd_uid, 1, 4), '999'),
      csd_uid
    )
  )

# check to see if any regions no longer align (should be empty)
get_clean_csd |> 
  left_join(pop_projections |> distinct(region) |> mutate(test_in = 1), by=c('csd_clean'='region')) |> 
  filter(is.na(test_in))

# now get pct of each 'clean' csd that is taken up by each DB
prop_of_csd <- get_clean_csd |> 
  group_by(csd_clean) |> 
  mutate(csd_population = sum(population)) |> 
  ungroup() |> 
  mutate(pct_of_csd = population/csd_population)

# join back to projections to get yearly estimates for each age, gender, year of interest
db_projections <- prop_of_csd |> 
  select(geo_uid, da_uid, csd_uid, csd_clean, area_sq_km, population, csd_population, dwellings, households, pct_of_csd) |> 
  left_join(
    pop_projections |> 
      filter(year %in% c(CURRENT_YEAR, CURRENT_YEAR + 5, CURRENT_YEAR+10)),
    by=c('csd_clean' = 'region'),
    relationship = 'many-to-many'
    )

# this table now has a pct of csd column that we can use to multiple by any other population projections to get estimates
db_projections

# =========================================================================== #
# Metrics for SBC locations of interest ----
# metrics of interest include: 
# - histograms or ridgelines of drive times/distances
# - map that has all the 'closest' dbs 
# - demographic estimates at DB level
# =========================================================================== #

# first assign every dissemination block a 'nearest facility'
assigned_facility <- drive_data |> 
  count(dissemination_block_id, nearest_facility) |> 
  arrange(dissemination_block_id, desc(n)) |> 
  group_by(dissemination_block_id) |> 
  slice_head(n=1) |>
  rename(assigned=nearest_facility) |> 
  select(-n) |> 
  ungroup()

# filter to only a couple of facilities
drive_data <- drive_data |> left_join(assigned_facility, by = 'dissemination_block_id')

drive_data_reduced <- drive_data |>  
  filter(assigned %in% (sbc_locs |> pull(nearest_facility))) |> 
  filter(nearest_facility %in% (sbc_locs |> pull(nearest_facility)))

# step 1: create a histogram/density plot for each facility
ggplot(
  drive_data |> 
    group_by(assigned) |> 
    mutate(drv_dist_mean = mean(drv_dist)) |> 
    ungroup() |> 
    mutate(assigned = fct_reorder(assigned, desc(drv_dist_mean))),
  aes(x = drv_dist, y=assigned, fill=assigned)
) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = 'none')

# step 2: map of all closest DBs/average drive distances
map_data <- drive_data_reduced |> 
  mutate(dissemination_block_id = as.character(dissemination_block_id)) |> 
  group_by(dissemination_block_id, nearest_facility) |> 
  summarize(drive_dist_mean = mean(drv_dist)) |> 
  ungroup() |> 
  left_join(db_shapefiles, by=c('dissemination_block_id')) |> 
  st_as_sf(crs = 3005)

var <- 'drive_dist_mean'
loc_id <- 'Service BC - Victoria'
loc_col <- 'nearest_facility'
plot_title <- glue('Driving Distances for {loc_id}')
var_title <- 'Driving Distance (km)'

map_plot <- build_map(
  data = map_data,
  servicebc_data = sbc_locs,
  varname = var,
  loc_id = loc_id,
  loc_col = loc_col,
  map_theme = MAP_THEME,
  fill_scale = FILL_THEME,
  plot_title = plot_title,
  #plot_subtitle = plot_subtitle,
  legend_title = var_title
)

map_plot
