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
# Notes ---

# This script is to fulfill a request from SBC to do a quick and dirty table for early Jan
# With the following columns:
#   - DB Name
#   - CSD name
#   - Estimated population (2025, 30, 35)
#   - Nearest office (option 1: existing 65 offices, option 2: existing 65 offices PLUS a pilot office)
#   - Travel distance to nearest office (DB centroid method - so won't actually be travel distance)
#   - Urban/rural label (based on service population)

# =========================================================================== #

# =========================================================================== #
# Load libraries and settings ----
# =========================================================================== #

source("R/settings.R")
source("R/fxns/rural-fxns.R")
source("R/fxns/calculations.r")

# =========================================================================== #
# Load data ----
# =========================================================================== #

# Locations of all Service BC locations
sbc_locs <- read_csv(glue("{SRC_DATA_FOLDER}/full-service-bc-locs.csv")) |>
  st_as_sf(
    coords = c('coord_x', 'coord_y'),
    crs = 3005
  )

# Include the pilot location
pilot_locs <- read_csv(glue("{SRC_DATA_FOLDER}/pilot-service-bc-locs.csv")) |>
  st_as_sf(
    coords = c('coord_x', 'coord_y'),
    crs = 4326 # this is regular lat long - need to conver it to BC ALBERS
  ) |>
  st_transform(3005)

full_pilot_locs <- sbc_locs |>
  select(nearest_facility, geometry) |>
  bind_rows(pilot_locs)

# DB shapefiles
db_shapefile <- st_read(glue("{SHAPEFILE_OUT}/full-db-with-location.gpkg")) |>
  # required for rural function to work properly
  rename(geometry = geom) |>
  st_transform(crs = 3005) |>
  select(dbid, csdid, geometry)

# Crosswalk to include CSDIDs and names of DBs
crosswalk <-
  read_csv(
    glue("{SRC_DATA_FOLDER}/csd-da-db-loc-correspondance.csv"),
    col_types = cols(.default = "c")
  ) |>
  clean_names()

# population centers for rural/urban flags
popcenter_boundaries <-
  st_read(
    glue("{SRC_DATA_FOLDER}/shapefiles/popcenter-statscan.gpkg"),
    layer = "popcenter_statscan"
  ) |>
  rename(geometry = geom) |>
  st_transform(crs = 3005)

# db population projections
db_projections_transformed <- readRDS(glue(
  "{SRC_DATA_FOLDER}/full-db-projections-transformed.rds"
))

# ----------------------------------------------------------------------------
# Assign DB to nearest SBC location according to closest centroid
# ----------------------------------------------------------------------------

# Assign all unassigned DBs directly to the nearest facility

# in this case, every DB should be unassigned, so just send in an empty tibble
# so that no DBs get filtered out
assigned_facility <- tibble(dbid = as.character(), assigned = as.character())

tictoc::tic()
complete_assignments <- assign_unassigned_dbs(
  db_shapefile,
  assigned_facility,
  facility_locations = sbc_locs,
  batch_size = 20000,
  verbose = TRUE
)
tictoc::toc()
# 580.35 sec elapsed


# Do again, but with the pilot location included
tictoc::tic()
pilot_assignments <- assign_unassigned_dbs(
  db_shapefile,
  assigned_facility,
  facility_locations = full_pilot_locs,
  batch_size = 20000,
  verbose = TRUE
)
tictoc::toc()
# 880.97 sec elapsed

# ----------------------------------------------------------------------------
# Population Estimates for each DB
# ----------------------------------------------------------------------------

# get DB projections rolled up to the total for the 3 years of interest
population_estimates_three_year <- crosswalk |>
  distinct(dbid, csdid, csd_name) |>
  # expand drivetime data to include all years of interest for each row
  expand_grid(tibble(year = rep(c(CURRENT_YEAR, CURRENT_YEAR+5, CURRENT_YEAR+10)))) |>
  left_join(
    db_projections_transformed |>
      filter(gender == "T") |>
      select(dbid, year, age, population, total),
    by = c("dbid", "year")
  ) |>
  filter(!is.na(csdid)) |>
  group_by(dbid, csdid, csd_name, year) |>
  summarize(pop = sum(population, na.rm=TRUE)) |>
  ungroup() |>
  pivot_wider(
    names_from = year,
    values_from = pop,
    values_fill = 0
  ) |>
  rename(
    !!paste0("estimated_population_", CURRENT_YEAR) := !!as.name(CURRENT_YEAR),
    !!paste0("5_yr_projection_", CURRENT_YEAR + 5) := !!as.name(
      CURRENT_YEAR + 5
    ),
    !!paste0("10_year_projection_", CURRENT_YEAR + 10) := !!as.name(
      CURRENT_YEAR + 10
    )
  )

# ----------------------------------------------------------------------------
# Rural and urban flags for each DB
# ----------------------------------------------------------------------------

popcenter_population <- assign_region(
  db_shapefile,
  popcenter_boundaries,
  "dbid",
  "pcname"
) |>
  rename("p_area_coverage" = area_ratio) |>
  mutate(
    urban_rural = case_when(
      is.na(p_area_coverage) ~ "RURAL",
      as.numeric(p_area_coverage) <= 0.3 ~ "RURAL",
      TRUE ~ "URBAN"
    )
  ) |> select(dbid, urban_rural)

# ----------------------------------------------------------------------------
# Bring everything together
# ----------------------------------------------------------------------------

summary_table <- population_estimates_three_year |>
  left_join(
    complete_assignments |>
    select(dbid, nearest_office_original_65=assigned, travel_distance_original_65=min_distance),
    by = 'dbid'
  ) |>
  left_join(
    pilot_assignments |>
    select(dbid, nearest_office_pilot_66=assigned, travel_distance_pilot_66=min_distance),
    by = 'dbid'
  ) |>
  left_join(
    popcenter_population,
    by = 'dbid'
  )

# quick scan that the movement to the pilot seems reasonable
summary_table |>
  select(dbid, csd_name, nearest_office_original_65, nearest_office_pilot_66) |>
  filter(nearest_office_original_65 != nearest_office_pilot_66) |>
  count(nearest_office_original_65, nearest_office_pilot_66)

# check it out on a map for further reassurance

for_plt <- db_shapefile |>
  left_join(summary_table, by = 'dbid') |>
  mutate(Assignment = if_else(
    nearest_office_pilot_66 %in% pilot_locs$nearest_facility,
    nearest_office_original_65,
  NA_character_))

for_plt_pts <- full_pilot_locs |>
  mutate(Facility = if_else(
    nearest_facility %in% pilot_locs$nearest_facility,
  'Pilot',
  'Original')
)

ggplot2::ggplot() +
    geom_sf(
      data = for_plt,
      aes(fill=Assignment),
      linewidth = 0.7
    ) +
  # Add Service BC locations
    geom_sf(
      data = for_plt_pts,
      aes(color=Facility),
      size = 2,
      stroke = 1.1
    ) +
  scale_color_manual(
    values = c(
      "Original" = "#006d2c",
      "Pilot" = "#cb181d"
    )
  ) +
  coord_sf(
    xlim = c(1250000, 1300000),
    ylim = c(450000, 500000),
    expand = FALSE
  )


# save table for SBC
# Create output directory if it doesn't exist
if (!dir.exists(FOR_SBC_OUT)) {
  dir.create(FOR_SBC_OUT, recursive = TRUE)
}

# Write combined statistics table
fn <- glue::glue("sbc-pilot_statistics-db-centroid-method-{Sys.Date()}.csv")

summary_table |>
  mutate(across(where(is.double), ~ round(., 1))) |>
  write_csv(file.path(FOR_SBC_OUT, fn))
