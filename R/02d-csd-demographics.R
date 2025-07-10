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
# Load libraries and settings ----
# =========================================================================== #

source("R/settings.R")
source("R/fxns/plots.R")

# =========================================================================== #
# Read required data
# =============================================== #
## drive time data from BCDS
drivetime_data <-
  read_csv(
    glue("{SRC_DATA_FOLDER}/reduced-drivetime-data.csv"),
    col_types = cols(.default = "c")
  ) %>%
  clean_names() %>%
  mutate(across(c(drv_time_sec, drv_dist), as.numeric))

## SBC locations to include from source folder
sbc_locs <- read_csv(glue("{SRC_DATA_FOLDER}/reduced-service_bc_locs.csv"),
     col_types = cols(.default = "c")
  ) |>
  st_as_sf(coords = c("coord_x", "coord_y"), crs = 3005) |>
  filter(csd_name %in% CSD_NAMES)


## population projections from catalogue
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

## census populations
pop_db <- read_csv(
  glue("{SRC_DATA_FOLDER}/reduced-population-db.csv"),
  col_types = cols(.default = "c")
) %>%
  clean_names() %>%
  mutate(across(c(area_sq_km, population, dwellings, households), as.numeric))

## census populations
pop_csd <- read_csv(
  glue("{SRC_DATA_FOLDER}/reduced-population-csd.csv"),
  col_types = cols(.default = "c")
) %>%
  clean_names() %>%
  mutate(across(c(area_sq_km, population, dwellings, households), as.numeric))

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
  left_join(crosswalk, by = c("dbid", "daid", "csdid", "csd_name", "csd_desc")) |>
  inner_join(
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
      filter(year %in% c(CURRENT_YEAR, CURRENT_YEAR + 5, CURRENT_YEAR + 10)) |>
      filter(region %in% CSDIDS),
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
# Population estimates
# =========================================================================== #

summary_stats <- db_projections_transformed |> 
  filter(year == 2025, gender == 'T') |>
  summarise(population = sum(population), .by = c(age, region_name)) |>
    group_by(region_name) |>
    summarise(
      est_population = sum(population, na.rm = TRUE),
      median_age = median(rep(age, population)))

write_csv(summary_stats, glue("{TABLES_OUT}/csd_population_metrics.csv"))

# =========================================================================== #
# Population Pyramid Creation ----
# =========================================================================== #

# Prepare data for population pyramids by joining with location assignment data
pyramid_data <- db_projections_transformed |>
  filter(year %in% c(CURRENT_YEAR, CURRENT_YEAR + 5, CURRENT_YEAR + 10)) |>
  filter(csdid %in% CSDIDS) |>
  # Join with crosswalk or directly with sbc_locs to get csd assignment
  inner_join(
    sbc_locs |>
      st_drop_geometry() |>
      filter(csdid %in% CSDIDS) |>
      select(csdid, nearest_facility, csd_name),
    by = c("csdid","csd_name")
  ) |>
  # Rename to match existing code structure
  rename(assigned = csd_name)

# Get unique csd names to generate pyramids for
regions <- unique(pyramid_data$assigned)

# Generate population pyramids for each csd
population_pyramids <- list()
for (csd in regions) {
  population_pyramids[[csd]] <- create_population_pyramid(
    data = pyramid_data,
    location_name = csd
  )
}

# Display a sample pyramid for the first csd
if (length(regions) > 0) {
  print(population_pyramids[[regions[1]]])
}

# Create a folder for drive distance maps
pyramid_folder <- file.path(MAP_OUT, "csd_population_pyramids")
if (!dir.exists(pyramid_folder)) {
  dir.create(pyramid_folder, recursive = TRUE)
}

for (csd in regions) {
  ggsave(
    filename = glue("{pyramid_folder}/{csd}_population_pyramid.png"),
    plot = population_pyramids[[csd]],
    width = 10,
    height = 8,
    dpi = 300
  )
}

# Create a combined view with multiple pyramids
# Take up to 4 facilities for a nice grid layout
if (length(regions) > 1) {
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
