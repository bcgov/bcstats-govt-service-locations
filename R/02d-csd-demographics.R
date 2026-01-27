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
source("R/fxns/csd-plots.R")

# =========================================================================== #
# Read required data
# =============================================== #

## SBC locations to include from source folder
sbc_locs <- read_csv(
  glue("{SRC_DATA_FOLDER}/reduced-service-bc-locs.csv"),
  col_types = cols(.default = "c")
) |>
  st_as_sf(coords = c("coord_x", "coord_y"), crs = 3005) |>
  filter(csd_name %in% CSD_NAMES)


## population projections from catalogue
pop_projections <- read_csv(glue(
  "{SRC_DATA_FOLDER}/full-population-projections.csv"
)) |>
  mutate(region = as.character(region))

## census populations
pop_db <- read_csv(
  glue("{SRC_DATA_FOLDER}/reduced-population-db.csv"),
  col_types = cols(.default = "c")
) |>
  clean_names() |>
  mutate(across(c(area_sq_km, population, dwellings, households), as.numeric))

## crosswalk (entire province, not filtered)
crosswalk <-
  read_csv(
    glue("{SRC_DATA_FOLDER}/csd-da-db-loc-correspondance.csv"),
    col_types = cols(.default = "c")
  ) |>
  clean_names()

# =========================================================================== #
# DB population projections ----
# filtering on CSDIDS pulls in db's that are not in our data
# =========================================================================== #

db_projections_transformed <- readRDS(glue(
  "{SRC_DATA_FOLDER}/full-db-projections-transformed.rds"
))
db_projections_transformed <- db_projections_transformed |>
  filter(csdid %in% CSDIDS)

# =========================================================================== #
# Population estimates
# =========================================================================== #

summary_stats <- db_projections_transformed |>
  filter(year == CURRENT_YEAR, gender == 'T') |>
  summarise(population = sum(population), .by = c(age, region_name)) |>
  group_by(region_name) |>
  summarise(
    est_population = sum(population, na.rm = TRUE),
    median_age = median(rep(age, population))
  )

write_csv(
  summary_stats,
  glue("{TABLES_OUT}/reduced-csd_population_metrics.csv")
)

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
    by = c("csdid", "csd_name")
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
# Take up to 4 regions for a nice grid layout
if (length(regions) > 1) {
  combined_pyramids <- cowplot::plot_grid(
    plotlist = population_pyramids[seq_len(min(4, length(regions)))],
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

rm(list = ls())
gc()
