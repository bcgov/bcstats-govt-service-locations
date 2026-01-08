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
# Self-contained Dissemination Block (DB) centroid assignment script

# This script performs a quick analysis by assigning each DB to the
# nearest Service BC (SBC) facility using simple centroid proximity. It then allocate
# population projections to DBs, rolls them up to facilities, and writes 2 CSVs plus
# a facility catchment shapefile.
#
# Inputs:
#   1) A CSV containing SBC facility names and latitude/longitude columns (EPSG:4326)
#   2) DB shapefiles from the BC Data Catalogue (downloaded automatically)
#   3) BC population projections from the BC Data Catalogue (downloaded automatically)
#   4) Census population data from Cancensus (downloaded automatically; requires API key)
#      Steps to Obtain Your API Key:
#      Visit the CensusMapper website: Go to the CensusMapper sign-up page.
#      Create an account: Register for a free account.
#      Access your profile: Once signed in, navigate to the "Edit Profile" section.
#      Retrieve the key: Your API key will be displayed there.
#      Once you have your key, you can store it in your system environment so it
#      is automatically used in API calls.
#      In R (using the cancensus package): The recommended way to set the key permanently
#      is by using the set_cancensus_api_key() function in R with your actual key:
#      cancensus::set_cancensus_api_key("YOUR_KEY_HERE", install = TRUE)
#      The script can then use Sys.getenv("CM_API_KEY")
#    5) CSD rural/urban identification from Rural Initiatives Excel matrix

# Outputs (written to a user-specified directory that will be created if missing):
#   - CSV 1 (DB assignments list - which DB gets assigned to which SBC facility using the centroid only method):
#            dbid | csdid | csd_name | pop_2025 | pop_2030 | pop_2035 |
#            assigned_facility | centroid_distance_m | urban_rural
#   - CSV 2 (SBC facility population projections - once every DB is rolled up into its assigned SBC facility,
#            what are the associated population demographics?):
#            facility | pop_2025 | pop_2030 | pop_2035 |
#            est_population_0_to_14_yrs | est_population_15_to_24_yrs |
#            est_population_25_to_64_yrs | est_population_over_64_years |
#            median_age | mean_age
#   - Shapefile: SBC-facility-level polygons representing aggregated DB catchments
#
# Facility CSV requirements:
#   - One row per facility
#   - Must contain exactly these columns:
#       - 'nearest_facility': (facility name)
#       - 'coord_x': (longitude, EPSG:4326)
#       - 'coord_y': (latitude, EPSG:4326)
#
# Notes/limitations:
#   - Assignment uses centroid distance in BC Albers (EPSG:3005), not road travel distance/time.
#   - Urban/rural is joined from the Rural Initiatives Excel matrix at the CSD level
#     ("Census Subdivision Data" sheet, "Rural Category" field). This script does not
#     compute urban/rural from DB density or StatCan population centres.

# Install required packages (only need to install once, then comment out)
# installed_packages(c(
#   "bcdata",
#   "sf",
#   "dplyr",
#   "readr",
#   "tidyr",
#   "purrr",
#   "stringr",
#   "glue",
#   "units",
#   "bcmaps",
#   "janitor",
#   "ggplot2",
#   "cancensus"
#   "readxl"
#   "safepaths",
#   "spatstat"
#   "rmapshaper"
# ))

# Load the installed packages
suppressPackageStartupMessages({
  library(bcdata)
  library(sf)
  library(dplyr)
  library(readr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(glue)
  library(units)
  library(bcmaps)
  library(readxl)
  library(janitor)
  library(ggplot2)
  library(cancensus)
  library(safepaths)
  library(spatstat)
  library(rmapshaper)
})
# =========================================================================== #
# Helper functions
# ------------------------------------------------------------------------
# Function: assign_dbs
#
# Description: Assigns each dissemination block (DB) directly to the
# nearest Service BC facility location based on centroid distance.
#
# Inputs:
#   - db_shapefile: An sf object containing all DB geometries with `dbid` column
#   - assigned_facility: Data frame with columns `dbid` and `assigned`
#   - facility_locations: An sf object containing Service BC facility locations (EPSG:3005)
#   - verbose: Whether to print progress messages (default TRUE)
#
# Outputs:
#   - Returns an updated data frame with DB assignments
#
# Assumptions:
#   - db_shapefile must be an sf object with spatial geometries
#   - facility_locations must be an sf object with point geometries and a
#     'nearest_facility' column identifying each location
# ------------------------------------------------------------------------
assign_dbs <- function(
  db_shapefile,
  assigned_facility,
  facility_locations,
  verbose = TRUE
) {
  db_shapefile_with_assignment <- db_shapefile |>
    left_join(assigned_facility, by = "dbid")

  unassigned_dbs <- db_shapefile_with_assignment |>
    filter(is.na(assigned)) |>
    select(-assigned) # Remove NA assigned column

  if (nrow(unassigned_dbs) == 0) {
    if (verbose) {
      message("No unassigned DBs found.")
    }
    return(assigned_facility)
  }

  # If no facilities (unlikely), return empty assignments
  if (nrow(facility_locations) == 0) {
    warning("No facility locations provided.")
    return(assigned_facility)
  }

  # make sure that the facility locations are unique
  facility_locations <- facility_locations %>%
    distinct(nearest_facility, .keep_all = TRUE)

  # Report the scale of the task
  if (verbose) {
    message("Processing ", nrow(unassigned_dbs), " unassigned DBs")
    message(
      "Finding nearest facility among ",
      nrow(facility_locations),
      " locations"
    )
  }

  # Ensure consistent CRS
  if (!identical(st_crs(unassigned_dbs), st_crs(facility_locations))) {
    if (verbose) {
      message("Transforming geometries to ensure consistent CRS...")
    }
    facility_locations <- st_transform(
      facility_locations,
      st_crs(unassigned_dbs)
    )
  }

  # Calculate centroids for all unassigned DBs
  if (verbose) {
    message("Calculating centroids for unassigned DBs...")
  }
  unassigned_centroids <- st_centroid(unassigned_dbs)

  # get index of nearest facility using vectorized sf operations:
  near_idx <- st_nearest_feature(
    unassigned_centroids,
    facility_locations
  )

  # use calculated indexes to assign a facility to each location
  new_assignments <- unassigned_centroids |>
    mutate(
      assigned = facility_locations$nearest_facility[near_idx],
      assignment_method = 'nearest_facility',
      min_distance = as.numeric(st_distance(
        geometry,
        facility_locations$geometry[near_idx],
        by_element = TRUE
      ))
    ) |>
    st_drop_geometry() |>
    select(dbid, assigned, assignment_method, min_distance)

  # Combine original assignments with new ones
  complete_assignments <- assigned_facility |>
    mutate(
      assignment_method = "drive_time",
      min_distance = NA_real_
    ) |>
    bind_rows(new_assignments)

  return(complete_assignments)
}
# =========================================================================== #

# =========================================================================== #
# Parameters
# =========================================================================== #
# default current and projection years
CURRENT_YEAR <- 2025
PROJECTION_YEARS <- c(CURRENT_YEAR, CURRENT_YEAR + 5, CURRENT_YEAR + 10)
# define census year for Cancensus data
CANCENSUS_YEAR <- "CA21"

# =========================================================================== #
# Inputs (set these before running the script)
# =========================================================================== #

# file paths, could put this into settings.R, but as a standalone script, define here
LAN_FOLDER <- use_network_path()
TEST_DATA_FOLDER <- glue(
  "{LAN_FOLDER}/2025 Government Service Locations/data/test"
)
TEST_OUT <- glue(
  "{LAN_FOLDER}/2025 Government Service Locations/outputs/test_07_output"
)

# file path to CSV containing SBC facility names and latitude/longitude columns (EPSG:4326)
facilities_csv <- glue("{TEST_DATA_FOLDER}/full-service-bc-locs-wgs84.csv")

# desired output directory
output_directory <- glue("{TEST_OUT}")

# create output directory
message("Creating output directory at ", output_directory)
dir.create(output_directory, showWarnings = FALSE, recursive = TRUE)

# read in facility data
message("Reading facilities from ", facilities_csv)
facilities_raw <- read_csv(facilities_csv, show_col_types = FALSE)

# check if required columns are in the provided CSV file, and column names are correct
required_fac_cols <- c("nearest_facility", "coord_x", "coord_y")
missing_fac <- setdiff(required_fac_cols, names(facilities_raw))
if (length(missing_fac) > 0) {
  stop("Facility CSV missing columns: ", paste(missing_fac, collapse = ", "))
}

# transform facility data to sf object
# since the facilities_raw has regular longitude/latitude, need to convert it to BC Albers
facilities <- facilities_raw |>
  st_as_sf(coords = c("coord_x", "coord_y"), crs = 4326) |>
  st_transform(3005) |>
  distinct(nearest_facility, .keep_all = TRUE)

# read in DB shapefiles from BC data catalogue
message(
  "Downloading dissemination block geometries from BCDC (this may take a few minutes)..."
)
db_shapefiles <- bcdc_query_geodata('76909e49-8ba8-44b1-b69e-dba1fe9ecfba') |>
  collect() |>
  clean_names() |>
  select(
    dbid = dissemination_block_id,
    daid = dissemination_area_id,
    csdid = census_subdivision_id,
    landarea = feature_area_sqm,
    geometry
  ) |>
  mutate(
    dbid = as.character(dbid),
    csdid = as.character(csdid),
    across(c(landarea), as.numeric)
  ) |>
  st_as_sf() |>
  st_transform(3005)

# read in CSD shapefiles from bcmaps
csd_shapefiles <- census_subdivision() |>
  clean_names() |>
  select(
    csdid = census_subdivision_id,
    csd_name = census_subdivision_name,
    csd_desc = census_subdivision_type_desc,
    landarea = feature_area_sqm,
    geometry
  )

# read in population projections from BC data catalogue
message("Downloading population projections from BC data catalogue...")
pop_projections <- bcdc_get_data(
  "86839277-986a-4a29-9f70-fa9b1166f6cb",
  resource = "0e15d04d-127c-457a-b999-20800c929927"
) |>
  clean_names() |>
  mutate(
    region = paste0(
      "59",
      str_pad(as.character(region), width = 5, side = "left", pad = "0")
    )
  )

# read in DB & CSD population data from Cancensus
pop_db <- get_census(
  dataset = CANCENSUS_YEAR,
  regions = list(PR = "59"), # grab only BC
  level = 'DB'
) |>
  clean_names() |>
  rename(dbid = geo_uid)

# read in CSD rural/urban identification from Rural Initiatives file from Ministry of Jobs and Economic Growth
# https://intranet.gov.bc.ca/economy-sector/jeg/rural-initiative/rural-data
csd_rural_category <- read_excel(
  glue("{TEST_DATA_FOLDER}/rural_matrix_list_of_communities.xlsx"),
  sheet = "Census Subdivision Data"
) |>
  transmute(
    csdid = as.character(CSDuid),
    csd_name_excel = as.character(Name),
    urban_rural = as.character(`Rural Category`)
  )

# make a correspondence file mapping each DB to its CSD context, so we can pull
# CSD names and types when building outputs and roll-ups.
crosswalk <- db_shapefiles |>
  st_drop_geometry() |>
  select(-landarea) |>
  # get csd names from csd shapefile
  left_join(
    csd_shapefiles |> st_drop_geometry() |> select(-landarea),
    by = "csdid"
  )

# =========================================================================== #
# DB population projections ----
# we want to convert the CSD population projections into DB projections
# we do this by approximation,
# and assume that the proportion of the DB that makes
# up the CSD each year doesn't change
# while not perfect, it will give reasonable estimates
# has a pct of csd column that we can use
# to multiply by any other population projections to get estimates
# =========================================================================== #

# first, we create a new 'csd_clean' label to match the population projections
# as some CSDs are rolled up in the projections
get_clean_csd <- pop_db |>
  left_join(crosswalk, by = c("dbid")) |>
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

# now get pct of each 'clean' csd that is taken up by each DB
prop_of_csd <- get_clean_csd |>
  group_by(csd_clean) |>
  mutate(csd_population = sum(population, na.rm = TRUE)) |>
  ungroup() |>
  mutate(pct_of_csd = if_else(population == 0, 0, population / csd_population))

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
    # Calculate population estimate for each DB
    population = population_by_age * pct_of_csd,
    total = total * pct_of_csd
  )


# =========================================================================== #
# Assign DB to nearest SBC facility location according to closest centroid
# every DB should be unassigned, so just send in an empty tibble
# so that no DBs get filtered out
# =========================================================================== #
message("Assigning DBs to nearest facility using centroids...")
facility_empty_tibble <- tibble(
  dbid = as.character(),
  assigned = as.character()
)

db_assignments_raw <- assign_dbs(
  db_shapefile = db_shapefiles,
  assigned_facility = facility_empty_tibble,
  facility_locations = facilities,
  verbose = TRUE
)

db_assignments <- db_assignments_raw |>
  rename(
    assigned_facility = assigned,
    centroid_distance_m = min_distance
  )

# db-level population estimates for the 3 projection years
db_population_estimates_three_year <- db_projections_transformed |>
  filter(gender == "T", year %in% PROJECTION_YEARS) |>
  group_by(dbid, year) |>
  summarize(pop = sum(population, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = year, values_from = pop, values_fill = 0) |>
  rename(
    pop_2025 = !!as.name(as.character(CURRENT_YEAR)),
    pop_2030 = !!as.name(as.character(CURRENT_YEAR + 5)),
    pop_2035 = !!as.name(as.character(CURRENT_YEAR + 10))
  )

# =========================================================================== #
# DB-level output table used for CSV 1.
# Note: `urban_rural` is joined verbatim from the Excel Rural Initiatives matrix
# (sheet: "Census Subdivision Data", field: "Rural Category"); it is not computed
# in this self-contained script.
# =========================================================================== #
db_with_assignment <- crosswalk |>
  distinct(dbid, csdid, csd_name) |>
  left_join(db_population_estimates_three_year, by = "dbid") |>
  left_join(
    db_assignments |>
      select(dbid, assigned_facility, centroid_distance_m),
    by = "dbid"
  ) |>
  left_join(
    csd_rural_category |>
      select(csdid, urban_rural), # here urban_rural = "Urban 1", "Rural 2", etc.
    by = "csdid"
  )


# =========================================================================== #
# Population Estimates for each DB, used for CSV 2
# =========================================================================== #
# Build one long table at the DB × (projection year) × age level, then reuse it
# to compute facility totals (3 projection years), age-band counts (CURRENT_YEAR),
# and weighted median/mean age.
facility_population_estimates_three_year_all <- db_assignments |>
  left_join(crosswalk |> distinct(dbid, csdid, csd_name), by = "dbid") |>
  distinct(assigned_facility, assignment_method, dbid, csdid, csd_name) |>
  # expand data to include all years of interest for each row
  expand_grid(tibble(year = PROJECTION_YEARS)) |>
  left_join(
    db_projections_transformed |>
      filter(gender == "T") |>
      select(dbid, year, age, population, total, area_sq_km),
    by = c("dbid", "year")
  ) |>
  filter(!is.na(csdid))

facility_population_estimates_three_year <- facility_population_estimates_three_year_all |>
  summarize(
    pop = sum(population, na.rm = TRUE),
    .by = c(assigned_facility, year)
  ) |>
  pivot_wider(
    names_from = year,
    values_from = pop,
    values_fill = 0
  ) |>
  rename(
    pop_2025 = !!as.name(as.character(CURRENT_YEAR)),
    pop_2030 = !!as.name(as.character(CURRENT_YEAR + 5)),
    pop_2035 = !!as.name(as.character(CURRENT_YEAR + 10))
  )

# count of age groups for CURRENT_YEAR using the rolled-up DB age distribution
facility_age_estimates_current_year <- facility_population_estimates_three_year_all |>
  filter(year == CURRENT_YEAR) |>
  summarize(
    est_population_0_to_14_yrs = sum(
      population[age >= 0 & age < 15],
      na.rm = TRUE
    ),
    est_population_15_to_24_yrs = sum(
      population[age >= 15 & age < 25],
      na.rm = TRUE
    ),
    est_population_25_to_64_yrs = sum(
      population[age >= 25 & age < 65],
      na.rm = TRUE
    ),
    est_population_over_64_yrs = sum(population[age >= 65], na.rm = TRUE),
    .by = assigned_facility
  )


# mean/median age (CURRENT_YEAR) using the weighted age distribution
facility_median_population <- facility_population_estimates_three_year_all |>
  filter(year == CURRENT_YEAR) |>
  summarize(
    population = sum(population, na.rm = TRUE),
    .by = c(assigned_facility, age)
  ) |>
  summarize(
    median_age = weighted.median(age, population, na.rm = TRUE),
    mean_age = weighted.mean(age, population, na.rm = TRUE),
    .by = c(assigned_facility)
  )

# final facility-level demographics table for CSV 2 output
facility_demographics <- facility_population_estimates_three_year |>
  left_join(facility_age_estimates_current_year, by = "assigned_facility") |>
  left_join(facility_median_population, by = "assigned_facility") |>
  rename(facility = assigned_facility)


# =========================================================================== #
# Write outputs
# =========================================================================== #
message("Writing outputs to ", output_directory, " ...")
csv1_path <- file.path(output_directory, "db_centroid_assignments.csv")
csv2_path <- file.path(output_directory, "facility_population_projections.csv")
shp_dir <- file.path(output_directory, "facility_catchments")
dir.create(shp_dir, showWarnings = FALSE, recursive = TRUE)

# create CSV 1 DB assignments list
# each DB is assigned to a facility using the centroid only method
# dbid | csdid | csd_name | pop_2025 | pop_2030 | pop_2035 | assigned_facility | centroid_distance | urban_rural
db_with_assignment |>
  write_csv(csv1_path)

# create CSV 2 SBC facility population projections
# facility | pop_2025 | pop_2030 | pop_2035 | est_population_0_to_14_yrs | est_population_15_to_24_yrs | est_population_25_to_64_yrs | est_population_over_64_yrs | median_age | mean_age
facility_demographics |>
  write_csv(csv2_path)

# create shapefiles for each SBC facility location catchment
db_shapefiles |>
  left_join(db_assignments, by = "dbid") |>
  filter(!is.na(assigned_facility)) |>
  summarize(geometry = st_union(geometry), .by = "assigned_facility") |>
  ms_simplify(keep = 0.01) |>
  st_write(file.path(shp_dir, "sbc-catchments.shp"), append = FALSE)

message("\nOutputs written to: ")
message("  - ", csv1_path)
message("  - ", csv2_path)
message("  - ", shp_dir)


# =========================================================================== #
# Optional: Quick visualization of a single facility catchment
# =========================================================================== #

# list facility names available in assignments
facility_name_options <- db_assignments |>
  distinct(assigned_facility) |>
  arrange(assigned_facility)

# choose a facility name from facility_name_options$assigned_facility
selected_facility_name <- "Service BC - Example Office White Rock"

# DB polygons assigned to the selected facility
selected_facility_catchment <- db_shapefiles |>
  left_join(db_assignments, by = "dbid") |>
  filter(assigned_facility == selected_facility_name)

# point location for the selected facility
selected_facility_point <- facilities |>
  select(nearest_facility, geometry) |>
  filter(nearest_facility == selected_facility_name)

# fail fast if the name doesn't exist / doesn't match
if (nrow(selected_facility_catchment) == 0) {
  stop(
    "No DBs found for selected_facility_name. Pick an exact value from facility_name_options."
  )
}

# map extent (xmin/xmax/ymin/ymax) for zooming
catchment_bbox <- st_bbox(selected_facility_catchment)

ggplot() +
  geom_sf(
    data = selected_facility_catchment,
    fill = "grey80",
    color = "grey30",
    linewidth = 0.15
  ) +
  geom_sf(
    data = selected_facility_point,
    color = "#cb181d",
    size = 3
  ) +
  coord_sf(
    xlim = c(catchment_bbox["xmin"], catchment_bbox["xmax"]),
    ylim = c(catchment_bbox["ymin"], catchment_bbox["ymax"]),
    expand = FALSE
  ) +
  labs(title = selected_facility_name) +
  theme_void()
