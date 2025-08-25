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

# Method 1. Create rural flag based on Canada Post FSA
# https://www.canadapost-postescanada.ca/cpc/en/support/articles/addressing-guidelines/postal-codes.page
# Notes: fsa boundaries are a mix of polygon and multipolygon due to holes (likely waterbodies) and/or spaces
# where the fsa is actually two polygons (may or may not be adjacent).
# boundary files downloaded from Statistics Canada and turned to .gpkg
# https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/index2021-eng.cfm?Year=21

# Method 2. Create rural flag based on Statistics Canada's population centers.
# Statistics Canada defines rural areas as including all territory lying outside population centres.
# https://www12.statcan.gc.ca/census-recensement/2021/ref/dict/az/definition-eng.cfm?ID=geo049a
# boundary files downloaded from Statistics Canada and turned to .gpkg
# https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/index2021-eng.cfm?Year=21

# More Rural references:
# https://www12.statcan.gc.ca/census-recensement/2021/as-sa/98-200-x/2021002/98-200-x2021002-eng.cfm
# https://www150.statcan.gc.ca/n1/pub/71-607-x/71-607-x2024012-eng.htm


# =========================================================================== #

# =========================================================================== #
# Function to determine which region each residence belongs to,
# abstracted and works for each method (FSA or population center) and
# data source (bcmaps, statscan, or pop centers).
# =========================================================================== #

# =========================================================================== #
# Load libraries and settings ----
# =========================================================================== #

source("R/settings.R")
source("R/fxns/rural-fxns.R")

# =========================================================================== #
# Load data ----
# =========================================================================== #

# --- Address data
drivetime_data <-
  read_csv(
    glue("{SRC_DATA_FOLDER}/full-processed-drivetime-data.csv"),
    col_types = cols(.default = "c")
  ) |>
  clean_names() |>
  mutate(across(c(drv_time_sec, drv_dist), as.numeric)) |>
  st_as_sf(coords = c("address_albers_x", "address_albers_y"), crs = 3005)

# --- FSA boundary shapefiles - Method 1
fsa_boundaries <-
  st_read(glue::glue("{SRC_DATA_FOLDER}/shapefiles/fsa-statscan.gpkg"),
          layer = "fsa_statscan") |>
  rename(geometry = geom) |>
  st_transform(crs = 3005)

# --- Population center shapefiles - Method 2
popcenter_boundaries <-
  st_read(glue("{SRC_DATA_FOLDER}/shapefiles/popcenter-statscan.gpkg"),
          layer = "popcenter_statscan") |>
  rename(geometry = geom) |>
  st_transform(crs = 3005)

catchments <- st_read(glue::glue("{FOR_SBC_OUT}/sbc-catchments/sbc-catchments.shp"))

complete_assignments <-
  read_csv(glue::glue("{FOR_SBC_OUT}/complete-db-assignments-for-SBC.csv")) |>
  clean_names() |>
  mutate(across(everything(), as.character))

csd_db_crosswalk <-
  read_csv(
    glue("{SRC_DATA_FOLDER}/csd-da-db-loc-correspondance.csv"),
    col_types = cols(.default = "c")
  ) |>
  clean_names()


db_shapefiles <- st_read(glue("{SHAPEFILE_OUT}/full-db-with-location.gpkg")) |>
  st_transform(crs = 3005)

db_projections_transformed_agg <- 
  readRDS(glue("{SRC_DATA_FOLDER}/full-db-projections-transformed.rds")) |>
  filter(gender == 'T', year == 2025) |>
  summarize(population = sum(population, na.rm = TRUE),
            .by = c("dbid"))

# =========================================================================== #
# Create (address-based) urban/rural flag using fsa and popcenter boundary files
# =========================================================================== #
addresses <- drivetime_data |> select(fid, geometry, nearest_facility, dbid)

# generate a crosswalk that maps each address to a region, for each boundary (method).
fsa_address <- resides_in_region(addresses, fsa_boundaries, "fid", "cfsauid")
popcenter_address <- resides_in_region(addresses, popcenter_boundaries, "fid", "pcname")

# combine and add an urban/rural flag for each method
addresses_region_crosswalk <- addresses |>
  left_join(fsa_address, by = "fid") |>
  left_join(popcenter_address, by = "fid") |>
  left_join(csd_db_crosswalk, by = "dbid") |>
  mutate(
    urban_rural_fsa = case_when(
      is.na(cfsauid) ~ NA,
      grepl("^V0", cfsauid) ~ "RURAL",
      TRUE ~ "URBAN"
    ),
    urban_rural_popcenter = case_when(
      is.na(pcname) ~ "RURAL",  # an area is rural if outside a population center
      TRUE ~ "URBAN"
    )
  )

# --- BC-level summary of classification methods (address-based)
addresses_region_crosswalk |>
  st_drop_geometry() |>
  summarise(
    n_addresses = n(),
    n_rural_fsa = sum(urban_rural_fsa == "RURAL", na.rm = TRUE),
    n_rural_popcenter = sum(urban_rural_popcenter == "RURAL", na.rm = TRUE),
    n_missing_fsa = sum(is.na(urban_rural_fsa), na.rm = TRUE),
    p_rural_fsa = 100*sum(urban_rural_fsa == "RURAL", na.rm = TRUE) / n(),
    p_rural_popcenter = 100*sum(urban_rural_popcenter == "RURAL", na.rm = TRUE) / n()
  ) |>
  pivot_longer(
    cols = starts_with(c("n_", "p_")),
    names_to = "method",
    values_to = "count"
)

# --- Catchment-level summaries of classification methods
# --- according to # of addresses assigned
catchment_rural_summary_addresses <- addresses_region_crosswalk |>
  st_drop_geometry() |>
  left_join(complete_assignments, by = "dbid") |>
  group_by(assigned) |>
  summarise(
    n_addresses = n(),
    n_rural_fsa = sum(urban_rural_fsa == "RURAL", na.rm = TRUE),
    n_rural_popcenter = sum(urban_rural_popcenter == "RURAL", na.rm = TRUE),
    p_rural_fsa = 100 * sum(urban_rural_fsa == "RURAL", na.rm = TRUE) / n(),
    p_rural_popcenter = 100 * sum(urban_rural_popcenter == "RURAL", na.rm = TRUE) / n(),
    is_rural_fsa = if_else(p_rural_fsa > 50, "RURAL", "URBAN"),
    is_rural_popcenter = if_else(p_rural_popcenter > 50, "RURAL", "URBAN"),
    .groups = 'drop'
  )

# note that due to an abundance of rural addresses, there are many more rural catchments than I might have expected
# does this mean that using address is misleading, and we should use population? what would that look like?
# or are they truly placed out in rural areas? 
catchment_rural_summary_addresses |> 
  summarize(
    mean_rural_fsa = mean(p_rural_address_fsa, na.rm = TRUE),
    mean_rural_popcenter = mean(p_rural_address_popcenter, na.rm = TRUE),
    median_rural_fsa = median(p_rural_address_fsa, na.rm = TRUE),
    median_rural_popcenter = median(p_rural_address_popcenter, na.rm = TRUE)
  ) |>
  pivot_longer(everything())

catchment_rural_summary_addresses |>
  count(is_rural_fsa, is_rural_popcenter) |>
  arrange(desc(n))

# =========================================================================== #
# Create (population-based) urban/rural flag using fsa and popcenter boundary files
# =========================================================================== #

# generate a crosswalk that maps each dbid to a population center, for each boundary (method).
popcenter_population <- resides_in_region(db_shapefiles, popcenter_boundaries, "dbid", "pcname")
fsa_population <- resides_in_region(db_shapefiles, fsa_boundaries, "dbid", "cfsauid")

# add flags for urban rural
population_region_crosswalk  <- db_projections_transformed_agg |> 
  st_drop_geometry() |>
  left_join(popcenter_population, by = "dbid") |>
  left_join(fsa_population, by = "dbid") |>
  left_join(complete_assignments, by = "dbid") |>
  mutate(
    urban_rural_fsa = case_when(
      is.na(cfsauid) ~ NA,
      grepl("^V0", cfsauid) ~ "RURAL",
      TRUE ~ "URBAN"
    ),
    urban_rural_popcenter = case_when(
      is.na(pcname) ~ "RURAL",  # an area is rural if outside a population center
      TRUE ~ "URBAN"
    )
  )

# --- BC-level summary of classification methods (population-based)
population_region_crosswalk |>
  st_drop_geometry() |>
  summarise(
    n_population = sum(population, na.rm = TRUE),
    n_rural_fsa = sum(population[urban_rural_fsa == "RURAL"], na.rm = TRUE),
    n_rural_popcenter = sum(population[urban_rural_popcenter == "RURAL"], na.rm = TRUE),
    n_missing_fsa = sum(population[is.na(urban_rural_fsa)], na.rm = TRUE),
    p_rural_fsa = 100 * n_rural_fsa / n_population,
    p_rural_popcenter = 100 * n_rural_popcenter / n_population
  ) |>
  pivot_longer(
    cols = starts_with(c("n_", "p_")),
    names_to = "method",
    values_to = "count"
)


# --- Catchment-level summaries of classification methods (population-based)
catchment_rural_summary_population <- population_region_crosswalk |>
  summarise(
    n_dbids = n(),
    n_rural_population_fsa = sum(population[urban_rural_fsa == "RURAL"], na.rm = TRUE),
    n_rural_population_popcenter = sum(population[urban_rural_popcenter == "RURAL"], na.rm = TRUE),
    ttl_population = sum(population, na.rm = TRUE),
    p_rural_population_fsa = 100 * n_rural_population_fsa / ttl_population,
    p_rural_population_popcenter = 100 * n_rural_population_popcenter / ttl_population,
    is_rural_population_fsa = if_else(p_rural_population_fsa > 50, "RURAL", "URBAN"),
    is_rural_population_popcenter = if_else(p_rural_population_popcenter > 50, "RURAL", "URBAN"),
    .by = c("assigned")
  )

catchment_rural_summary_population

catchment_rural_summary_population |> 
  summarize(
    mean_rural_population_fsa = mean(p_rural_population_fsa, na.rm = TRUE),
    median_rural_population_fsa = median(p_rural_population_fsa, na.rm = TRUE),
    mean_rural_population_popcenter = mean(p_rural_population_popcenter, na.rm = TRUE),
    median_rural_population_popcenter = median(p_rural_population_popcenter, na.rm = TRUE)
  ) |>
  pivot_longer(everything())

catchment_rural_summary_population |>
  count(is_rural_population_fsa, is_rural_population_popcenter) |>
  arrange(desc(n))

# =========================================================================== #
# compare the two methods (address and population, use popcenter method)
# =========================================================================== #

catchment_rural_summary_compare <- catchment_rural_summary_residence |>
  select(assigned, n_residences, p_rural_popcenter) |>
  inner_join(catchment_rural_summary_population, by = "assigned") |>
  mutate(ppl_per_address = ttl_population / n_residences)

catchment_rural_summary_compare

catchment_rural_summary_compare |>
  filter(assigned %in% c("Service BC - Atlin"))

catchment_rural_summary_compare |>
  filter(!assigned %in% c("Service BC - Atlin")) |> 
  ggplot(aes(x =  p_rural_population_popcenter, y = n_rural_population_popcenter/ttl_population)) +
  geom_point(aes(color = ppl_per_address), size = 5) +
  geom_smooth() +
  scale_color_viridis_c(trans = "reverse") +
  labs(
    title = "",
    x = "% Rural (Address-Based)",
    y = "% Rural (Population-Based)"
  )

# where are the offices themselves located, urban/rural?
facilities <- drivetime_data |> st_drop_geometry() |> distinct(nearest_facility, coord_x, coord_y) |>
  st_as_sf(coords = c("coord_x", "coord_y"), crs = 3005)

urban_facilities <- resides_in_region(facilities, pop_centers, "nearest_facility", "pcname") # OR
# urban_facilities <- resides_in_region(facilities, fsa, "nearest_facility", "cfsauid")

facilities <- facilities |> left_join(urban_facilities, by = "nearest_facility") |>
  mutate(urban_rural = if_else(is.na(pcname), "RURAL", "URBAN"))

facilities |>  
ggplot() +
  geom_sf(data = facilities, aes(color = urban_rural)) +
  scale_color_manual(values = c("RURAL" = "blue", "URBAN" = "red")) +
  labs(
    title = "Urban and Rural Facilities",
    fill = "Urban/Rural"
)


# =========================================================================== #
# Roll up of rural flag to CSD ----
# =========================================================================== #
# --- Create a summary table of rural/urban classification by CSD
# --- according to # of addresses assigned
csd_rural_summary <- residence_region_crosswalk |>
  st_drop_geometry() |>
  group_by(csdid, csd_name, csd_desc) |>
  summarise(
    n_residences = n(),
    n_rural_fsa = sum(urban_rural_fsa == "RURAL", na.rm = TRUE),
    n_rural_popcenter = sum(urban_rural_popcenter == "RURAL", na.rm = TRUE),
    p_rural_fsa = 100 * sum(urban_rural_fsa == "RURAL", na.rm = TRUE) / n(),
    p_rural_popcenter = 100 * sum(urban_rural_popcenter == "RURAL", na.rm = TRUE) / n(),
    is_rural_fsa = if_else(p_rural_fsa > 50, "RURAL", "URBAN"),
    is_rural_popcenter = if_else(p_rural_popcenter > 50, "RURAL", "URBAN"),
    .groups = 'drop'
  )

csd_rural_summary

csd_rural_summary |> 
  summarize(
    mean_rural_fsa = mean(p_rural_fsa, na.rm = TRUE),
    mean_rural_popcenter = mean(p_rural_popcenter, na.rm = TRUE),
    median_rural_fsa = median(p_rural_fsa, na.rm = TRUE),
    median_rural_popcenter = median(p_rural_popcenter, na.rm = TRUE)
  ) |>
  pivot_longer(everything())

csd_rural_summary |>
  count(is_rural_fsa, is_rural_popcenter) |>
  arrange(desc(n))

# =========================================================================== #
# Plot urban vs rural for each method, different catchments
# =========================================================================== #

# --- Create data for mapping all the regions
residence_region_long <- residence_region_crosswalk |>
  pivot_longer(
    cols = starts_with("urban_rural_"),
    names_to = "method",
    values_to = "rural"
  )

# --- Plot urban/rural for each method
p <- residence_region_long |>
  plot_urban_rural(title = "Urban and Rural Areas - Population Centers") + 
  facet_wrap(~ method, nrow = 2)

ggsave(
  glue("{MAP_OUT}/rural-analysis/all-data-urban-rural.png"),
  plot = p,
  width = 10, height = 8
)

# --- Plot urban/rural for select csd/office
facility <- catchment_rural_summary |> arrange(p_rural_popcenter) |> slice(1) |> pull(assigned)

p <- residence_region_long |>
  filter(nearest_facility %in% facility) |>
  plot_urban_rural(title = glue::glue("{facility}")) + 
  facet_wrap(~ method, nrow = 2)

ggsave(
  glue("{MAP_OUT}/rural-analysis/{make_clean_names(facility, sep_out = '-')}-urban-rural.png"),
  plot = p,
  width = 10, height = 8
)

# --- Plot urban/rural for each method
p <- residence_region_long |>
  plot_urban_rural(title = "Urban and Rural Areas - Population Centers") +
  geom_sf(data = catchments, color = "black", alpha = 0) +
  facet_wrap(~ method, nrow = 2)

ggsave(
  glue("{MAP_OUT}/rural-analysis/catchment-urban-rural.png"),
  plot = p,
  width = 10, height = 8
)
