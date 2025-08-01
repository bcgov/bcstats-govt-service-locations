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

# Method 2. Create rural flag based on Statistics Canada's population centers.
# Statistics Canada defines rural areas as including all territory lying outside population centres.
# https://www12.statcan.gc.ca/census-recensement/2021/ref/dict/az/definition-eng.cfm?ID=geo049a

# https://www12.statcan.gc.ca/census-recensement/2021/as-sa/98-200-x/2021002/98-200-x2021002-eng.cfm
# https://www150.statcan.gc.ca/n1/pub/71-607-x/71-607-x2024012-eng.htm
# =========================================================================== #


# =========================================================================== #
# Function to determine which region each residence belongs to, 
# abstracted and works for each method (FSA or population center) and 
# data source (bcmaps, statscan, or pop centers).
# =========================================================================== #

resides_in_region <- function(residences, regions, region_name_col) {

  results <- st_within(residences, regions, sparse = FALSE)

  colnames(results) <- regions[[region_name_col]]
  rownames(results) <- residences$fid
  
  # print a message saying how many residences were matched
  message(glue("{nrow(residences)} residences processed."))
  message(glue("{sum(rowSums(results) == 0)} were unmatched to a region."))
  message(glue("{sum(rowSums(results) > 1)} were matched to more than one region."))

  # collapse the results matrix and join to drive data
  results |>
    as.data.frame() |>
    rownames_to_column("fid") |>
    pivot_longer(-fid, names_to = region_name_col, values_to = "in_region") |>
    filter(in_region) |>
    select(-in_region)

}

# =========================================================================== #
# Load libraries and settings ----
# =========================================================================== #

source("R/settings.R")

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
fsa_bcmaps <- bcmaps::fsa() |>
  clean_names() |>
  st_transform(crs = 3005)

# --- FSA boundary shapefiles - Method 1b
fsa_statscan <- st_read(
    glue::glue("{data_dir}lfsa000b21a_e/lfsa000b21a_e.shp")
  ) |>
  clean_names() |>
  select(-c(dguid, landarea)) |>
  filter(pruid == "59") |>
  st_transform(crs = 3005)

# --- Population center shapefiles - Method 2
pop_centers <- st_read(
    glue::glue("{data_dir}lpc_000b21a_e/lpc_000b21a_e.shp")
  ) |>
  clean_names() |>
  filter(pruid == "59") |>
  select(popid = dguid, pcname, pcclass, pctype) |>
  st_transform(crs = 3005)


# =========================================================================== #
# Compare the different methods and data sources ----
# =========================================================================== #

# --- Combine results into a single data frame
residences <- drivetime_data |> select(fid, geometry, nearest_facility)

fsa_bcmaps_results <- resides_in_region(residences, fsa_bcmaps, "cfsauid")
fsa_statscan_results  <- resides_in_region(residences, fsa_statscan, "cfsauid")
popcenter_results <- resides_in_region(residences, pop_centers, "pcname")

combined_results <- residences |>
  left_join(fsa_bcmaps_results, by = "fid") |>
  left_join(fsa_statscan_results, by = "fid", suffix = c("_bcmaps", "_statscan")) |>
  left_join(popcenter_results, by = "fid") |>
  mutate(
    urban_rural_bcmaps = case_when(
      is.na(cfsauid_bcmaps) ~ NA,                # missing FSA
      grepl("^V0", cfsauid_bcmaps) ~ "RURAL",       # rural
      TRUE ~ "URBAN"                              # urban
    ),
    urban_rural_statscan = case_when(
      is.na(cfsauid_statscan) ~ NA,
      grepl("^V0", cfsauid_statscan) ~ "RURAL",
      TRUE ~ "URBAN"
    ),
    urban_rural_popcenter = case_when(
      is.na(pcname) ~ "RURAL",                     # not in a pop center = rural
      TRUE ~ "URBAN"
    )
  )

# --- Aggregate the results so we can look at them
summary_results <- combined_results |>
  st_drop_geometry() |>
  summarise(
    n_residences = n(),
    n_rural_bcmaps = sum(urban_rural_bcmaps == "RURAL", na.rm = TRUE),
    n_rural_statscan = sum(urban_rural_statscan == "RURAL", na.rm = TRUE),
    n_rural_popcenter = sum(urban_rural_popcenter == "RURAL", na.rm = TRUE),
    n_missing_bcmaps = sum(is.na(urban_rural_bcmaps), na.rm = TRUE),
    n_missing_statscan = sum(is.na(urban_rural_statscan), na.rm = TRUE),
    p_rural_bcmaps = 100*sum(urban_rural_bcmaps == "RURAL", na.rm = TRUE)/n(),
    p_rural_statscan = 100*sum(urban_rural_statscan == "RURAL", na.rm = TRUE)/n(),
    p_rural_popcenter = 100*sum(urban_rural_popcenter == "RURAL", na.rm = TRUE)/n()
  ) |>
  pivot_longer(
    cols = starts_with(c("n_", "p_")),
    names_to = "method",
    values_to = "count"
  )

summary_results

rural_discrepancies_summary <- combined_results |>
  count(urban_rural_bcmaps, urban_rural_statscan, urban_rural_popcenter) |>
  rowwise() |>
  filter(
    (urban_rural_bcmaps != urban_rural_statscan) |
    (urban_rural_bcmaps != urban_rural_popcenter) |
    (urban_rural_statscan != urban_rural_popcenter)
  )

rural_discrepancies_summary

# =========================================================================== #
# Plot urban vs rural for each method
# =========================================================================== #

# --- Map the data for all the regions
all_the_regions <- combined_results |>
    pivot_longer(
      cols = starts_with("urban_rural_"),
      names_to = "method",
      values_to = "rural"
    )

ggplot(data = all_the_regions |> filter(method =="urban_rural_popcenter")) +
  geom_sf(aes(color = rural), size = 0.5) +
  scale_color_manual(values = c("URBAN" = "#084d08", "RURAL" = "#6060e4"), name = "Rural") +
  labs(title = glue::glue("Rural and Urban Areas - BC"),
       subtitle = "Colored by Rural Flag and Method",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

# ---- Map the data for custom regions
regions_to_plot <- drivetime_data |> pull(nearest_facility) |> unique()
facility <- sample(regions_to_plot, 1)
one_region_data <- all_the_regions |> filter(nearest_facility %in% facility) 

ggplot(data = one_region_data) +
  geom_sf(aes(color = rural), size = 0.5) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "blue"), name = "Rural") +
  facet_wrap(~ method, nrow = 2) +
  labs(title = glue::glue("{facility}"),
       subtitle = "Colored by Rural Flag and Method",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

