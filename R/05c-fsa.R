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
data_dir <- "C:/Users/BASHCROF/work/bcstats-govt-service-locations/data/"

# =========================================================================== #
# Load data ----
# =========================================================================== #

# address data
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

# ---------------------------------------------------------------------------
# Method 1. Create rural flag based on Canada Post FSA
# https://www.canadapost-postescanada.ca/cpc/en/support/articles/addressing-guidelines/postal-codes.page
# Notes: fsa boundaries are a mix of polygon and multipolygon due to holes (likely waterbodies) and/or spaces
# where the fsa is actually two polygons (may or may not be adjacent).

# Method 2. Create rural flag based on Statistics Canada's population centers.
# Statistics Canada defines rural areas as including all territory lying outside population centres.
# https://www12.statcan.gc.ca/census-recensement/2021/ref/dict/az/definition-eng.cfm?ID=geo049a
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# Function to determine which region each residence belongs to, 
# abstracted and works for each method (FSA or population center) and 
# data source (bcmaps, statscan, or pop centers).
# ---------------------------------------------------------------------------

resides_in_region <- function(residences, regions, region_name_col) {

  results <- st_within(residences, regions, sparse = FALSE)

  colnames(results) <- regions[[region_name_col]]
  rownames(results) <- residences$fid
  
  # collapse the results matrix and join to drive data
  results |>
    as.data.frame() |>
    rownames_to_column("fid") |>
    pivot_longer(-fid, names_to = region_name_col, values_to = "in_region") |>
    filter(in_region) |>
    select(-in_region)

}

# =========================================================================== #
# Compare the different methods and data sources ----
# =========================================================================== #

# Combine results into a single data frame
residences <- drivetime_data |> select(fid, geometry)

fsa_bcmaps_results <- resides_in_region(residences, fsa_bcmaps, "cfsauid")
fsa_statscan_results  <- resides_in_region(residences, fsa_statscan, "cfsauid")
popcenter_results <- resides_in_region(residences, pop_centers, "pcname")

combined_results <- fsa_bcmaps_results |>
  left_join(fsa_statscan_results, by = "fid", suffix = c("_bcmaps", "_statscan")) |>
  left_join(popcenter_results, by = "fid") |>
  mutate(
    rural_bcmaps = grepl("^V0", cfsauid_bcmaps),
    rural_statscan = grepl("^V0", cfsauid_statscan),
    rural_popcenter = is.na(pcname)
  )

# Filter for discrepancies between methods and summarize
fsa_discrepancies_summary <- combined_results |> 
  filter(cfsauid_bcmaps != cfsauid_statscan) |>
  count(cfsauid_bcmaps, cfsauid_statscan) |> 
  arrange(desc(n))

rural_discrepancies_summary <- combined_results |>
  count(rural_bcmaps, rural_statscan, rural_popcenter) |>
  rowwise() |>
  filter(
    (rural_bcmaps != rural_statscan) |
    (rural_bcmaps != rural_popcenter) |
    (rural_statscan != rural_popcenter)
  ) 

fsa_discrepancies_summary
rural_discrepancies_summary


# Plotting the discrepancies for custom regions
all_the_regions <- drivetime_data |>
  left_join(combined_results, by = "fid") |>
   select(fid, geometry, starts_with("rural_")) |>
    pivot_longer(
      cols = starts_with("rural_"),
      names_to = "method",
      values_to = "rural"
    )


microbenchmark(
      method1 = {drivetime_data |> distinct(nearest_facility) |> pull(nearest_facility)},
      method2 = {drivetime_data |> pull(nearest_facility) |> unique()},
      times = 10
    )

regions_to_plot1 <- drivetime_data |> distinct(nearest_facility) |> pull(nearest_facility)
regions_to_plot2 <- drivetime_data |> pull(nearest_facility) |> unique()


plot_data |> 
  filter(nearest_facility %in% regions_to_plot[1])

#plot the data using ggplot and geom_sf.  Facet on the method used
ggplot() +
  geom_sf(data = plot_data, aes(color = rural), size = 0.5) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "blue"), name = "Rural") +
  facet_wrap(~ method) +
  labs(title = "Population Centers in British Columbia",
       subtitle = "Colored by Rural Flag and Method",
       x = "Longitude", y = "Latitude") +
  theme_minimal()

