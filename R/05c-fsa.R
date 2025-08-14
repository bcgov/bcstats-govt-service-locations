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

# Method 2. Create rural flag based on bcmaps FSA
# https://bcgov.github.io/bcmaps/
# Notes: fsa boundaries are a mix of polygon and multipolygon due to holes (likely waterbodies) and/or spaces
# where the fsa is actually two polygons (may or may not be adjacent).

# Method 3. Create rural flag based on Statistics Canada's population centers.
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
  n_unmatched <- sum(rowSums(results) == 0)
  n_multi_matched <- sum(rowSums(results) > 1)
  
  message(glue("{nrow(residences)} residences processed."))
  message(glue("{n_unmatched} ({round(100*n_unmatched/nrow(residences), 1)}%) were unmatched to a region."))
  message(glue("{n_multi_matched} ({round(100*n_multi_matched/nrow(residences), 1)}%) were matched to more than one region."))

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
fsa_statscan <- st_read(glue::glue("{SRC_DATA_FOLDER}/shapefiles/fsa-statscan.gpkg")) |>
  select(-c(dguid, landarea)) |>
  st_transform(crs = 3005)

# --- Population center shapefiles - Method 2
pop_centers <- st_read(glue("{SRC_DATA_FOLDER}/shapefiles/pop-centers-statscan.gpkg")) |>
  select(popid = dguid, pcname, pcclass, pctype) |>
  st_transform(crs = 3005)

# =========================================================================== #
# Create urban/rural flag for different methods and data sources ----
# =========================================================================== #

residences <- drivetime_data |> select(fid, geometry, nearest_facility)

# generate a crosswalk that maps each residence to a region, for each boundary (method).
fsa_residence_crosswalk_bcmaps <- resides_in_region(residences, fsa_bcmaps, "cfsauid")
fsa_residence_crosswalk_statscan <- resides_in_region(residences, fsa_statscan, "cfsauid")
popcenter_residence_crosswalk_statscan <- resides_in_region(residences, pop_centers, "pcname")

# combine and add an urban/rural flag for each method
residence_region_crosswalk <- residences |>
  left_join(fsa_residence_crosswalk_bcmaps, by = "fid") |>
  left_join(fsa_residence_crosswalk_statscan, by = "fid", suffix = c("_bcmaps", "_statscan")) |>
  left_join(popcenter_residence_crosswalk_statscan, by = "fid") |>
  mutate(
    urban_rural_bcmaps_fsa = case_when(
      is.na(cfsauid_bcmaps) ~ NA,                
      grepl("^V0", cfsauid_bcmaps) ~ "RURAL",    # an area is rural if the second character is a 0
      TRUE ~ "URBAN"                              
    ),
    urban_rural_statscan_fsa = case_when(
      is.na(cfsauid_statscan) ~ NA,
      grepl("^V0", cfsauid_statscan) ~ "RURAL",
      TRUE ~ "URBAN"
    ),
    urban_rural_popcenter = case_when(
      is.na(pcname) ~ "RURAL",  # an area is rural if outside a population center
      TRUE ~ "URBAN"
    )
  )

# =========================================================================== #
# Analyze results for different methods and data sources ----
# =========================================================================== #

# --- generate a summary table so we can compares how different classification methods label residences as "rural" or "urban." 
rural_summary_by_method <- residence_region_crosswalk |>
  st_drop_geometry() |>
  summarise(
    n_residences = n(),
    n_rural_bcmaps_fsa = sum(urban_rural_bcmaps_fsa == "RURAL", na.rm = TRUE),
    n_rural_statscan_fsa = sum(urban_rural_statscan_fsa == "RURAL", na.rm = TRUE),
    n_rural_popcenter = sum(urban_rural_popcenter == "RURAL", na.rm = TRUE),
    n_missing_bcmaps_fsa = sum(is.na(urban_rural_bcmaps_fsa), na.rm = TRUE),
    n_missing_statscan_fsa = sum(is.na(urban_rural_statscan_fsa), na.rm = TRUE),
    p_rural_bcmaps_fsa = 100*sum(urban_rural_bcmaps_fsa == "RURAL", na.rm = TRUE) / n(),
    p_rural_statscan_fsa = 100*sum(urban_rural_statscan_fsa == "RURAL", na.rm = TRUE) / n(),
    p_rural_popcenter = 100*sum(urban_rural_popcenter == "RURAL", na.rm = TRUE) / n()
  ) |>
  pivot_longer(
    cols = starts_with(c("n_", "p_")),
    names_to = "method",
    values_to = "count"
  )

rural_summary_by_method |> write_csv(
  glue("{TABLES_OUT}/rural-summary-by-method.csv")
)


# --- Create a table of classification patterns

method_classification_patterns <- residence_region_crosswalk |>
  st_drop_geometry() |>
  count(urban_rural_bcmaps_fsa, urban_rural_statscan_fsa, urban_rural_popcenter, name = "n_residences") |>
  mutate(
    agreement_type = case_when(
      (urban_rural_bcmaps_fsa == urban_rural_statscan_fsa) & (urban_rural_bcmaps_fsa == urban_rural_popcenter) ~ "All methods agree",
      (urban_rural_bcmaps_fsa == urban_rural_statscan_fsa) & (urban_rural_bcmaps_fsa != urban_rural_popcenter) ~ "FSA methods agree",
      (urban_rural_bcmaps_fsa != urban_rural_statscan_fsa) & (urban_rural_bcmaps_fsa == urban_rural_popcenter) ~ "BCMaps & PopCenter agree",
      (urban_rural_statscan_fsa != urban_rural_bcmaps_fsa) & (urban_rural_statscan_fsa == urban_rural_popcenter) ~ "StatsCan & PopCenter agree",
      TRUE ~ "All methods disagree"
    )
  ) |>
  arrange(desc(n_residences))

method_classification_patterns |> write_csv(
  glue("{TABLES_OUT}/method-classification-patterns.csv")
)


# --- Calculate a divergence score for each office, so we can compare how each office is affected by the different methods
# --- Not super meaningful, but useful for plotting areas with  high disagreement between offices
divergence_by_office <- residence_region_crosswalk |>
  st_drop_geometry() |>
  mutate(
    divergence_score = case_when(
      (urban_rural_bcmaps_fsa == urban_rural_statscan_fsa) & (urban_rural_bcmaps_fsa == urban_rural_popcenter) ~ 0, # all methods agree - no divergence
      (urban_rural_bcmaps_fsa == urban_rural_statscan_fsa) & (urban_rural_bcmaps_fsa != urban_rural_popcenter) ~ 1, # only the FSA methods agree
      (urban_rural_bcmaps_fsa != urban_rural_statscan_fsa) & (urban_rural_bcmaps_fsa == urban_rural_popcenter) ~ 1, # bcmaps_fsa agrees with popcenter, not statscan_fsa
      (urban_rural_bcmaps_fsa != urban_rural_statscan_fsa) & (urban_rural_statscan_fsa == urban_rural_popcenter) ~ 1, # statscan_fsa agrees with popcenter, not bcmaps_fsa
      TRUE ~ 2 # all methods disagree - this should only be possible when NA values
    )
  ) |>
  group_by(nearest_facility) |>
  summarise(
    total_divergence = sum(divergence_score, na.rm = TRUE),
    n_addresses = n(),
    n_no_divergence = sum(divergence_score == 0, na.rm = TRUE),
    n_partial_divergence = sum(divergence_score == 1, na.rm = TRUE),
    pct_divergent = 100 * (n() - n_no_divergence) / n(),
    .groups = 'drop'
  ) |>
  arrange(desc(pct_divergent))

divergence_by_office

# =========================================================================== #
# Plot urban vs rural for each method
# =========================================================================== #

# --- Create a reusable plotting function for urban/rural maps
plot_urban_rural <- function(data, title = "Rural and Urban Areas - BC") {

  data |>
    ggplot() +
    geom_sf(aes(color = rural), size = 0.5, alpha = 0.25) +
    scale_color_manual(values = c("URBAN" = "#074607", "RURAL" = "#8888f5"), name = "Rural") +
    labs(title = title,
         subtitle = "Colored by Rural Flag and Method",
         x = "Longitude", y = "Latitude") + 
    guides(color = guide_legend(override.aes = list(shape = 15, size = 5, alpha = 1))) +
    theme_minimal()
}

# --- Create data for mapping all the regions
residence_region_long <- residence_region_crosswalk |>
  pivot_longer(
    cols = starts_with("urban_rural_"),
    names_to = "method",
    values_to = "rural"
  )

# --- Plot urban/rural for each method
p <- residence_region_long |>
  plot_urban_rural(title = "Urban and Rural Areas - Population Centers") + facet_wrap(~ method, nrow = 2)

ggsave(
  glue("{MAP_OUT}/fsa-methods/urban-rural-population-centers.png"),
  plot = p,
  width = 10, height = 8
)

# --- Plot urban/rural for select offices
facility <- divergence_by_office |> slice(2) |> pull(nearest_facility)

p <- residence_region_long |>
  filter(nearest_facility %in% facility) |>
  plot_urban_rural(title = glue::glue("{facility}")) + facet_wrap(~ method, nrow = 2)

p
