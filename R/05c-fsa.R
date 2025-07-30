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
# fsa is a mix of polygon and multipolygon due to holes (likely waterbodies) and spaces
# where the fsa is actually two polygons that may or may not be adjunct.
fsa <- bcmaps::fsa() |>
  st_transform(crs = 3005) |>
  clean_names()

drivetime_data <-
  read_csv(
    glue("{SRC_DATA_FOLDER}/reduced-drivetime-data.csv"),
    col_types = cols(.default = "c")
  ) |>
  clean_names() |>
  mutate(across(c(drv_time_sec, drv_dist), as.numeric))

# census populations
pop_db <- read_csv(
  glue("{SRC_DATA_FOLDER}/reduced-population-db.csv"),
  col_types = cols(.default = "c")
) %>%
  clean_names() %>%
  mutate(across(c(area_sq_km, population, dwellings, households), as.numeric))

# ---------------------------------------------------------------------------
# Method 1. Create rural flag based on fsa
# according to canada post, an fsa is rural if the second character is a 0
# https://www.canadapost-postescanada.ca/cpc/en/support/articles/addressing-guidelines/postal-codes.page
# ---------------------------------------------------------------------------

residents <- drivetime_data %>% 
    select(fid, nearest_facility, address_albers_x, address_albers_y) %>%
    st_as_sf(coords = c("address_albers_x", "address_albers_y"), crs = 3005)

results <- st_within(residents, fsa, sparse = FALSE) # BA: check the other predicates like st_contains
colnames(results) <- fsa$cfsauid
rownames(results) <- residents$fid

# If the residence is not located in any fsa, NA will naturally be added to residents df later.
no_fsa <- which(rowSums(results) == 0)
if (length(no_fsa) > 0) {
    message(glue("Found {length(no_fsa)} residences that are not located in any fsa, check data. 
            fid: {paste0(rownames(results)[no_fsa], collapse = ', ')}"))
    results <- results[-no_fsa, ]
}

# If the residence is located on a boundary between 2 fsa's, then we want to flag them and deal with that.
multis <- which(rowSums(results) > 1)
if (length(multis) > 0) {
    message(glue("Found {length(multis)} residences that are located in multiple fsa's, check fsa boundaries for these residences.
            fid: {paste0(rownames(results)[multis], collapse = ', ')}"))
    results <- results[-multis, ]
}

# collapse the results matrix so each address is associated with it's fsa
fsa_residents <- results |>
    as.data.frame() |>
    rownames_to_column("fid") |>
    pivot_longer(-fid, names_to = "cfsauid", values_to = "in_fsa") |>
    left_join(residents, by = "fid") |>
    # add rural flag - if the second character of the fsa is 0, then it is rural
    mutate(rural = ifelse(substr(cfsauid, 2, 2) == "0", TRUE, FALSE)) |>
    select(-c(in_fsa, cfsauid)) 


# ----------------------------------------------------------------------------
# Create rural flag based on population and population density
# Statistics Canada defines rural areas as including all territory lying outside population centres
# The 2021 delineation rules for population centres (POPCTR) are ranked in order of priority:
# https://www12.statcan.gc.ca/census-recensement/2021/ref/dict/az/definition-eng.cfm?ID=geo049a
# ----------------------------------------------------------------------------


