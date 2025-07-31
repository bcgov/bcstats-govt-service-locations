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

# --- CSD shapefiles
shp_csd_all <-
  st_read(glue("{SHAPEFILE_OUT}/reduced-csd-with-location.gpkg")) %>%
  select(census_subdivision_name = csd_name, census_subdivision_id = csdid)

# ---------------------------------------------------------------------------
# Method 1. Create rural flag based on Canada Post FSA
# https://www.canadapost-postescanada.ca/cpc/en/support/articles/addressing-guidelines/postal-codes.page
# Notes: fsa boundaries are a mix of polygon and multipolygon due to holes (likely waterbodies) and/or spaces
# where the fsa is actually two polygons (may or may not be adjacent).
# ---------------------------------------------------------------------------

residents <- drivetime_data %>% 
    select(fid, nearest_facility, address_albers_x, address_albers_y, dguid = dbid) %>%
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

# If the residence is located on a boundary between 2 fsa's, then we want to deal with that.
multis <- which(rowSums(results) > 1)
if (length(multis) > 0) {
    message(glue("Found {length(multis)} residences that are located in multiple fsa's, check fsa boundaries for these residences.
            fid: {paste0(rownames(results)[multis], collapse = ', ')}"))
    results <- results[-multis, ]
}

# collapse the results matrix and join to drive data - each address will be associated with an fsa
fsa_residents <- results |>
    as.data.frame() |>
    rownames_to_column("fid") |>
    pivot_longer(-fid, names_to = "cfsauid", values_to = "in_fsa") |>
    left_join(residents, by = "fid") |>
    # add rural flag - if the second character of the fsa is 0, then it is rural
    mutate(rural = ifelse(substr(cfsauid, 2, 2) == "0", TRUE, FALSE)) |>
    select(-c(in_fsa, cfsauid)) 


# ----------------------------------------------------------------------------
# Method 2. Create rural flag based on Statistics Canada's population centers.
# Statistics Canada defines rural areas as including all territory lying outside population centres.
# https://www12.statcan.gc.ca/census-recensement/2021/ref/dict/az/definition-eng.cfm?ID=geo049a
# ----------------------------------------------------------------------------

# read population center boundary files
pop_centers <- st_read("C:/Users/BASHCROF/work/bcstats-govt-service-locations/lpc_000b21a_e/lpc_000b21a_e.shp") %>%
  filter(PRUID == "59") |>
  clean_names() |>
  st_drop_geometry() |>
  select(popid = dguid, pcname,pcclass, pctype)

# read relationship file to get mapping between population center, csd and dissmination block.  Keep rows
# corresponding to province British Columbia (PRDGUID_PRIDUGD == "2021A000259").  read in cols as a character
# to avoid issues with leading zeros in the GUIDs. 
concordance <- read_csv("C:/Users/BASHCROF/work/bcstats-govt-service-locations/2021_98260004/2021_98260004.csv",
                        col_types = cols(.default = "c")) |>
  filter(PRDGUID_PRIDUGD == "2021A000259") |>
  clean_names() |>
  select(csdid = csddguid_sdridugd, dbid = dbdguid_ididugd, popid = popctrdguid_ctrpopidugd) |>
  mutate(dbid = gsub("2021S[0-9][0-9][0-9][0-9]", "", dbid))

pop_residents <- residents |>
  left_join(concordance, by = c("dguid" = "dbid")) |>
  left_join(pop_centers, by = "popid") |>
  mutate(rural = ifelse(is.na(popid), TRUE, FALSE)) |>
  select(-popid)

# choose one nearest facility
# and map the pop_residents in a ggplot geom_sf.  Color the points by rural flag
pop_residents_plot <- pop_residents |>
   filter(nearest_facility == "Service BC - Smithers")

ggplot() +
  geom_sf(data = pop_residents_plot, aes(color = rural), size = 0.5) +
  scale_color_manual(values = c("TRUE" = "red", "FALSE" = "blue"), name = "Rural") +
  labs(title = "Population Centers in British Columbia",
       subtitle = "Colored by Rural Flag",
       x = "Longitude", y = "Latitude") +
  theme_minimal()
