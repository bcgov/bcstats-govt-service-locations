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

# This script loads aggregated csv data files containing spatial data for 
# municipality of interest in BC (loc). It produces maps at the dissemination block level 
# displaying quantitative information on basic descriptive statisics

library(tidyverse)
library(glue)
library(janitor)
library(sf)
library(tigris)
library(spatstat)
library(stars)
library(bcmaps)
library(terra)

source("R/settings.R")

# -----------------------------------------------------------------------------------------------------
# Read data points into one data frame
# -----------------------------------------------------------------------------------------------------
crosswalk <-
  read_csv(glue("{SRC_DATA_FOLDER}/csd-da-db-loc-crosswalk.csv"), col_types = cols(.default = "c")) %>%
  clean_names()

drivetime_data <-
  read_csv(glue("{SRC_DATA_FOLDER}/reduced-drivetime-data.csv"), col_types = cols(.default = "c")) %>%
  clean_names() %>%
  mutate(across(c(drv_time_sec, drv_dist), as.numeric))

drivetime_data <- drivetime_data %>% 
  inner_join(crosswalk, by = c("dbid", "daid", "csdid", "csd_name", "csd_desc"))

pop_db <- read_csv(glue("{SRC_DATA_FOLDER}/population-db.csv"), col_types = cols(.default = "c")) %>%
  clean_names() %>%
  mutate(across(c(area_sq_km, population, dwellings, households), as.numeric))

#------------------------------------------------------------------------------
# Read service bc location data from source folder
#------------------------------------------------------------------------------
servicebc <- read_csv(glue("{SRC_DATA_FOLDER}/reduced-service_bc_locs.csv")
           , col_types = cols(.default = "c")) %>%
  clean_names() %>%
  st_as_sf(coords = c("coord_x", "coord_y"), crs = 3005)

# -----------------------------------------------------------------------------------------------------
# read csd shapefiles
# -----------------------------------------------------------------------------------------------------

shp_csd_all <- census_subdivision() %>%
  select(4) %>%
  filter(CENSUS_SUBDIVISION_NAME %in% (servicebc %>% pull(csd_name))) %>%
  clean_names()

# -----------------------------------------------------------------------------------------------------
# make map data
# -----------------------------------------------------------------------------------------------------

for (csd in shp_csd_all %>% pull(census_subdivision_name)){

message(glue("Generating map for {csd} ..."))

points <- drivetime_data %>% 
  st_as_sf(coords = c("address_albers_x", "address_albers_y"), crs = 3005) %>%
  st_intersection(shp_csd)

# lets get the density of this thing
# Assign weights to each spatial point based on driving time
points$weights <- points$drv_time_sec

# Convert to ppp object with weights
stats_ppp <- as.ppp(points$geometry, W = as.owin(shp_csd))
marks(stats_ppp) <- points$drv_dist

density_stats_stars <- stars::st_as_stars(Smooth(stats_ppp, sigma = 1000))

#lets convert back to sf so it's compatible with ggplot2::geom_sf()
density_stats_sf <- st_as_sf(density_stats_stars) %>%
  st_set_crs(3005)

map_plot <- ggplot() +
  geom_sf(data = density_stats_sf, aes(fill = v), color = NA) +
  geom_sf(data = shp_csd, fill = NA, color = "grey70", linewidth = 1) +
  geom_sf(data = points, size = 0.25, color = "grey25", alpha = 0.1) +
  coord_sf(crs = 3005) +
  FILL_THEME +
  MAP_THEME +
  labs(
    #title = "Weighted Service BC Locations in Kamloops",
    #subtitle = "Density map showing population distribution and Service BC locations",
    #fill = "Density (people per unit area)",
    x = "\nLongitude",
    y = "Latitude\n"
  )
 print(map_plot)

   # Save the plot
  fn <- to_snake_case(glue("stars-{csd}"))

  ggsave(
    filename = glue("csd-drive-distance-maps/temp/{fn}.svg"),
    path = MAP_OUT,
    plot = map_plot,
    width = 8,
    height = 7,
    device = "svg"
  )

}
