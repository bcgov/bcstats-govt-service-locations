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
  read_csv(glue("{SRC_DATA_FOLDER}/temp/csd-da-db-loc-crosswalk.csv"), col_types = cols(.default = "c")) %>%
  clean_names()

drivetime_data <-
  read_csv(glue("{SRC_DATA_FOLDER}/temp/processed-drivetime-data.csv"), col_types = cols(.default = "c")) %>%
  clean_names() %>%
  mutate(across(c(drv_time_sec, drv_dist), as.numeric))

drivetime_data <- drivetime_data %>% 
  inner_join(crosswalk, by = c("dbid", "daid"))

pop_db <- read_csv(glue("{SRC_DATA_FOLDER}/temp/population-db.csv"), col_types = cols(.default = "c")) %>%
  clean_names() %>%
  mutate(across(c(area_sq_km, population, dwellings, households), as.numeric))

#------------------------------------------------------------------------------
# Read service bc location data from source folder
#------------------------------------------------------------------------------
servicebc <- read_csv(glue("{SRC_DATA_FOLDER}/temp/service_bc_locs.csv")
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

shp_csd <- shp_csd_all %>% filter(census_subdivision_name == "Smithers")

points <- drivetime_data %>% 
  st_as_sf(coords = c("address_albers_x", "address_albers_y"), crs = 3005) %>%
  st_intersection(shp_csd)

# lets get the density of this thing
stats_ppp <- as.ppp(points$geometry, W = as.owin(shp_csd))
plot(stats_ppp)

pop_db

w0 <- 1
w1 <- if_else(points$dwellings == 0, 1, points$population/points$dwellings)
w2 <- if_else(is.na(points$drv_dist_mean), 0, points$drv_dist_mean)

density_stats_stars <- stars::st_as_stars(density(stats_ppp, weights = w0, dymx = 300))
plot(density_stats_stars)

#lets convert back to sf so it's compatible with ggplot2::geom_sf()
density_stats_sf <- st_as_sf(density_stats_stars) %>%
  st_set_crs(3005)

ggplot() +
  geom_sf(data = density_stats_sf, aes(fill = v), color = NA) +
  geom_sf(data = shp_csd, fill = NA, color = "black", linewidth = 0.25) +
  geom_sf(data = points, size = 0.1, color = "grey80", alpha = 0.5) +
  FILL_THEME +
  coord_sf(crs = 3005)
