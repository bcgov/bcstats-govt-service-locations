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
#library(svglite)
#library(scales)
library(snakecase)

source("R/settings.R")
source("R/fxns/plots.R")

#------------------------------------------------------------------------------
# Read shape file data from source folder
#------------------------------------------------------------------------------

csd_shapefile <-
  st_read(glue("{SHAPEFILE_OUT}/reduced-csd-with-location.gpkg")) %>%
  mutate(across(c(landarea), as.numeric))

da_shapefile <-
  st_read(glue("{SHAPEFILE_OUT}/reduced-da-with-location.gpkg")) %>%
  mutate(across(c(landarea), as.numeric))

db_shapefile <-
  st_read(glue("{SHAPEFILE_OUT}/reduced-db-with-location.gpkg")) %>%
  mutate(across(c(landarea), as.numeric))

#------------------------------------------------------------------------------
# Read drive time data from source folder
#------------------------------------------------------------------------------

da_drivetime_data <-
  read_csv(glue("{SRC_DATA_FOLDER}/da_average_times_dist_all_locs.csv")
          , col_types = cols(.default = "c")) %>%
  clean_names()  %>%
  mutate(across(c(starts_with("drv_"), n_address, area_sq_km, population, dwellings, households), as.numeric))

db_drivetime_data <-
  read_csv(glue("{SRC_DATA_FOLDER}/db_average_times_dist_all_locs.csv")
          , col_types = cols(.default = "c"))  %>%
  clean_names() %>%
  mutate(across(c(starts_with("drv_"), n_address, area_sq_km, population, dwellings, households), as.numeric))

csd_drivetime_data <-
  read_csv(glue("{SRC_DATA_FOLDER}/csd_average_times_dist_all_locs.csv")
          , col_types = cols(.default = "c"))  %>%
  clean_names() %>%
  mutate(across(c(starts_with("drv_"), n_address, area_sq_km, population, dwellings, households), as.numeric))

#------------------------------------------------------------------------------
# Read service bc location data from source folder
#------------------------------------------------------------------------------

servicebc <-
  read_csv(glue("{SRC_DATA_FOLDER}/service_bc_locs.csv")
           , col_types = cols(.default = "c")) %>%
  clean_names() %>%
  st_as_sf(coords = c("coord_x", "coord_y"), crs = 3005)

#------------------------------------------------------------------------------
# Join shapefiles to data for mapping
# Use left_join to color differently those da/db's missing data
# remove DA's for for which we have only an extremely small number of addresses
#------------------------------------------------------------------------------

da_drivetime_map_data <- da_shapefile %>%
  inner_join(da_drivetime_data, by = join_by(daid, csd_name))

db_drivetime_map_data <- db_shapefile %>%
  inner_join(db_drivetime_data, by = join_by(dbid, csdid, csd_name))

csd_drivetime_map_data <- csd_shapefile %>%
  inner_join(csd_drivetime_data, by = join_by(csdid, csd_name))

#------------------------------------------------------------------------------
# build map - this is where we provide options for build map function
#------------------------------------------------------------------------------

region <- "Dissemination Block"
map_data  <- db_drivetime_map_data

if(region == "Dissemination Area"){
  map_data  <- da_drivetime_map_data
}

# user-defined map parameters
var <- "n_address"  # colnames(map_data) for other options
var_title <- "Count of Addresses"

plot_subtitle <- ""

# filter on desired csd here
for (csd in csd_drivetime_map_data %>% pull(csd_name)){

  message(glue("Generating map for {csd} ..."))

  plot_title <- glue("{var_title} by {region_title}, {csd}")

  map_plot <- build_map(
    data = map_data,
    servicebc_data = servicebc,
    varname = var,
    csd_col = "csd_name",
    csd_name = csd,
    map_theme = MAP_THEME,
    fill_scale = FILL_THEME,
    plot_title = plot_title,
    plot_subtitle = plot_subtitle,
    legend_title = var_title
  )

  # show the plot
  show(map_plot)
  
  # Save the plot
  fn <- to_snake_case(glue("{var}-by-{region_title}-{csd}"))

  ggsave(
    filename = glue("{fn}.svg"),
    path = MAP_OUT,
    plot = map_plot,
    width = 8,
    height = 7,
    device = "svg"
  )
}

# clean up the environment
rm(list = ls())
gc()