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

#------------------------------------------------------------------------------
# Load Libraries and Settings
#------------------------------------------------------------------------------

source("R/settings.R")
source("R/fxns/csd-plots.R")

#------------------------------------------------------------------------------
# Read required data files from source folder
#------------------------------------------------------------------------------

# db drivetime and shapefile data
db_shapefile <-
  st_read(glue("{SHAPEFILE_OUT}/reduced-db-with-location.gpkg")) %>%
  mutate(across(c(landarea), as.numeric))

db_drivetime_data <-
  read_csv(
    glue("{SRC_DATA_FOLDER}/reduced_db_average_times_dist_all_locs.csv"),
    col_types = cols(.default = "c")
  ) %>%
  clean_names() %>%
  mutate(across(
    c(
      starts_with("drv_"),
      n_address,
      area_sq_km,
      population,
      dwellings,
      households
    ),
    as.numeric
  ))

db_drivetime_map_data <- db_shapefile %>%
  inner_join(db_drivetime_data, by = join_by(dbid, csdid, csd_name))

# service bc location data
servicebc <-
  read_csv(
    glue("{SRC_DATA_FOLDER}/reduced-service_bc_locs.csv"),
    col_types = cols(.default = "c")
  ) %>%
  clean_names() %>%
  st_as_sf(coords = c("coord_x", "coord_y"), crs = 3005)


#------------------------------------------------------------------------------
# build map - this is where we provide options for build map function
#------------------------------------------------------------------------------

region <- "Dissemination Block"
map_data <- db_drivetime_map_data

# user-defined map parameters
var <- "drv_dist_mean" # colnames(map_data) for other options
var_title <- "Mean Driving Distance"
unit <- "km"

# filter on desired csd here
for (csd in CSD_NAMES) {
  plot_title <- glue("{var_title} to Nearest Service BC Office - {csd}")

  message(glue("Generating map for {csd} ..."))

  map_plot <- build_map(
    data = map_data,
    servicebc_data = servicebc,
    varname = var,
    csd_col = "csd_name",
    csd_name = csd,
    sbc_col = "nearest_facility",
    map_theme = MAP_THEME,
    fill_scale = FILL_THEME,
    plot_title = plot_title,
    plot_subtitle = "",
    legend_title = glue("{var_title} ({unit})")
  )

  # show the plot
  show(map_plot)

  # Save the plot
  fn <- to_snake_case(glue("{var}-by-{region}-{csd}"))

  ggsave(
    filename = glue("csd-drive-distance-maps/{fn}.svg"),
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
