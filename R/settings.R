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

# ------------------------------------------------------------------------
# Script: settings.R

# Description: This script centralizes project-wide configuration. When sourced,
#  it sets global R options (e.g., timeout) and defines numerous global constants
# used throughout the project for file paths, filenames, expected values,
# data cleaning patterns, required column names, and specific tags.

# Requirements:
#   - Implicit dependency on the `glue` package for constructing paths/patterns.
#   - Implicit dependency on the `safepaths` package .
#   - Relies on the network environment/path accessible via `use_network_path`.

# Side Effects/Outputs:
#   - Modifies the global R environment by setting the `timeout` option.
#   - Defines multiple global variables (constants) in the calling environment
# ------------------------------------------------------------------------

#------------------------------------------------------------------------------
# Set options
#------------------------------------------------------------------------------

# set timeout on file load process
getOption("timeout")
options(timeout = 600)

# common project libraries
library(safepaths)
library(glue)
library(tidyverse)
library(janitor)
library(snakecase)
library(ggplot2)
library(scales)
library(readxl)

# for working with spatial vector data
library(sf)
library(bcdata)
library(tigris)
library(spatstat)
library(stars)
library(terra)
library(fs)
library(ggnewscale)
library(bcmaps)
library(rmapshaper)    # simplify geometries

# for statistical calculations
library(e1071)

#------------------------------------------------------------------------------
# Constants declaration
#------------------------------------------------------------------------------
CSD_NAMES <- c("Langford", "Dawson Creek", "Smithers", "Kamloops")
CSDIDS <- c(5917044, 5955014, 5951043, 5933042)
CENSUS_BASIS <- 2021
CANCENSUS_YEAR <- paste0('CA', str_sub(as.character(CENSUS_BASIS), 3, 4))
CURRENT_YEAR <- 2025

# File paths
LAN_FOLDER <- use_network_path()
SRC_DATA_FOLDER <- glue("{LAN_FOLDER}/2025 Government Service Locations/data/source/")
RAW_DATA_FOLDER <- glue("{LAN_FOLDER}/2025 Government Service Locations/data/raw/")
DT_DATA_FOLDER <- glue("{LAN_FOLDER}/2025 Government Service Locations/data/raw/nearest_facility_BC")
SBCLOC_FILEPATH <- glue("{SRC_DATA_FOLDER}/full-service-bc-locs.csv")

# Output filenames
SHAPEFILE_OUT <- glue("{SRC_DATA_FOLDER}/shapefiles/")
MAP_OUT <- glue("{LAN_FOLDER}/2025 Government Service Locations/outputs/visuals")
VISUALS_OUT <- glue("{LAN_FOLDER}/2025 Government Service Locations/outputs/visuals")
TABLES_OUT <- glue("{LAN_FOLDER}/2025 Government Service Locations/outputs/tables")

# File patterns
INPUT_ADDR_DA_PATTERN <- "address_with_da.*"
NO_ERRS_FILE_PATTERN <- "no_errors.csv"

# Columns
REQUIRED_COLS <- c("site_albers_x", "site_albers_y", "dissemination_block_id", "drv_time_sec", "drv_dist", "tag")
POP_COLS <- c("region_name", "area_sq_km", "population", "dwellings", "households")
FACILITY_TAG <- "servicebc"

# Constants for visualizations
# noting for later: these are almost the same and could be collaped into 1
# only difference is axis.text.x
MAP_THEME <- theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    plot.title = element_text(size = 20, face = "bold", hjust = 0),
    plot.subtitle = element_text(size = 18),
    legend.title = element_text(size = 13, face = "bold"),
    legend.text = element_text(size = 12),
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 15),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title.position = "top")


BOX_PLOT_THEME <- theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 18),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 13),
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 18),
    legend.position = "none",
    legend.box = "horizontal",
    legend.title.position = "top",
    axis.text.x = element_text(hjust = 0.5)
  )

  VIOLIN_PLOT_THEME <- theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 12),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title.position = "top",
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

SCATTER_PLOT_THEME <- BOX_PLOT_THEME +
  theme(axis.text.x = element_text(angle = 0, hjust = 1))

FILL_THEME <- scale_fill_viridis_c(option = "mako", alpha = 0.6, na.value = "red") # add limits = c(0, NA) to start scaling at 0.
FILL_THEME_D <- scale_fill_viridis_d(option = "mako", alpha = 0.6)
COLOR_THEME_D <- scale_color_viridis_d(option = "mako", alpha = 0.6)