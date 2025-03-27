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

source("configuration.R") # load libraries and other settings
source("fxns/maps.R") # functions for plotting maps

#------------------------------------------------------------------------------
# load aggregated data average drive time and distance by DA and DB 
# Langford: locality 909
# Smithers: locality 227
# Dawson Creek: locality 213
# Kamploops: locality 420
#------------------------------------------------------------------------------

data_folder <- safepaths::use_network_path()
loc <- "213" # Dawson Creek: locality 213
outfolder_da <- glue::glue("{data_folder}/data/da_average_times_dist_loc_{loc}")
outfolder_db <- glue::glue("{data_folder}/data/db_average_times_dist_loc_{loc}")

#------------------------------------------------------------------------------
# DA shp file
#------------------------------------------------------------------------------
download.file("https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lda_000b21a_e.zip", # nolint: line_length_linter.
              destfile = glue::glue("{data_folder}/data/raw/boundaries/lda_000a21a_e.zip")) # nolint: line_length_linter.

# file_path <- glue::glue("{data_folder}/data/raw/lda_000a21a_e/lda_000b21a_e.shp") # nolint
file_path <- "data/lda_000a21a_e/lda_000b21a_e.shp"

# Load the dissemination area shapefile
da_shapefile <- st_read(file_path)
da_shapefile <- da_shapefile %>%
  filter(PRUID == "59") %>%
  st_transform(crs = 3005)

#------------------------------------------------------------------------------
# DB shp file
#------------------------------------------------------------------------------

plot.dat <- da_shapefile %>% 
  inner_join(avg_dist_drvtime_by_db_service %>%
               distinct(daid), by = c("DAUID" = "daid")) # nolint: line_length_linter.

ggplot(plot.dat) +
  geom_sf() +
  geom_sf(aes(address_sf_with_da))

ggplot() +
  geom_sf(data = plot.dat, fill = "white") +
  geom_sf(data = address_sf_with_da,
          aes(color = drv_dist, alpha = 0.1)) +
  theme_minimal()