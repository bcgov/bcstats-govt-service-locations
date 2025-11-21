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

get_fsa_statscan_shp <- function() {

  # check if file already exists in SRC_DATA_FOLDER
  # if not, then we need to get it from statscan and unzip to the SRC_DATA_FOLDER
  dir <- glue::glue("{SRC_DATA_FOLDER}/shapefiles/")
  fil <- "lfsa000b21a_e.shp"

  if(file.exists(file.path(dir, tools::file_path_sans_ext(fil), fil))) {
    print("overwriting existing file")
  }

  link <- "https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lfsa000b21a_e.zip"
  temp_zipped <- file.path(tempdir(), basename(link))
  httr::GET(link, httr::write_disk(temp_zipped, overwrite = TRUE))

  temp_unzipped <- utils::unzip(temp_zipped, exdir = dir)

  # return status
  return(file.exists(temp_unzipped[grepl("\\.shp$", temp_unzipped)]))
}

get_popcenter_statscan_shp <- function() {

  # check if file already exists in SRC_DATA_FOLDER
  # if not, then we need to get it from statscan and unzip to the SRC_DATA_FOLDER
  dir <- glue::glue("{SRC_DATA_FOLDER}/shapefiles/")
  fil <- "lpc_000b21a_e.shp"
  sub <- tools::file_path_sans_ext(fil)

  if(file.exists(file.path(dir, sub, fil))) {
    print("overwriting existing file")
  }

  link <- "https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lpc_000b21a_e.zip"
  temp_zipped <- file.path(tempdir(), basename(link))
  httr::GET(link, httr::write_disk(temp_zipped, overwrite = TRUE))

  temp_unzipped <- utils::unzip(temp_zipped, exdir = file.path(dir, sub))

  # return status
  return(file.exists(temp_unzipped[grepl("\\.shp$", temp_unzipped)]))
}