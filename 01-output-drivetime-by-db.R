library(tidyverse)
library(safepaths)
library(glue)
library(janitor)

library(sf)

# library(cancensus) # nolint
# library(cansim) # nolint

# library(bcmaps) # nolint
# library(geojsonsf) # nolint
# library(jsonlite) imported by bcmaps

#library(cowplot) for aligning multiple plots # nolint
#library(patchwork) # nolint

# library(arrow) # nolint
# library(duckdb) # nolint

# Load the rlang package for the bang-bang operator - imported by cowplot
# library(rlang) # nolint

# functions for plotting maps
# source("../../git-repos/github/bcstats-ses/src/utils.R") # nolint: 

# set timeout on file load process
getOption("timeout")
options(timeout = 600)

#------------------------------------------------------------------------------
# geodata team creates drive times files for service bc
# with unique id in place of civic address
# zik files are just zip files, so read_csv can handle them directly.
# currently only service bc locations are needed
# geodata team has removed duplicate rows
# ask geodatabc team - why do we have DAID column in the file? Should be DBID
#------------------------------------------------------------------------------

# data for service bc with unique id
data_folder <- safepaths::use_network_path()
data_path <- glue::glue("{data_folder}/data/raw/20250325/")
file_path <- list.files(data_path, full.names = TRUE, recursive = TRUE)[18]
loc <- gsub(glue::glue("({data_path})(.*)(/locality_)([0-9][0-9][0-9])(.*)"), "\\4", file_path) # nolint: line_length_linter.

outfolder <- glue::glue("{data_folder}/data/processed/locality_{loc}")
if (!dir.exists(outfolder)) {
  dir.create(outfolder)
}

new_da_servicebc_df <- read_csv(file_path, col_types = cols(.default = "c"))

address_sf_with_da <- new_da_servicebc_df %>%
  janitor::clean_names() %>%
  filter(tag == "servicebc") %>% # rows for distance to nearest service bc only
  rename(address_albers_x = site_albers_x, address_albers_y = site_albers_y) %>%
  mutate(daid = str_sub(dissemination_block_id, 1, 8),
         drv_time_sec = as.numeric(drv_time_sec),
         drv_dist = as.numeric(drv_dist), 
         address_albers_x = as.numeric(address_albers_x),
         address_albers_y = as.numeric(address_albers_y)) %>%
  st_as_sf(coords = c("address_albers_x", "address_albers_y"), crs = 3005)

# drop the address coordinates
address_sf_with_da %>%
  st_drop_geometry() %>%
  write_csv(glue::glue("{outfolder}/address_with_da_loc_{loc}.csv"))

#------------------------------------------------------------------------------
# Create a DA level summary table: average drive time and distance
# and number of address. No row missing distance value
# all the addresses and DA information are from geodata team by sampling, 
# therefore not full picture.
#------------------------------------------------------------------------------

avg_dist_drvtime_by_db_service <- address_sf_with_da %>%
  st_drop_geometry() %>%
  group_by(dissemination_block_id, daid) %>%
  summarise(
    avg_drv_time_sec = mean(drv_time_sec, na.rm = TRUE),
    avg_drv_dist = mean(drv_dist, na.rm = TRUE),
    n_address = n_distinct(fid)
  ) %>%
  ungroup()

avg_dist_drvtime_by_db_service %>% view()

avg_dist_drvtime_by_db_service %>%
  write_csv(glue::glue("{outfolder}/db_average_times_dist_loc_{loc}.csv"))

#------------------------------------------------------------------------------
# DA shp file
#------------------------------------------------------------------------------
download.file("https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lda_000b21a_e.zip", # nolint: line_length_linter.
               destfile=glue::glue("{data_folder}/data/raw/boundaries/lda_000a21a_e.zip")) # nolint: line_length_linter.


#file_path <- glue::glue("{data_folder}/data/raw/lda_000a21a_e/lda_000b21a_e.shp") # nolint
file_path <- "data/lda_000a21a_e/lda_000b21a_e.shp"

# # Load the dissemination area shapefile
da_shapefile <- st_read(file_path)
da_shapefile <- da_shapefile %>%
  filter(PRUID == "59") %>%
  st_transform(crs = 3005)

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



# Notes

# Stats Canada Files:
# 1. Dissemination Geographies Relationship File
# https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/dguid-idugd/index2021-eng.cfm?year=21 # nolint

# StatsCan’s 2021 Census uses a hierarchical geographic framework. At one level, census subdivisions (CSDs) represent municipalities and similar administrative areas,  # nolint
# while at a lower level, dissemination areas (DAs) are created by aggregating dissemination blocks into contiguous areas (typically with 400–700 people)  # nolint
# that usually nest within CSD boundaries.

# To help users link these different levels, Statistics Canada provides a lookup table—the Dissemination Geographies Relationship File—which uses  # nolint
# unique identifiers (DGUIDs) to connect DAs to higher geographic units such as CSDs, census tracts, and beyond.  # nolint
# This file (along with related correspondence files) lets analysts cross-reference and integrate data across the geographic hierarchy. # nolint

# 2. boundary files
# https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/index2021-eng.cfm?year=21 # nolint
# landing page for Downloadable boudary files, many options.  Including cartographic and digital and three types of geographic area: administrative, statistical and non-standard # nolint

# General Notes
# Geodata team has updated the data with DA id but can add any admin boundary id if requested # nolint
# Geodata fixed those addresses that does not have valid coordinates or are not connected to the road network. - check if this has been done in our files?   # nolint