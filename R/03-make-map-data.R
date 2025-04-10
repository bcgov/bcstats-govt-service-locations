library(tidyverse)
library(glue)
library(janitor)
library(e1071)
library(sf)

source("R/settings.R")
source("R/fxns/pre-processing.R")
source("R/fxns/calculations.R")

da_file_path <-  glue("{RAW_DATA_FOLDER}/statscan/lda_000b21a_e/lda_000b21a_e.shp")
db_file_path <-  glue("{RAW_DATA_FOLDER}/statscan/ldb_000b21a_e/ldb_000b21a_e.shp")

da_shapefile <- st_read(da_file_path)
da_shapefile <- da_shapefile %>%
  filter(PRUID == '59') %>%
  st_transform(crs = 3005)

#  %>%
#  clean_names() %>%
# select(daid = dauid, landarea, geometry) # Select relevant columns

data <- read_csv(glue("{SRC_DATA_FOLDER}/da_average_times_dist_all_locs.csv")) %>%
  clean_names() %>%
  mutate(across(c(daid, loc), as.character)) %>%
  filter(loc == "213") %>% # Filter for locality 213
  select(daid, loc, drv_dist_mean)


# read in shape files (db and da)

# filter on db's in our localities (need a mapping table)

db_shapefile <- st_read(db_file_path)
db_shapefile <- db_shapefile %>%
  filter(PRUID == '59') %>%
  st_transform(crs = 3005)

# write to source folder



# filter on da's in our localities (need a mapping table)



# 