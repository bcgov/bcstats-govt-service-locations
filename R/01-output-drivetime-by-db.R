library(tidyverse)
library(safepaths)
library(glue)
library(janitor)
library(e1071)

library(sf)

# library(cancensus) # nolint
# library(cansim) # nolint

# library(bcmaps) # nolint
# library(geojsonsf) # nolint
# library(jsonlite) imported by bcmaps

#library(cowplot) for aligning multiple plots # nolint
#library(patchwork) # nolint
# library(duckdb) # nolint

# Load the rlang package for the bang-bang operator - imported by cowplot
# library(rlang) # nolint

# functions for plotting maps
source("fxns/fxns.R")

# set timeout on file load process
getOption("timeout")
options(timeout = 600)

#------------------------------------------------------------------------------
# geodata team creates drive times files for service bc
# with unique id in place of civic address
# zik files are just zip files, so read_csv can handle them directly.
# currently only service bc locations are needed
# geodata team has removed duplicate rows
# Langford: locality 909
# Smithers: locality 227
# Dawson Creek: locality 213
# Kamploops: Locality 420
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
# To do: DA or DB?
# To do: is this the full sample of data?
#------------------------------------------------------------------------------

avg_dist_drvtime_by_db_service <- address_sf_with_da %>%
  st_drop_geometry() %>%
  group_by(dissemination_block_id, daid) %>%
  summarise(
    mn_drv_time_sec = mean(drv_time_sec, na.rm = TRUE),
    mn_drv_dist = mean(drv_dist, na.rm = TRUE),
    qnt0_drv_time_sec = quantile(drv_time_sec, probs = 0, na.rm = TRUE),
    qnt1_drv_time_sec = quantile(drv_time_sec, probs = 0.25, na.rm = TRUE),
    qnt2_drv_time_sec = quantile(drv_time_sec, probs = 0.5, na.rm = TRUE),
    qnt3_drv_time_sec = quantile(drv_time_sec, probs = 0.75, na.rm = TRUE),
    qnt4_drv_time_sec = quantile(drv_time_sec, probs = 1, na.rm = TRUE),
    qnt0_drv_dist = quantile(drv_dist, probs = 0, na.rm = TRUE),
    qnt1_drv_dist = quantile(drv_dist, probs = 0.25, na.rm = TRUE),
    qnt2_drv_dist = quantile(drv_dist, probs = 0.5, na.rm = TRUE),
    qnt3_drv_dist = quantile(drv_dist, probs = 0.75, na.rm = TRUE),
    qnt4_drv_dist = quantile(drv_dist, probs = 1, na.rm = TRUE),
    var_drv_time_sec = var(drv_time_sec, na.rm = TRUE),
    var_drv_dist = var(drv_dist, na.rm = TRUE),
    skw_drv_time_sec = skewness(drv_time_sec, na.rm = TRUE, type = 1),
    skw_drv_dist = skewness(drv_dist, na.rm = TRUE, type = 1),
    kurt_drv_time_sec = kurtosis(drv_time_sec, na.rm = TRUE, type = 1),
    kurt_drv_dist = kurtosis(drv_dist, na.rm = TRUE, type = 1),
    n_address = n_distinct(fid)
  ) %>%
  ungroup()

avg_dist_drvtime_by_da_service <- address_sf_with_da %>%
  st_drop_geometry() %>%
  group_by(daid) %>%
  summarise(
    mn_drv_time_sec = mean(drv_time_sec, na.rm = TRUE),
    mn_drv_dist = mean(drv_dist, na.rm = TRUE),
    qnt0_drv_time_sec = quantile(drv_time_sec, probs = 0, na.rm = TRUE),
    qnt1_drv_time_sec = quantile(drv_time_sec, probs = 0.25, na.rm = TRUE),
    qnt2_drv_time_sec = quantile(drv_time_sec, probs = 0.5, na.rm = TRUE),
    qnt3_drv_time_sec = quantile(drv_time_sec, probs = 0.75, na.rm = TRUE),
    qnt4_drv_time_sec = quantile(drv_time_sec, probs = 1, na.rm = TRUE),
    qnt0_drv_dist = quantile(drv_dist, probs = 0, na.rm = TRUE),
    qnt1_drv_dist = quantile(drv_dist, probs = 0.25, na.rm = TRUE),
    qnt2_drv_dist = quantile(drv_dist, probs = 0.5, na.rm = TRUE),
    qnt3_drv_dist = quantile(drv_dist, probs = 0.75, na.rm = TRUE),
    qnt4_drv_dist = quantile(drv_dist, probs = 1, na.rm = TRUE),
    var_drv_time_sec = var(drv_time_sec, na.rm = TRUE),
    var_drv_dist = var(drv_dist, na.rm = TRUE),
    skw_drv_time_sec = skewness(drv_time_sec, na.rm = TRUE, type = 1),
    skw_drv_dist = skewness(drv_dist, na.rm = TRUE, type = 1),
    kurt_drv_time_sec = kurtosis(drv_time_sec, na.rm = TRUE, type = 1),
    kurt_drv_dist = kurtosis(drv_dist, na.rm = TRUE, type = 1),
    n_address = n_distinct(fid)
  ) %>%
  ungroup()

avg_dist_drvtime_by_db_service %>% view()
avg_dist_drvtime_by_da_service %>% view()

avg_dist_drvtime_by_db_service %>%
  write_csv(glue::glue("{outfolder}/db_average_times_dist_loc_{loc}.csv"))

avg_dist_drvtime_by_da_service %>%
  write_csv(glue::glue("{outfolder}/da_average_times_dist_loc_{loc}.csv"))

#------------------------------------------------------------------------------
# DA shp file
# To do: collect db shapefile from statistics candada
#------------------------------------------------------------------------------
download.file("https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/files-fichiers/lda_000b21a_e.zip", # nolint: line_length_linter.
              destfile = glue::glue("{data_folder}/data/raw/boundaries/lda_000a21a_e.zip")) # nolint: line_length_linter.


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

