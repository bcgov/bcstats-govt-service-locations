library(tidyverse)
library(safepaths)
library(glue)
library(janitor)

library(sf)

#library(cancensus) # nolint
#library(cansim) # nolint

#library(bcmaps) # nolint
#library(geojsonsf) # nolint

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
outfolder <- glue::glue("{data_folder}/data/processed/")
new_da_servicebc_file_path = glue::glue("{data_folder}/data/raw/20250324/Langford_min_dist_adjusted/locality_909_nearest_with_admin_id_servicebc_20250324_152921_no_errors.csv") # nolint: line_length_linter.
new_da_servicebc_df <- read_csv(new_da_servicebc_file_path)

address_sf_with_da <- new_da_servicebc_df %>%
  janitor::clean_names() %>%
  filter(tag == "servicebc") %>% # rows for distance to nearest service bc only
  rename(address_albers_x = site_albers_x, address_albers_y = site_albers_y) %>%
  st_as_sf(coords = c("address_albers_x", "address_albers_y"), crs = 3005)

address_sf_with_da %>%
  st_drop_geometry() %>%
  write_csv(glue::glue("{outfolder}/address_with_da_langford.csv"))

#------------------------------------------------------------------------------
# Create a DA level summary table: average drive time and distance
# and number of address. No row missing distance value
# all the addresses and DA information are from geodata team by sampling, 
# therefore not full picture.
#------------------------------------------------------------------------------

avg_dist_drvtime_by_da_service <- address_sf_with_da %>%
  st_drop_geometry() %>%
  group_by(daid) %>%
  summarise(
    avg_drv_time_sec = mean(drv_time_sec),
    avg_drv_dist = mean(drv_dist),
    n_address = n_distinct(fid)
  )

# sanity check
avg_dist_drvtime_by_da_service %>%
  filter(is.na(avg)) %>%
  count(daid) %>%
  glimpse()

avg_dist_drvtime_by_da_service %>%
    write_csv(glue::glue("{outfolder}/da_langford.csv"))

#------------------------------------------------------------------------------
# pre-process Dissemination Geographies Relationship File
# currently keeps csds.  Look at DB's etc.
#------------------------------------------------------------------------------

# Dissemination Geographies Relationship File from statscan
# geouid_url = "https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/dguid-idugd/files-fichiers/2021_98260004.zip" # nolint
# download.file(geouid_url, destfile = "./out/2021_98260004.zip") # nolint
#unzip("./out/2021_98260004.zip", exdir = "./out/2021_98260004") # nolint

daid_file_path = "./out/2021_98260004/2021_98260004.csv"

# read csv all columns as strings instead of numbers
bc_daid_df <- read_csv(daid_file_path, col_types = cols(.default = "c")) %>%
  filter(PRDGUID_PRIDUGD == "2021A000259") # BC

bc_cd_daid_mapping <- bc_daid_df %>%
  count(PRDGUID_PRIDUGD, CDDGUID_DRIDUGD, CSDDGUID_SDRIDUGD, DADGUID_ADIDUGD, DBDGUID_IDIDUGD) # 7848 DAs in BC


#------------------------------------------------------------------------------
# preprocess the csd file from statscan - 
# downloading to project folder and then reading it
# doesn't work so download straight from statsca.
# needed here since I want the name of each csd - 
# not needed for the DA file as can be stripped from name
#------------------------------------------------------------------------------

SGC_2021_url = 
  "https://www.statcan.gc.ca/en/statistical-programs/document/sgc-cgt-2021-structure-eng.csv"

sgc_2021_df = read_csv(SGC_2021_url, col_types = cols(.default = "c")) %>%
  filter(str_sub(Code, 1, 2) == "59") %>%
  filter(`Hierarchical structure` %in% c("Census subdivision")) %>%
  select(CSDID = Code, MUN_NAME_2021 = `Class title`) %>%
  write_csv(("./out/bc_csdid_name_2021.csv"))

#------------------------------------------------------------------------------
# join csd names to daid file and do some further processing
# I"m not sure what is being processed here but it creates a map with
# only CD, CDS and DA to the DA name.  7848 DA's total
#------------------------------------------------------------------------------

bc_daid_mapping  <- bc_cd_daid_mapping  %>%
  select(CDDGUID_DRIDUGD, CSDDGUID_SDRIDUGD, DADGUID_ADIDUGD, DBDGUID_IDIDUGD) %>%
  mutate(
    CDID = str_sub(CDDGUID_DRIDUGD, 10, 13),
    CSDID = str_sub(CSDDGUID_SDRIDUGD, 10, 16),
    DAID = str_sub(DADGUID_ADIDUGD, 10, 17),
    DBID = str_sub(DBDGUID_IDIDUGD, 10, 17)
  ) %>%
  select(-CDDGUID_DRIDUGD, -CSDDGUID_SDRIDUGD, -DADGUID_ADIDUGD, -DBDGUID_IDIDUGD) %>%
  select(-DBID) %>%
  distinct() %>%
  #select(-CDDGUID_DRIDUGD, -CSDDGUID_SDRIDUGD, -DADGUID_ADIDUGD, ) %>%
  left_join(sgc_2021_df, by = join_by(CSDID))

bc_daid_mapping %>%
  write_csv(("./out/bc_daid_mapping.csv"))

# errors with row 4 and row 9699
CSD_DA_address_dist_drvtime <- bc_daid_mapping %>%
  left_join(
    address_sf_with_da %>% mutate(DAID = as.character(DAID)),
    by = join_by("DAID" == "DAID"))

# -- List of municipalities
CSD_DA_address_dist_drvtime %>% 
  count(MUN_NAME_2021) %>% View()

town <- "Smithers"
one_town <- CSD_DA_address_dist_drvtime %>% 
  filter(MUN_NAME_2021 == town) %>%
  filter(TAG_2 == "servicebc")

one_town %>% glimpse()

ggplot2::ggplot(one_town, aes(x = ADDRESS_ALBERS_X, y = ADDRESS_ALBERS_Y, color = DRV_TIME_SEC/3600)) +
  geom_point(show.legend = TRUE) +
  coord_quickmap()

ggplot(data = csd_shapefile %>% filter(TAG_2 == "servicebc")) +
    geom_sf(aes(fill = DRV_TIME_SEC, color = "gray"))


# Notes

# Stats Canada Files: 
# 1. Dissemination Geographies Relationship File
# https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/dguid-idugd/index2021-eng.cfm?year=21

# StatsCan’s 2021 Census uses a hierarchical geographic framework. At one level, census subdivisions (CSDs) represent municipalities and similar administrative areas, 
# while at a lower level, dissemination areas (DAs) are created by aggregating dissemination blocks into contiguous areas (typically with 400–700 people) 
# that usually nest within CSD boundaries.

# To help users link these different levels, Statistics Canada provides a lookup table—the Dissemination Geographies Relationship File—which uses 
# unique identifiers (DGUIDs) to connect DAs to higher geographic units such as CSDs, census tracts, and beyond. 
# This file (along with related correspondence files) lets analysts cross-reference and integrate data across the geographic hierarchy.

# Get the Dissemination Geographies Relationship File from statscan, and join the distance dataset to the DA shape file.


# 2. boundary files
# https://www12.statcan.gc.ca/census-recensement/2021/geo/sip-pis/boundary-limites/index2021-eng.cfm?year=21
# landing page for Downloadable boudary files, many options.  Including cartographic and digital and three types of geographic area: administrative, statistical and non-standard


# 3. geographic attribute file
# https://www12.statcan.gc.ca/census-recensement/2021/geo/aip-pia/attribute-attribs/index-eng.cfm
# The Geographic Attribute File contains geographic data at the dissemination block level.



# General Notes
# Geodata team has updated the data with DA id but can add any admin boundary id if requested
# Geodata fixed those addresses that does not have valid coordinates or are not connected to the road network. - check if this has been done in our files?  

# Check these stats:
# 751 CSDs and 7848 DAs in the DA shape file from statsan
# but only 420 CSDUIDs and 6967 DAs in the CSD_DA_list/TMF file from bcstats
# 7326 DAs in distance dataset from geodata team