library(tidyverse)
library(glue)
library(janitor)
library(sf)

source("R/settings.R")

crosswalk_file_path <- glue("{SRC_DATA_FOLDER}/da-db-loc-crosswalk.csv")
da_shapefile_path <-  glue("{RAW_DATA_FOLDER}/statscan/lda_000b21a_e/lda_000b21a_e.shp")
db_shapefile_path <-  glue("{RAW_DATA_FOLDER}/statscan/ldb_000b21a_e/ldb_000b21a_e.shp")
shapefile_out <- glue("{SRC_DATA_FOLDER}/shapefiles/")


# -----------------------------------------------------------------------------------------
# Load and prepare da-db-location crosswalk file
# -----------------------------------------------------------------------------------------

crosswalk <- tryCatch({
  read_csv(crosswalk_file_path, col_types = cols(.default = "c"))
}, error = function(e) {
  stop(glue("Failed to read or process crosswalk file '{crosswalk_file_path}': {e$message}"))
})

if (nrow(crosswalk) == 0) {
  stop("Crosswalk data is empty after processing.")
}

# -----------------------------------------------------------------------------------------
# Load and prepare da shapefile - check for invalid data
# -----------------------------------------------------------------------------------------

da_shapefiles_processed <- tryCatch({
    st_read(da_shapefile_path) %>%
    filter(PRUID == "59") %>%
    st_transform(crs = 3005) %>%
    clean_names() %>%
    select(daid = dauid, landarea, geometry)
}, error = function(e) {
  stop(glue("Failed to read or process da shapefile '{da_shapefile_path}': {e$message}"))
})

if (nrow(da_shapefiles_processed) == 0) {
  stop("DA shapefile data is empty after filtering for BC.")
}

# -----------------------------------------------------------------------------------------
# Load and prepare db shapefile - check for invalid data
# -----------------------------------------------------------------------------------------

db_shapefiles_processed <- tryCatch({
    st_read(db_shapefile_path) %>%
    filter(PRUID == "59") %>%
    st_transform(crs = 3005) %>%
    clean_names() %>%
    select(dissemination_block_id = dbuid, landarea, geometry)

}, error = function(e) {
  stop(glue("Failed to read or process da shapefile '{da_shapefile_path}': {e$message}"))
})

if (nrow(db_shapefiles_processed) == 0) {
  stop("DA shapefile data is empty after filtering for BC.")
}


# -----------------------------------------------------------------------------------------
# Filter shapefile to include only those da/db's in our localities of interest
# could do some light error checking here, but we don't have a true mapping from
# location to da/db (locality is still not definded and we don't have shapefiles for those)
# -----------------------------------------------------------------------------------------

da_with_location  <- db_shapefiles_processed  %>%
   inner_join(crosswalk %>% distinct(daid, dissemination_block_id, location_id), by = "dissemination_block_id")

if (nrow(db_with_location) == 0) {
  stop("No DAs after joining with the crosswalk.")
}

da_with_location  <- da_shapefiles_processed  %>%
   inner_join(crosswalk %>% distinct(daid, location_id), by = "daid")

if (nrow(da_with_location) == 0) {
  stop("No DAs after joining with the crosswalk.")
}


# -----------------------------------------------------------------------------------------
# write processed da/db shapefiles to source folder
# -----------------------------------------------------------------------------------------
st_write(da_with_location, glue("{shapefile_out}/processed_da_with_location.shp"))
st_write(da_with_location, glue("{shapefile_out}/processed_db_with_location.shp"))

