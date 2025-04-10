library(tidyverse)
library(glue)
library(janitor)
library(sf)

source("R/settings.R")

crosswalk_file_path <- glue("{SRC_DATA_FOLDER}/da-db-loc-crosswalk.csv")
da_shapefile_path <-  glue("{RAW_DATA_FOLDER}/statscan/lda_000b21a_e/lda_000b21a_e.shp")
da_shapefile_out <- glue("{SRC_DATA_FOLDER}/processed_da_shapefiles.shp")
# db_file_path <-  glue("{RAW_DATA_FOLDER}/statscan/ldb_000b21a_e/ldb_000b21a_e.shp") lets do da only for now



# -----------------------------------------------------------------------------------------
# Load and prepare da-db-location crosswalk file
# -----------------------------------------------------------------------------------------

crosswalk <- tryCatch({
  read_csv(crosswalk_file_path, col_types = cols(.default = "c")) %>%
    distinct(daid, location_id)
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
# Filter da shapefile to include only those da's in our localities of interest
# could do some light error checking here, but we don't have a true mapping from
# location to da (since locality is still undefinded and we don't have shapefiles for those)
# -----------------------------------------------------------------------------------------

test <- da_shapefiles_processed  %>%
   inner_join(crosswalk, by = "daid")

# -----------------------------------------------------------------------------------------
# write processed da shapefiles to source folder
# -----------------------------------------------------------------------------------------
st_write(da_with_location, output_gpkg_path, delete_layer = TRUE)

