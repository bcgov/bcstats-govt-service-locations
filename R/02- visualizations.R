
source("configuration.R") # load libraries and other settings
source("fxns/fxns.R") # functions for plotting maps

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