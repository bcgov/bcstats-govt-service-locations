source("R/settings.R")
source("R/fxns/rural-fxns.R")

# -------------------------- READ DATA -----------------------------------
db_shapefiles <- st_read(glue("{SHAPEFILE_OUT}/full-db-with-location.gpkg")) |>
  rename(geometry = geom) |>
  st_transform(crs = 3005) |>
  select(dbid, csdid, geometry)

popcenter_boundaries <-
  st_read(
    glue("{SRC_DATA_FOLDER}/shapefiles/popcenter-statscan.gpkg"),
    layer = "popcenter_statscan"
  ) |>
  rename(geometry = geom) |>
  st_transform(crs = 3005)

data <- read_csv(glue("{FOR_SBC_OUT}/rural-method-misc/rurality-by-db.csv")) |>
  select(dbid, pcname, rurality) |>
  mutate(dbid = as.character(dbid))

# -------------------------- PREPARE DATA -----------------------------------
data <- data |>
  left_join(db_shapefiles |> select(dbid, "db_geom" = geometry)) |>
  left_join(popcenter_boundaries |> select(pcname, "pc_geom" = geometry))

# uncomment below to filter to subset of popcenters
# locs <- c("Fort St. James", "Ucluelet", "Duncan", "Victoria") # nolint: commented_code_linter.
# data <- data |> filter(pcname %in% locs) # nolint: commented_code_linter.

grps <- data |>
  nest(.by = pcname) |>
  arrange(pcname) |>
  mutate(id = row_number())

# -------------------------- CREATE PLOTS -----------------------------------

plts <- apply(grps, 1, vis_pc)
names(plts) <- grps |> pull(pcname)

grid::current.viewport() # workaround for: https://github.com/tidyverse/ggplot2/issues/2514
for (i in seq_along(plts)) {
  fn <- glue("{FOR_SBC_OUT}/rural-method-misc/{names(plts)[i]}.svg")
  svglite::svglite(file = fn, width = 8, height = 6)
  grid::grid.draw(plts[[i]])
}
dev.off()
