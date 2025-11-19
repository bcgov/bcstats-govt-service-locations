# -------------------------- DECLARE FUNCTION ----------------------------
vis_pc <- function(data) {
  # this function allows you to visualize an region (i.e. popcenter) and it's intersecting blocks (i.e. DB's)

  cat(paste0("Processing plot ", data$id, ": ", data$pcname), "\n")
  cat(paste0("\t", "Count of DB's: ", length(unique(data$data$dbid)), "\n"))

  sf_data <- st_as_sf(data$data, crs = 3005)
  bbox <- st_bbox(sf_data$pc_geom) # use db_geom instead, to zoom out from popcenter
  title <- paste0(
    "Population Center: ",
    paste0(data$pcname, collapse = ", "),
    "\n",
    "(N) DBID's: ",
    paste0(length(unique(data$data$dbid)))
  )

  p <- ggplot2::ggplot() +
    geom_sf(
      data = sf_data |> distinct(), # plot boundary one time only
      aes(geometry = pc_geom, color = "population center"),
      fill = NA,
      linewidth = 0.7
    ) +
    geom_sf(
      data = sf_data,
      aes(geometry = db_geom, fill = rurality),
      alpha = 0.2,
      linewidth = 0.1
    ) +
    MAP_THEME +
    coord_sf(
      datum = st_crs(3005),
      xlim = c(bbox$xmin - 1000, bbox$xmax + 1000),
      ylim = c(bbox$ymin - 1000, bbox$ymax + 1000)
    ) +
    labs(
      y = "",
      x = "",
      title = title
    ) +
    theme(
      axis.text = element_text(size = 10),
      plot.title = element_text(size = 12),
      legend.title = element_blank()
    ) +
    scale_color_manual(
      values = c("population center" = "black")
    )

  # return the grob object
  ggplot2::ggplotGrob(p)
}

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
