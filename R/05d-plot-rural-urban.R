source("R/settings.R")
library(grid)
library(svglite)

# -------------------------- DECLARE FUNCTION ----------------------------
vis_pc <- function(data, ttl = NULL, ...) {
  # this function allows you to visualize an region (i.e. popcenter) and it's intersecting blocks (i.e. DB's)

  cat(paste0("Processing plot ", data$id, ": ", data$pcname), "\n")
  cat(paste0("\t", "Count of DB's: ", length(unique(data$data$dbid)), "\n"))

  sf_data <- st_as_sf(data$data, crs = 3005) |> arrange(dbid, branch)
  bbox <- st_bbox(sf_data$pc_geom) # use db_geom instead, to zoom out from popcenter
  title <- paste0(
    "Population Center: ",
    paste0(data$pcname, collapse = ", "),
    "\n",
    "(N) DBID's: ",
    paste0(length(unique(data$data$dbid)))
  )

  custom_labels <- c(
    'new' = 'shared-boundary branch',
    'old' = 'sbc-data-table-fixes branch'
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
      aes(geometry = db_geom, fill = value),
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
    ) +
    facet_wrap(~branch, labeller = as_labeller(custom_labels))

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
  select(dbid, pcname, rurality.old, rurality.new) |>
  mutate(dbid = as.character(dbid))

# -------------------------- PREPARE DATA -----------------------------------
data <- data |>
  left_join(db_shapefiles |> select(dbid, "db_geom" = geometry)) |>
  left_join(popcenter_boundaries |> select(pcname, "pc_geom" = geometry)) |>
  pivot_longer(
    cols = starts_with("rurality"),
    names_to = "branch",
    names_prefix = "rurality."
  )

grps_all <- data |>
  nest(.by = pcname) |>
  arrange(pcname) |>
  mutate(id = row_number()) |>
  mutate(pclabel = paste0(pcname, " (", id, ")"))

# -------------------------- CREATE PLOTS -----------------------------------

locs <- c("Fort St. James", "Ucluelet", "Duncan", "Victoria")
grps <- grps_all[grps_all$pcname %in% locs, ]

plts <- apply(grps, 1, vis_pc)
names(plts) <- grps |> pull(pcname)

# sometimes I have to run this chunk twice
for (i in seq_along(1:3)) {
  #gridExtra::grid.arrange(plts[[i]])
  fn <- glue("{FOR_SBC_OUT}/rural-method-misc/{names(plts)[i]}.svg")
  svglite(file = fn, width = 8, height = 6)
  grid.draw(plts[[i]])
  dev.off()
}
