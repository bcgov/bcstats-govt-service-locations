# -------------------------- DECLARE FUNCTION ----------------------------
vis_pc_rural_urban <- function(data) {
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
      aes(geometry = db_geom, fill = urban_rural),
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
