## generate the geographic grid from a SpatialPolygons object
get_bounding_box <- function(map, cellsize = .5) {
  box <- sp::bbox(map)
  gt <- sp::GridTopology(c(box[1, 1], box[2, 1]),
    cellsize = rep(cellsize, 2), rep(100, 2)
  )
  gr <- as(
    as(sp::SpatialGrid(gt, CRS(proj4string(map))), "SpatialPixels"),
    "SpatialPolygons"
  )
  it <- rgeos::gIntersects(map, gr, byid = TRUE)
  gr[it]
}

## convert the SpatialGrid object into a data frame that contains the
## coordinates of the corners of square of the geographic grid
bb_to_df <- function(bb) {
  res <- lapply(seq_along(bb), function(i) {
    data.frame(
      xmin = xmin(bb[i]), ymax = ymax(bb[i]),
      xmax = xmax(bb[i]), ymin = ymin(bb[i])
    )
  })
  res <- dplyr::bind_rows(res)
  res <- mutate_all(res, round, digits = 3)
  res <- mutate(res, key = paste(xmin, ymax, xmax, ymin, sep = "|"))
  res
}

## from the map of the EEZ from the USA, create a data frame that
## contains the coordinates for a grid that spans the area. This grid
## will be used to do an iDigBio search by geography
generate_bounding_boxes <- function(map_eez, cellsize = .5) {
  map_eez_sp_df <- geojson_sp(map_eez)

  map_eez_fort <- ggplot2::fortify(map_eez_sp_df)
  res <- lapply(levels(map_eez_fort$piece), function(i) {
    SpatialPolygons(
      list(
        Polygons(
          list(Polygon(subset(map_eez_fort, piece == i)[, c("long", "lat")])),
          ID = 1
        )
      ),
      proj4string = CRS(proj4string(map_eez_sp_df))
    )
  }) %>%
    lapply(get_bounding_box, cellsize = cellsize) %>%
    lapply(bb_to_df)
  names(res) <- levels(map_eez_fort$piece)

  dplyr::bind_rows(res, .id = "piece")
}
