get_usa_map <- function(file = "data-raw/USA-EEZ/eez.shp") {
    geojsonio::geojson_read(file = file, what = "sp")
}

is_in_eez <- function(uuid, long, lat, usa_map) {
    points <- data.frame(
        uuid = uuid,
        long = long,
        lat = lat,
        stringsAsFactors = FALSE
    )
    points <- points[complete.cases(points), ]
    pts <- SpatialPoints(points[, c("long", "lat")],
                         proj4string = CRS(proj4string(usa_map)))

    dplyr::bind_cols(points,
                     data.frame(is_in_eez = !is.na(sp::over(pts, usa_map)$polygonid),
                                stringsAsFactors = FALSE)
                     )
}
