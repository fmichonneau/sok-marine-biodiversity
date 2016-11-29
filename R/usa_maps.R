get_usa_map <- function(file) {
    res <- geojsonio::geojson_read(x = file, what = "sp")
    geojsonio::geojson_json(res)
}

is_in_eez <- function(points, map_usa) {
    message("Sit back, relax, it's going to be a while... (about 45 min)")
    pts <- geojsonio::geojson_json(points, lat = "lat", lon = "long")
    res <- lawn::lawn_within(pts, map_usa)
    res
}



### this approach was way too slow!

## fetch_eez_coords <- function(key, namespace) {
##     if (is.na(key))
##         return(NA)
##     usa_map <- get_usa_map()
##     coords <- unlist(strsplit(key, ":"))
##     lat <- as.numeric(coords[1])
##     lon <- as.numeric(coords[2])
##     pts <- data.frame(long = lon, lat = lat, stringsAsFactors = FALSE)
##     pts <- SpatialPoints(pts, proj4string = CRS(proj4string(usa_map)))
##     in_eez <- !is.na(sp::over(pts, usa_map)$polygonid)
##     in_eez
## }

## store_eez_coords <- function(path = "data/eez_coords_storr") {
##     invisible(storr::storr_external(
##                          storr::driver_rds(path = path, mangle_key = TRUE),
##                          fetch_eez_coords)
##               )
## }


## get_is_in_eez <- function(uuid, long, lat) {
##     points <- data.frame(
##         uuid = uuid,
##         long = long,
##         lat = lat,
##         stringsAsFactors = FALSE
##     )
##     res <- logical(nrow(points))
##     pb <- progress::progress_bar$new(total = nrow(points))
##     for (i in seq_len(nrow(points))) {
##         if (any(is.na(c(points$lat[i], points$long[i]))))
##             res[i] <- NA
##         coords <- paste(points$lat[i], points$long[i], sep = ":")
##         res[i] <- store_eez_coords()$get(coords)
##         pb$tick()
##     }
##     res
## }
