get_usa_map <- function(file = "data-raw/USA-EEZ/eez.shp", out = "data/usa_eez.rds") {
    if (file.exists(out))
        return(readRDS(out))
    res <- geojsonio::geojson_read(x = file, what = "sp")
    saveRDS(res, file = out)
}

lawn_get_is_in_eez <- function(points, cache = "data/is_in_eez.rds") {
    if (file.exists(cache)) {
        message("Using cached data, it won't work if there are new records in the iDigBio data!")
        res <- readRDS(cache)
    }
    else {
        message("Sit back, relax, it's going to be a while... (about 45 min)")
        tt <- geojsonio::geojson_json(get_usa_map())
        pts <- geojsonio::geojson_json(points, lat = "lat", lon = "long")
        res <- lawn::lawn_within(pts, tt)
        saveRDS(res, file = cache)
    }
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
