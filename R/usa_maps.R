get_map_usa <- function(file) {
    res <- geojsonio::geojson_read(x = file, what = "sp")
    res <- geojsonio::geojson_json(res)
    saveRDS(res, file = "data/map_eez_usa.rds")
    res
}

is_in_eez <- function(points, map_usa) {
    ##message("Sit back, relax, it's going to be a while...")
    pts <- geojsonio::geojson_json(points, lat = "lat", lon = "long")
    res <- lawn::lawn_within(pts, map_usa)
    res
}

is_in_eez_records <- function(filtered_data, map_usa, coords_store = eez_coords_store()) {
    res_lawn <- filtered_data %>%
        dplyr::mutate(
                   lat = round(decimallatitude, 1),
                   long = round(decimallongitude, 1),
                   coord_key = paste(lat, long, sep = "|"))

    simplified_coords <-  res_lawn %>%
        dplyr::select(coord_key, lat, long) %>%
        dplyr::distinct(coord_key, .keep_all = TRUE)

    res_is_in_eez <- is_in_eez(simplified_coords, map_usa)$features$geometry$coordinates %>%
                        lapply(function(x) data.frame(
                                               coord_key = paste(x[2], x[1], sep = "|"),
                                               lat = x[2],
                                               lon = x[1],
                                               stringsAsFactors = FALSE)) %>%
                        dplyr::bind_rows()

    res_lawn$is_in_eez <- res_lawn$coord_key %in% res_is_in_eez$coord_key
    res_lawn
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
