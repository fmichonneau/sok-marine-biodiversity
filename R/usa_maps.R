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

eez_coords_hook <- function(key, namespace) {
    coords <- unlist(strsplit(key, "\\|"))
    pts <- data.frame(
        coords = key,
        lat = as.numeric(coords[1]),
        long = as.numeric(coords[2]),
        stringsAsFactors = FALSE
    )
    !is.null(is_in_eez(pts, readRDS("data/map_eez_usa.rds"))$features$properties)
}

eez_coords_store <- function(path = "data/eez_coords_storr") {
    invisible(
        storr::storr_external(
                   storr::driver_rds(path),
                   eez_coords_hook
               )
    )
}


is_in_eez_records <- function(filtered_data, map_usa, coords_store = eez_coords_store()) {
    p <- progress::progress_bar$new(total = nrow(filtered_data))
    res_lawn <- filtered_data %>%
        dplyr::mutate(
                   lat = round(decimallatitude, 1),
                   long = round(decimallongitude, 1)) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
                   is_in_eez = { p$tick(); coords_store$get(paste(lat, long, sep = "|")) }
               )
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
