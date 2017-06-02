get_map_usa <- function(file) {
    res <- geojsonio::geojson_read(x = file, what = "sp")
    res <- geojsonio::geojson_json(res)
    saveRDS(res, file = "data/map_eez_usa.rds")
    res
}

get_felder_map_gom <- function(file) {
    gom_b <- rgdal::readOGR(file)
    wkt_string <- "POLYGON((-83.3 25.2, -80.4 25.2, -81.1 23.2, -83.3 23.01, -83.3 25.2))"
    east_gom <- rgeos::readWKT(wkt_string)
    proj4string(east_gom) <- proj4string(gom_b)
    rgeos::gUnion(gom_b, east_gom) %>%
        geojsonio::geojson_json()
}

get_map_pnw <- function() {
    wkt_string <- "POLYGON((-129.43 51.29, -126.9 51.29, -122.2 49.18, -122.2 47, -123.5 46.8, -123.5 43.3, -124 43.3, -124.3 46.9, -129.43 51.29))"
    rgeos::readWKT(wkt_string, p4s = "+proj=longlat +datum=WGS84") %>%
        geojsonio::geojson_json()
}

is_within_map <- function(points, map) {
    pts <- geojsonio::geojson_json(points, lat = "lat", lon = "long")
    res <- lawn::lawn_within(pts, map)
    res
}

is_within_map_records <- function(area) {
    function(filtered_data, map) {
        res_lawn <- filtered_data %>%
            dplyr::mutate(
                       lat = round(decimallatitude, 1),
                       long = round(decimallongitude, 1),
                       coord_key = paste(lat, long, sep = "|"))

        simplified_coords <-  res_lawn %>%
            dplyr::select(coord_key, lat, long) %>%
            dplyr::distinct(coord_key, .keep_all = TRUE)

        res_is_within <- is_within_map(simplified_coords, map)$features$geometry$coordinates %>%
                           purrr::map_df(function(x)
                                      data.frame(
                                          coord_key = paste(x[2], x[1], sep = "|"),
                                          lat = x[2],
                                          lon = x[1],
                                          stringsAsFactors = FALSE))

        res_lawn[[paste0("is_in_", area)]] <- res_lawn$coord_key %in% res_is_within$coord_key
        dplyr::select(res_lawn, -lat, -long, -coord_key)
    }
}

is_within_eez_records <- is_within_map_records("eez")
is_within_pnw_records <- is_within_map_records("pnw")
is_within_gom_records <- is_within_map_records("gom")


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
