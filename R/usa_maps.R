get_map_usa <- function(file) {
    res <- geojsonio::geojson_read(x = file, what = "sp")
    res <- geojsonio::geojson_json(res)
    saveRDS(res, file = "data/map_eez_usa.rds")
    res
}


get_felder_gom_map <- function(file) {
    gom_b <- rgdal::readOGR(file)
    wkt_string <- "POLYGON((-83.3 25.2, -80.4 25.2, -81.1 23.2, -83.3 23.01, -83.3 25.2))"
    east_gom <- rgeos::readWKT(wkt_string)
    proj4string(east_gom) <- proj4string(gom_b)
    rgeos::gUnion(gom_b, east_gom) %>%
        geojsonio::geojson_json()
}

get_kozloff_map <- function() {
    wkt_string <- "POLYGON((-129.43 51.29, -126.9 51.29, -122.2 49.18, -122.2 47, -123.5 46.8, -123.5 43.3, -124 43.3, -124.3 46.9, -129.43 51.29))"
    rgeos::readWKT(wkt_string, p4s = "+proj=longlat +datum=WGS84") %>%
        geojsonio::geojson_json()
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
    dplyr::select(res_lawn, -lat, -long, -coord_key)
}


is_in_gulf_of_mexico <- function(lon, lat) {
    if (is.null(lon) | is.null(lat))
        stop("NULL values for the coordinates. Incorrect column specified?")
    ## coordinates used by Felder et al, but in our case, it will
    ## cover a smaller area as the iDigBio query currently only
    ## applies to EEZ waters
    res <- (lon > -100 & lon < -80.5) &
        (lat > 17 & lat < 31)
    ## remove Jacksonville area points
    res[lon > -82 & lat > 27] <- FALSE
    res
}

is_in_pnw <- function(lon, lat) {
    if (is.null(lon) | is.null(lat))
        stop("NULL values for the coordinates. Incorrect column specified?")
    (lon > -125 & lon < -122) &
        (lat > 47 & lat < 49)
}

is_in_gulf_of_mexico_records <- function(idig) {
    ## TODO -- this is quick as it potentially includes records that
    ## don't have correct GPS coordinates and are in land. So, we use
    ## the intersect of being both in the EEZ and in the GOM. If we
    ## extend the iDigBio search outside de US EEZ to also include
    ## Mexico and Cuba waters, then this will need to be modified.
    idig$is_in_gom <- is_in_gulf_of_mexico(idig$decimallongitude,
                                           idig$decimallatitude) &
        idig$is_in_eez
    idig
}

is_in_pnw_records <- function(idig) {
    idig$is_in_pnw <- is_in_pnw(idig$decimallongitude,
                                idig$decimallatitude)
    idig
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
