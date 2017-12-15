get_map_eez <- function(file) {
    res <- geojsonio::geojson_read(x = file, method = "local", what = "sp")
    geojsonio::geojson_json(res)
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

is_within_map_records <- function(d, map_name) {
    map_name <- match.arg(map_name, c("eez", "gom", "pnw"))

    stopifnot(exists(c("decimallatitude", "decimallongitude"), d))

    d_to_insert <- dplyr::select(d,
                       decimallatitude,
                       decimallongitude) %>%
        dplyr::filter(!is.na(decimallatitude),
                      !is.na(decimallongitude)) %>%
        dplyr::mutate(hash = paste(decimallatitude, decimallongitude, sep = "-")) %>%
        dplyr::distinct(hash, .keep_all = TRUE) %>%
        dplyr::select(-hash)

    if (nrow(d_to_insert) < 1) {
        d[[glue::glue("within_{map_name}")]] <- rep(NA, nrow(d))
        return(d)
    }

    db <- sok_db()
    temp_name <- glue::collapse(c("temp_coords_", format(Sys.time(), "%Y%m%d%H%M%S")))
    on.exit(dbDrop(db, temp_name, display = FALSE))

    dbWriteTable(db, temp_name, d_to_insert, temporary = TRUE)
    dbExecute(db, glue::glue("ALTER TABLE {temp_name} ADD COLUMN geom_point geometry DEFAULT NULL;"))
    dbExecute(db, glue::glue("ALTER TABLE {temp_name} ADD COLUMN within_{map_name} BOOL DEFAULT NULL;"))

    dbExecute(db,
              glue::glue("UPDATE {temp_name} ",
                         "SET ",
                         "  geom_point = ST_SetSRID(ST_MakePoint(decimallongitude, decimallatitude), 4326);"))

    dbExecute(db,
              glue::glue(
                        "UPDATE {temp_name} ",
                        "SET within_{map_name} = ST_Contains(map_{map_name}, {temp_name}.geom_point) ",
                        "FROM (SELECT geom_polygon AS map_{map_name} FROM maps WHERE area_id ='map_{map_name}') AS foo;"))
    q <- dbSendQuery(db, glue::glue("SELECT decimallatitude, decimallongitude, within_{map_name} FROM {temp_name}"))
    res <- dbFetch(q)
    dbClearResult(q)

    col_nm <- rlang::sym(glue::glue("within_{map_name}"))

    dplyr::left_join(d, res, by = c("decimallatitude", "decimallongitude"))
}

is_within_eez_records <- function(d) is_within_map_records(d, "eez")
is_within_pnw_records <- function(d) is_within_map_records(d, "pnw")
is_within_gom_records <- function(d) is_within_map_records(d, "gom")
