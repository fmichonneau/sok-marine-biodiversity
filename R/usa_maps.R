get_map_eez <- function(file) {
  res <- geojsonio::geojson_read(x = file, what = "sp")
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

  add_hash <- . %>%
    dplyr::mutate(hash = paste(decimallatitude, decimallongitude, sep = "|"))

  d_ <- d %>% add_hash()


  d_to_insert <- d_ %>%
    dplyr::select(decimallatitude, decimallongitude, hash) %>%
    dplyr::filter(
      !is.na(decimallatitude),
      !is.na(decimallongitude)
    ) %>%
    dplyr::distinct(hash, .keep_all = TRUE)

  if (nrow(d_to_insert) < 1) {
    d[[glue::glue("within_{map_name}")]] <- rep(NA, nrow(d))
    return(d)
  }

  col_nm <- rlang::sym(glue::glue("within_{map_name}"))
  timestamp <- format(Sys.time(), "%Y%m%d%H%M%S")
  temp_name <- glue::glue_collapse(c("temp_coords_", timestamp))
  full_coords_name <- glue::glue_collapse(c("temp_full_coords_", timestamp))
  to_select <- glue::glue_collapse(paste(full_coords_name, paste0("\"", names(d), "\""), sep = "."), ", ")

  message("Creating table: ", temp_name, " ... ", appendLF = FALSE)
  dbCreateTable(sok_db(), temp_name, d_to_insert, temporary = TRUE)
  message("DONE.")


  message("Creating table: ", full_coords_name, " ... ", appendLF = FALSE)
  dbCreateTable(sok_db(), full_coords_name, d_, temporary = TRUE)
  message("DONE.")

  message(
    "Tables: ",
    paste("- ", DBI::dbListTables(sok_db()), sep = "\n")
  )

  dbExecute(sok_db(), glue::glue("ALTER TABLE {temp_name} ADD COLUMN geom_point geometry DEFAULT NULL;"))
  dbExecute(sok_db(), glue::glue("ALTER TABLE {temp_name} ADD COLUMN within_{map_name} BOOL DEFAULT NULL;"))

  dbExecute(
    sok_db(),
    glue::glue(
      "UPDATE {temp_name} ",
      "SET ",
      "  geom_point = ST_SetSRID(ST_MakePoint(decimallongitude, decimallatitude), 4326);"
    )
  )

  dbExecute(
    sok_db(),
    glue::glue(
      "UPDATE {temp_name} ",
      "SET within_{map_name} = ST_Contains(map_{map_name}, {temp_name}.geom_point) ",
      "FROM (SELECT geom_polygon AS map_{map_name} FROM maps WHERE area_id ='map_{map_name}') AS foo;"
    )
  )

  dbExecute(sok_db(), glue::glue("CREATE INDEX ON {full_coords_name} (hash)"))
  dbExecute(sok_db(), glue::glue("CREATE INDEX ON {temp_name} (hash)"))

  q <- dbSendQuery(
    sok_db(), glue::glue("
           SELECT {to_select}, within_{map_name}  FROM {full_coords_name}
           LEFT JOIN {temp_name} ON {temp_name}.hash = {full_coords_name}.hash"
    )
  )
  res <- dbFetch(q)
  dbClearResult(q)

  dbRemoveTable(sok_db(), temp_name)
  dbRemoveTable(sok_db(), full_coords_name)

  res
}

is_within_eez_records <- function(d) is_within_map_records(d, "eez")
is_within_pnw_records <- function(d) is_within_map_records(d, "pnw")
is_within_gom_records <- function(d) is_within_map_records(d, "gom")
