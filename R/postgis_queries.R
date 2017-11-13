insert_map_into_db <- function(map) {
    ## First we create a table that holds all the layers of the shape object.
    ## This table is named like the `map` object.
    name <- deparse(substitute(map))
    sp_map <- geojsonio::geojson_sp(map)
    rpostgis::pgInsert(db, name = c("public", name),
                       data.obj = sp_map, overwrite = TRUE,
                       row.names = FALSE)

    db <- sok_db()

    ## Then we had the union of the layers of these objects into a master table
    ## that holds all the polygons we use: map_usa, map_gom, map_pnw.  But
    ## first, we need to make sure that the `maps` table exists, and create it
    ## otherwise.
    if (!dbExistsTable(db, "maps")) {
        dbExecute(db,
                  glue::glue("CREATE TABLE maps (",
                             "area_id TEXT PRIMARY KEY, ",
                             "geom_polygon GEOMETRY",
                             ");"))
    }

    ## If the row for the map already exists, we first delete it
    q_exists <- dbSendQuery(db, glue::glue("SELECT * FROM maps WHERE area_id='{name}';"))
    res_exists <- dbFetch(q_exists)
    if (nrow(res_exists) > 0) {
        dbExecute(db, glue::glue("DELETE FROM maps WHERE area_id='{name}';"))
    }

    ## that's where it all happens
    dbExecute(db,
              glue::glue("INSERT INTO maps (area_id, geom_polygon)",
                         "VALUES ('{name}', (SELECT ST_Union({name}.geom) ",
                         "FROM {name}));"))
}

## db: database connection
## src_table: the table in the database that holds the coordinates that need to
## be filtered for geography
add_unique_coords_to_db <- function(db, src_table) {

    maps <- c("map_usa", "map_gom", "map_pnw")

    ## make sure maps table exists and it contains all the data we need
    if (!dbExistsTable(db, "maps")) stop("'maps' table doesn't exist.")
    q <- dbSendQuery(db, "SELECT maps.area_id FROM maps")
    res <- dbFetch(q)
    all(maps %in% dplyr::pull(res, area_id))

    if (!dbExistsTable(db, "unique_coords")) {
        q_create <- c(
            "CREATE TABLE unique_coords (",
            "decimallatitude REAL NOT NULL,",
            "decimallongitude REAL NOT NULL,",
            "geom_point GEOMETRY DEFAULT NULL,",
            "within_eez BOOL DEFAULT NULL,",
            "within_gom BOOL DEFAULT NULL, ",
            "within_pnw BOOL DEFAULT NULL, ",
            "PRIMARY KEY (decimallatitude, decimallongitude)",
            ");")
        dbExecute(db, glue::collapse(q_create))
    }

    ## The join takes a long time, but it is way slower to calculate st_contains
    ## on the table directly, instead of on only the unique coordinates.

    ## create temporary table with all coordinates
    ## 1. extract the unique coordinates
    dbExecute(db,
              glue::glue("CREATE TEMPORARY TABLE tmp_coords ",
                         "AS SELECT DISTINCT {src_table}.decimallatitude, {src_table}.decimallongitude ",
                         "FROM {src_table}", src_table = src_table))
    dbExecute(db, "ANALYZE tmp_coords")
    on.exit(dbExecute(db, "DISCARD TEMP;"))

    ## 2. insert them into the database
    message("add coordinates to unique_coords table ...", appendLF = FALSE)
    dbExecute(db,
              glue::glue("INSERT INTO unique_coords ",
                         "SELECT * FROM tmp_coords ",
                         "LEFT JOIN unique_coords USING (decimallatitude, decimallongitude) ",
                         "WHERE within_eez IS NULL;"))
    message(" DONE.")

    ## 3. convert new records with MakePoint
    dbExecute(db,
              glue::glue("UPDATE unique_coords ",
                         "SET ",
                         "  geom_point = ST_SetSRID(ST_MakePoint(decimallongitude, decimallatitude), 4326) ",
                         "WHERE geom_point IS NULL;"))

    ## 4. compute whether the coordinates are in the polygon
    contains_queries <- glue::glue(
        "UPDATE unique_coords ",
        "SET ",
        "  within_{col} = ST_Contains({maps}, unique_coords.geom_point) ",
        "FROM (SELECT geom_polygon AS {maps} FROM maps WHERE area_id = '{maps}') AS foo ",
        "WHERE within_{col} IS NULL;", maps = maps, col = c("eez", "gom", "pnw")
        )

    res <- purrr::map_int(contains_queries, function(x) {
        message(glue::glue("figure out points within map, ..."), appendLF = FALSE)
        dbExecute(db, x)
        })
    message(" DONE.")
    if (any(res < 0)) stop("something went wrong")
}


add_within_polygon_to_db <- function(db_table) {

    db <- sok_db()

    ## add coordinates from data table in unique_coords table
    add_unique_coords_to_db(db, db_table)

    ## make sure unique coords table exists
    if (!dbExistsTable(db, "unique_coords"))
        stop("something is very wrong: ", sQuote("unique_coords"),
             " table doesn't exist.")

    message(glue::glue("Working on joining unique_coords and {db_table} ..."), appendLF = FALSE)
    dbExecute(db,
              glue::glue(
                        "UPDATE {db_table} ",
                        "SET (within_eez, within_gom, within_pnw) = ",
                        "(SELECT unique_coords.within_eez, unique_coords.within_gom, unique_coords.within_pnw ",
                        "FROM unique_coords ",
                        "WHERE unique_coords.decimallatitude = {db_table}.decimallatitude AND ",
                        "      unique_coords.decimallongitude = {db_table}.decimallongitude );"))
    message(" DONE.")

}
