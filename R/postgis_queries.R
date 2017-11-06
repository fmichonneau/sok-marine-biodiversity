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


add_within_polygon_to_db <- function(db_table) {

    db <- sok_db()

    if (!"geom_point" %in% dbListFields(db, db_table)) {
        dbExecute(db,
                  glue::glue("ALTER TABLE {db_table} ADD COLUMN geom_point geometry DEFAULT NULL;"))
    }

    message("Converting lat/long into geometry ...", appendLF = FALSE)
    dbExecute(db,
              glue::glue("UPDATE {db_table} ",
                         "SET ",
                         "  geom_point = ST_SetSRID(ST_MakePoint(decimallongitude, decimallatitude), 4326) ",
                         "WHERE geom_point IS NULL;"))
    message(" DONE.")

    ## Create within_* fields if they don't exist
    purrr::map(list("within_eez", "within_gom", "within_pnw"), function(x) {
        if (! x %in% dbListFields(db, db_table))
            dbExecute(db, glue::glue("ALTER TABLE {db_table} ADD COLUMN {x} BOOL DEFAULT NULL;"))
        })

    contains_queries <- glue::glue(
        "UPDATE {db_table} ",
        "SET ",
        "  within_{col} = ST_Contains({maps}, {db_table}.geom_point) ",
        "FROM (SELECT geom_polygon AS {maps} FROM maps WHERE area_id = '{maps}') AS foo ",
        "WHERE within_{col} IS NULL;", maps = c("map_usa", "map_gom", "map_pnw"),
        col = c("eez", "gom", "pnw")
        )

    message("Figuring out whether the points fall within the polygons ... ", appendLF = FALSE)
    res <- purrr::map_int(contains_queries, function(x) {
                      dbExecute(db, x)
                  })
    message("DONE.")

}
