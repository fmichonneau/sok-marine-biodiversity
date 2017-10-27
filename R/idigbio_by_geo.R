## build the list of iDigBio queries based on the data frame that
## contains the coordinates of each element of the geographic grid.
coords_to_query <- function(coords) {
    qry <- lapply(seq_len(nrow(coords)), function(i) {
        list(basisofrecord = "PreservedSpecimen",    # only extant specimens (no fossils)
             scientificname = list(type = "exists"), # only records that contain something in the species name
             geopoint = list(
                 type = "geo_bounding_box",
                 top_left = list(lon = coords$xmin[i], lat = coords$ymax[i]),
                 bottom_right = list(lon = coords$xmax[i], lat = coords$ymin[i])
             ))
    })
    names(qry) <- coords$key
    qry
}


## fetch hook for the idigbio by geography storr. We use a closure to
## enforce the remake dependency on the data frame that contains the
## coordinates for each element of the grid.
internal_idigbio_by_geo_fetch <- function(coords_qry, key) {
    ridigbio::idig_search_records(rq = coords_qry[[key]], fields = idigbio_fields())
}

make_hook_idigbio_by_geo <- function(coords_qry) {
    force(coords_qry)
    function(key, namespace) {
        v2("... ", appendLF = FALSE)
        res <- try(internal_idigbio_by_geo_fetch(coords_qry, key),
                   silent = TRUE)
        attempts <- 0
        pred <- (inherits(res, "try-error") && !grepl("return more than", res))
        while (pred && attempts <= 3) {
            v3("sleeping ... ")
            Sys.sleep(exp(runif(1) * attempts))
            res <-  try(internal_idigbio_by_geo_fetch(coords_qry, key),
                        silent = TRUE)
            pred <- (inherits(res, "try-error") && !grepl("return more than", res))
            attempts <- attempts + 1
            v3("attempt ... ", attempts)
        }
        if (pred) {
            stop(res)
        } else {
            return(res)
        }
    }
}

## define the storr that contains the iDigBio record for each element
## of the grid.
store_idigbio_by_geo <- function(coords, store_path = "data/idigbio_by_geo") {
    fetch_hook_idigbio_by_geo <- make_hook_idigbio_by_geo(coords)
    storr::storr_external(storr::driver_rds(store_path),
                          fetch_hook_idigbio_by_geo)
}


## Work around the 1e5 records idigbio api limit. We try to get the
## results for the coordinates, if we fail because of the limit, we
## split the area into 4 squares, and look for records within
## them. Its recursive nature should make it work.
get_idigbio_by_geo <- function(coords, q) {

    res <- try(store_idigbio_by_geo(coords)$get(q), silent = TRUE)
    if (inherits(res, "try-error")) {
        if (grepl("return more than", res)) {
            crds <- unlist(strsplit(q, "\\|"))
            mid_lon <- mean(as.numeric(c(crds[1], crds[3])))
            mid_lat <- mean(as.numeric(c(crds[2], crds[4])))
            .r <- list(
                top_left     = c(crds[1], crds[2], mid_lon, mid_lat),
                top_right    = c(mid_lon, crds[2], crds[3], mid_lat),
                bottom_left  = c(crds[1], mid_lat, mid_lon, crds[4]),
                bottom_right = c(mid_lon, mid_lat, crds[3], crds[4])
            ) %>%
                purrr::map(function(x) set_names(x, c("xmin", "ymax", "xmax", "ymin"))) %>%
                purrr::map(as.list) %>%
                purrr::map_df(as_tibble) %>%
                dplyr::mutate(key = paste(xmin, ymax, xmax, ymin, sep = "|")) %>%
                coords_to_query()
            res <- lapply(names(.r), function(x)
                get_idigbio_by_geo(.r, x)) %>%
                dplyr::bind_rows()
            store_idigbio_by_geo(coords)$set(q, res)
            res
        } else stop("Error with iDigBio query")
    } else {
        return(res)
    }
}

validate_idigbio_storr <- function(coords) {
    res <- vapply(names(coords), function(x)
        inherits(store_idigbio_by_geo(coords)$get(x), "try-error"),
        logical(1)
        )
    if (sum(res) > 0) {
        stop("Coordinates at these positions are problematic: ",
             paste(names(which(res)), collapse = ", "))
    } else {
        v3("the coordinates look good")
    }
}




get_coords_idigbio_query <- function(map_usa, cellsize = .5) {
    bb_eez <- generate_bounding_boxes(map_usa, cellsize = cellsize)
    coords_to_query(bb_eez)
}

## for all the coordinates of the bounding boxes, find the iDigBio
## records they contain coords: output of get_coords_idigbio_query
## map_usa: map in GeoJSON db_table: name of table in the postgres
## database that will store the results use_cache: if TRUE, this uses
## the results from the iDigBio storr; if false, the entire storr is
## destroyed before
create_records_db <- function(coords, db_table) {
    idig_types <- structure(c("TEXT", "TEXT", "TEXT", "TEXT", "TEXT",
                              "TEXT", "TEXT", "TEXT", "TEXT", "TEXT",
                              "TEXT", "REAL", "REAL"),
                            .Names = c("uuid", "catalognumber",
                                  "datecollected", "institutioncode",
                                  "phylum", "class", "order", "family", "genus",
                                  "scientificname", "country",
                                  "decimallatitude", "decimallongitude"
                                  ))
    db_begin(sok_db())
    on.exit(db_rollback(sok_db(), db_table))

    if (db_has_table(sok_db(), db_table))
        db_drop_table(sok_db(), db_table)

    db_create_table(sok_db(), db_table, types = idig_types, temporary = FALSE)

    lapply(names(coords), function(q) {
        v2("Getting iDigBio records for ", q, appendLF = FALSE)
        r <- get_idigbio_by_geo(coords, q) %>%
            dplyr::rename(phylum = `data.dwc:phylum`,
                          class = `data.dwc:class`,
                          order = `data.dwc:order`,
                          family = `data.dwc:family`,
                          genus = `data.dwc:genus`,
                          decimallatitude  = geopoint.lat,
                          decimallongitude = geopoint.lon) %>%
            dplyr::mutate_if(is.character, tolower)
        v2(" DONE.")
        db_insert_into(sok_db(), db_table, r)
    })

    db_create_indexes(sok_db(), db_table, indexes = list(c("phylum", "class", "family", "scientificname"),
                                                    c("country")),
                      unique = FALSE)
    db_analyze(sok_db(), db_table)
    db_commit(sok_db())
    on.exit(NULL)
}


extract_inverts_from_db <- function(db_table, list_phyla) {

    ## We only want:
    ## - unique records
    ## - invertebrates
    ## - marine

    db <- sok_db() %>%
        dplyr::tbl(db_table)

    chr_class_to_rm <- chordata_classes_to_rm()
    chr_fam_to_rm <- chordata_families_to_rm()
    arth_class_to_rm <- arthropod_classes_to_rm()

    chordata_family_to_rm <- db %>%
        dplyr::filter(phylum == "chordata" &
                      (class %in% chr_class_to_rm |
                       family %in% chr_fam_to_rm)) %>%
        dplyr::select(phylum, class, family) %>%
        dplyr::distinct(phylum, class, family)

    arth_family_to_rm <- db %>%
        dplyr::filter(phylum == "arthropoda" & class %in% arth_class_to_rm) %>%
        dplyr::select(phylum, family) %>%
        dplyr::distinct(phylum, family) %>%
        dplyr::filter(!is.na(family))

    phyla_to_keep_clean <- list_phyla %>%
        dplyr::filter(common_phylum != "to_drop") %>%
        dplyr::distinct(common_phylum) %>%
        dplyr::pull(common_phylum)

    all_phyla_to_keep <- list_phyla %>%
        dplyr::filter(common_phylum != "to_drop",
                      !is.na(phylum)) %>%
        dplyr::distinct(phylum) %>%
        dplyr::pull(phylum)

    check_phyla_in_db(db, list_phyla)

    db %>%
        ## First let's get the phylum names we need to keep, that will take care
        ## of plants, fungi, and records with no specified phylum
        dplyr::filter(phylum %in% all_phyla_to_keep) %>%
        ## remove the obviously vertebrates
        dplyr::anti_join(chordata_family_to_rm, by = c("phylum", "family")) %>%
        dplyr::anti_join(chordata_family_to_rm, by = c("phylum", "class")) %>%
        ## and the arthropods
        dplyr::anti_join(arth_family_to_rm, by = c("phylum", "family")) %>%
        ## remove some vertebrates identified at higher level in the scientificname
        ## field
        dplyr::filter(!scientificname %in% c("chordata", "pisces", "vertebrata", "agnatha")) %>%
        dplyr::select(uuid, phylum, class, order, family, genus, scientificname,
                      decimallatitude, decimallongitude, datecollected, institutioncode,
                      dplyr::contains("depth", ignore.case = TRUE)) %>%
        dplyr::collect(n = Inf) %>%
        dplyr::distinct(uuid, .keep_all = TRUE) %>%
        dplyr::mutate(cleaned_scientificname = cleanup_species_names(scientificname),
                      is_binomial = is_binomial(cleaned_scientificname),
                      datecollected = as.Date(datecollected),
                      uuid = as.character(uuid)) %>%
        dplyr::left_join(list_phyla) %>%
        dplyr::select(-phylum,
                      phylum = common_phylum)
}

insert_map_into_db <- function(db, map) {
    ## First we create a table that holds all the layers of the shape object.
    ## This table is named like the `map` object.
    name <- deparse(substitute(map))
    sp_map <- geojsonio::geojson_sp(map)
    rpostgis::pgInsert(db, name = c("public", name),
                       data.obj = sp_map, overwrite = TRUE,
                       row.names = FALSE)
    ## Then we had the union of the layers of these objects into a master table
    ## that holds all the polygons we use: map_usa, map_gom, map_pnw.
    if (!dbExistsTable(db, "maps")) {
        dbExecute(db,
                  glue::glue("CREATE TABLE maps (",
                             "area_id TEXT PRIMARY KEY, ",
                             "geom_polygon GEOMETRY",
                             ");"))
    }
    dbExecute(db,
              glue::glue("INSERT INTO maps (area_id, geom_polygon)",
                         "VALUES ('{name}', (SELECT ST_Union({name}.geom) ",
                         "FROM {name}));"))
}

## db: database connection
## src_table: the table in the database that holds the coordinates that need to
## be filtered for geography
add_unique_coords_to_db <- function(db, src_table) {
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

    ## create temporary table with all coordinates
    ## 1. extract the unique coordinates
    dbExecute(db,
              glue::glue("CREATE TEMPORARY TABLE tmp_coords ",
                         "AS SELECT DISTINCT {src_table}.decimallatitude, {src_table}.decimallongitude ",
                         "FROM {src_table}", src_table = src_table))
    dbExecute(db, "ANALYZE tmp_coords")
    on.exit(dbExecute(db, "DISCARD TEMP;"))

    ## 2. insert them into the database
    dbExecute(db,
              glue::glue("INSERT INTO unique_coords ",
                         "SELECT * FROM tmp_coords ",
                         "LEFT JOIN unique_coords USING (decimallatitude, decimallongitude) ",
                         "WHERE within_eez IS NULL;"))
    ## 3. convert new records with MakePoint
    dbExecute(db,
              glue::glue("UPDATE unique_coords ",
                         "SET ",
                         "  geom_point = ST_SetSRID(ST_MakePoint(decimallongitude, decimallatitude), 4326) ",
                         "WHERE geom_point IS NULL;"))
    ## 4. compute whether the coordinates are in the polygon
    dbExecute(db,
              glue::glue("UPDATE unique_coords ",
                         "SET ",
                         "  within_eez = ST_Contains(map_usa, unique_coords.geom_point)  ",
                        # "  within_gom = ST_Contains(map_gom, unique_coords.geom_point), ",
                        # "  within_pnw = ST_Contains(map_pnw, unique_coords.geom_point)  ",
              "FROM (SELECT maps.geom_polygon FROM maps WHERE area_id = 'map_usa' LIMIT 1) AS map_usa ", #", ",
#              "      SELECT geom_polygon FROM maps WHERE area_id = 'map_gom' AS map_gom, ",
#              "      SELECT geom_polygon FROM maps WHERE area_id = 'map_pnw' AS map_pnw)",
              "WHERE within_eez IS NULL;"
                         ))

}

idig_stats_by_kingdom <- function(db_table, list_phyla, map) {

    db <- sok_db()
    dplyr::copy_to(db, list_phyla, name = "list_phyla",
                   overwrite = TRUE)

    idig_tbl <- db %>%
        dplyr::tbl(db_table)

    idig_phy <- db %>%
        dplyr::tbl("list_phyla")

    check_phyla_in_db(idig_tbl, list_phyla)

    ## select records within EEZ
    ## 1. convert latitude and longitude into points
    q <- c("CREATE TABLE small_idigbio AS SELECT * FROM us_idigbio LIMIT 200;",
           "ALTER TABLE small_idigbio ADD COLUMN geom_point geometry DEFAULT NULL;",
           "UPDATE small_idigbio SET geom_point = ST_SetSRID(ST_MakePoint(small_idigbio.decimallongitude, small_idigbio.decimallatitude), 4326);",
           "ALTER TABLE small_idigbio ADD COLUMN pg_within_eez bool DEFAULT NULL;",
           "UPDATE small_idigbio SET pg_within_eez = ST_Contains(one_layer.geom, small_idigbio.geom_point) FROM one_layer;")


    q <- c("CREATE TABLE uniq_coords AS SELECT DISTINCT us_idigbio.decimallongitude, us_idigbio.decimallatitude FROM us_idigbio;",
           "ALTER TABLE uniq_coords ADD COLUMN geom_point geometry DEFAULT NULL;",
           "UPDATE uniq_coords SET geom_point = ST_SetSRID(ST_MakePoint(decimallongitude, decimallatitude), 4326);",
           "ALTER TABLE uniq_coords ADD COLUMN within_eez bool DEFAULT NULL",
           "UPDATE uniq_coords SET within_eez = ST_Contains(one_layer.geom, uniq_coords.geom_point) FROM one_layer;")


    res <- purrr::map(q, function(x) {
                      r <- dbExecute(db, x)
                      if (r < 0) stop("error") else message("OK: ", x)
                      r
                  })

    ## dsn <- "PG:dbname='idigbio' host='localhost' user='marinediversity' password='password'"
    ##   ogrListLayers(dsn)
    idig_tbl %>%
        dplyr::left_join(idig_phy, by = "phylum") %>%
        dplyr::count(kingdom)


}

## db: connection to table in postgres database
## list_phyla: csv file with phylum dictionary
## Check that all phyla in the database are in the dictionary so they can be
## translated to their standardized name (for invertebrates) or assigned the
## correct kingdom
check_phyla_in_db <- function(db, list_phyla) {
   phyla_in_db <- db %>%
        dplyr::distinct(phylum) %>%
        dplyr::collect(n = Inf) %>%
        dplyr::pull(phylum)

    ## make sure all phyla are in the dictionary
    if (!all(phyla_in_db %in% list_phyla[["phylum"]]))
        stop("Some phyla in the database are not in the dictionary: ",
             paste(phyla_in_db[!phyla_in_db %in% list_phyla[["phylum"]]],
                   collapse = ", "))
}

filter_records_by_geo <- function(rec, map_area) {

    ## extract name of area based on name of variable
    area <- gsub(".+_(.+)", "\\1", deparse(substitute(map_area)))
    stopifnot(area %in% c("usa", "gom", "pnw"))

    ## reuse factory to get the correct function
    is_within_geo <- is_within_map_records(area)

    ## build the column name
    col_name <- paste0("is_in_", area)

    rec %>%
        is_within_geo(map_area) %>%
        ## only keep records within map limits
        dplyr::filter(!!sym(col_name)) %>%
        add_worms() %>%
        parse_year()
}


n_spp_from_idigbio <- function(idigbio_records) {
    idigbio_records %>%
        dplyr::filter(!is.na(is_marine),
                      is_marine == TRUE,
                      worms_valid_name != "not in worms") %>%
        mutate(phylum = tolower(`data.dwc:phylum`)) %>%
        group_by(phylum) %>%
        summarize(
            n_spp = n_distinct(worms_valid_name)
        )
}

plot_idigbio_invert_summary <- function(idigbio_records, idigbio_bold) {
    n_spp <- n_spp_from_idigbio(idigbio_records)

    n_bold <- idigbio_bold %>%
        group_by(phylum) %>%
        summarize(
            n_spp_bold = sum(n_bold_records > 0)
        )

    res <- left_join(n_spp, n_bold, by = "phylum") %>%
        mutate(p_bold = n_spp_bold/n_spp)

    ggplot(res, aes(x = reorder(phylum, p_bold), y = p_bold)) +
        geom_col() +
        coord_flip()

}

make_plot_idigbio_records_per_date <- function(idig, to_keep = c("Echinodermata", "Annelida", "Arthropoda", "Mollusca", "Porifera")) {
    idig %>%
        filter(is_marine == TRUE, !is.na(year)) %>%
        mutate(`phylum` = capitalize(`phylum`, strict = TRUE)) %>%
        filter(year >=  1900,
               `phylum` %in% to_keep) %>%
        group_by(year, institutioncode, `data.dwc:phylum`) %>%
        tally() %>%
        ggplot(aes(x = year, y = n)) +
        geom_bar(aes(fill=institutioncode), stat = "identity") +
        geom_smooth(method = "lm", formula = y ~ splines::bs(x, 6), se = FALSE) + #geom_smooth() +
        scale_y_log10() +
        facet_wrap(~ `data.dwc:phylum`) +
        scale_fill_viridis(discrete = TRUE)
}
