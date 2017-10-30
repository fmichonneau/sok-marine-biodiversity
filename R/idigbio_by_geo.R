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
store_idigbio_by_geo <- function(coords, store_path = "data/storr_idigbio_by_geo") {
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

    taxa_to_rm <- db %>%
        dplyr::filter(
               (phylum == "chordata" &
                (class %in% chr_class_to_rm |
                 family %in% chr_fam_to_rm)) |
               (phylum == "arthropoda" & class %in% arth_class_to_rm)
               ) %>%
        dplyr::select(phylum, class, family) %>%
        dplyr::distinct(phylum, class, family) %>%
        dplyr::filter(!is.na(family))

    all_phyla_to_keep <- list_phyla %>%
        dplyr::filter(common_phylum != "to_drop",
                      !is.na(phylum)) %>%
        dplyr::distinct(phylum) %>%
        dplyr::pull(phylum)

    check_phyla_in_db(db, list_phyla)

    db %>%
        ## First let's keep only the within_eez records
        dplyr::filter(within_eez) %>%
        ## let's get the phylum names we need to keep, that will take care
        ## of plants, fungi, and records with no specified phylum
        dplyr::filter(phylum %in% all_phyla_to_keep) %>%
        ## remove the obviously vertebrates and terrestrial arthropods
        dplyr::anti_join(taxa_to_rm, by = c("phylum", "family")) %>%
        dplyr::anti_join(taxa_to_rm, by = c("phylum", "class")) %>%
        ## remove some vertebrates identified at higher level in the scientificname
        ## field
        dplyr::filter(!scientificname %in% c("chordata", "pisces", "vertebrata", "agnatha")) %>%
        ## select needed columns
        dplyr::select(uuid, phylum, class, order, family, genus, scientificname,
                      decimallatitude, decimallongitude, datecollected, institutioncode,
                      within_eez, within_gom, within_pnw,
                      dplyr::contains("depth", ignore.case = TRUE)) %>%
        dplyr::collect(n = Inf) %>%
        dplyr::distinct(uuid, .keep_all = TRUE) %>%
        dplyr::mutate(cleaned_scientificname = cleanup_species_names(scientificname),
                      is_binomial = is_binomial(cleaned_scientificname),
                      datecollected = as.Date(datecollected),
                      uuid = as.character(uuid)) %>%
        dplyr::left_join(list_phyla, by = "phylum") %>%
        dplyr::select(-phylum,
                      phylum = common_phylum)
}

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

    ## TODO -- the join takes a long time, it might be better to calculate directly st_contains
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
    message(glue::glue("figure out points within {maps}, ..."), appendLF = FALSE)
    contains_queries <- glue::glue(
        "UPDATE unique_coords ",
        "SET ",
        "  within_{col} = ST_Contains({maps}, unique_coords.geom_point) ",
        "FROM (SELECT geom_polygon AS {maps} FROM maps WHERE area_id = '{maps}') AS foo ",
        "WHERE within_{col} IS NULL;", maps = maps, col = c("eez", "gom", "pnw")
        )
    message(" DONE.")

    res <- purrr::map_int(contains_queries, function(x) {
        dbExecute(db, x)
        })
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

    ## Create within_* fields if they don't exist
    map(list("within_eez", "within_gom", "within_pnw"), function(x) {
        if (! x %in% dbListFields(db, db_table))
            dbExecute(db, glue::glue("ALTER TABLE {db_table} ADD COLUMN {x} BOOL DEFAULT NULL;"))
    })

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

add_worms_to_idigbio_db <- function(db_table) {

    temp_file <- tempfile(fileext = glue::glue("_{db_table}.csv"))

    sok_db() %>%
        dplyr::tbl(db_table) %>%
        dplyr::filter(within_eez) %>%
        dplyr::distinct(scientificname) %>%
        dplyr::collect(100) %>%
        dplyr::mutate(cleaned_scientificname = cleanup_species_names(scientificname),
                      is_binomial = is_binomial(cleaned_scientificname)) %>%
        dplyr::filter(is_binomial) %>%
        dplyr::distinct(cleaned_scientificname, .keep_all = TRUE) %>%
        add_worms() %>%
        write_csv(temp_file)


}

all_idigbio_species_name_cleanup <- . %>%
    ## remove non-ascii characters
    dplyr::mutate(cleaned_scientificname = iconv(scientificname, "latin1", "ASCII", sub = "")) %>%
    ## remove subsp. foo
    dplyr::mutate(cleaned_scientificname = gsub("\\ssubsp\\.? .+$", "", cleaned_scientificname)) %>%
    dplyr::mutate(cleaned_scientificname = cleanup_species_names(cleaned_scientificname)) %>%
    ## remove quotes
    dplyr::mutate(cleaned_scientificname = gsub("\"|\'", "", cleaned_scientificname)) %>%
    ## remove ex. foo bar
    dplyr::mutate(cleaned_scientificname = gsub("\\sex\\.? .+$", "", cleaned_scientificname)) %>%
    ## remove author names in botanical names
    ## in the form l. or (l.) or (c. l.)
    dplyr::mutate(cleaned_scientificname = gsub("\\s\\(?([a-z]+\\.)+\\s?([a-z]+\\.)?\\)?", "", cleaned_scientificname)) %>%
    ## remove authors with & e.g. (bartram & smith)
    dplyr::mutate(cleaned_scientificname = gsub("\\s\\(?[a-z]+\\.?\\s&\\s[a-z]+\\.?\\)?", "", cleaned_scientificname)) %>%
    ## remove synonyms e.g. solariella (=margarita) infundibulum
    dplyr::mutate(cleaned_scientificname =  gsub("\\s\\(=[a-z]+\\)", "", cleaned_scientificname))  %>%
    ## remove names that end with variations of (something)
    dplyr::mutate(cleaned_scientificname = gsub("(\\s\\(\\s?[a-z]+$)|(\\s[a-z]+\\s?\\)$)|(\\s\\(\\s?[a-z]+\\s?\\)$)", "", cleaned_scientificname)) %>%
    ## remove blend, dronen and armstrong OR dronen and armtrong
    dplyr::mutate(cleaned_scientificname = gsub("([a-z]+,\\s)?[a-z]+\\sand\\s[a-z]+", "", cleaned_scientificname)) %>%
    ## remove blend, dronen
    dplyr::mutate(cleaned_scientificname = gsub("\\s[a-z]+,\\s[a-z]+", "", cleaned_scientificname))

prepare_idig_stats_by_kingdom <- function(db_table) {

    db <- sok_db()

    ## clean up
    chordata_classes <- chordata_classes_to_rm()[-match("unknown", chordata_classes_to_rm())]
    chordata_classes <- glue("(", collapse(paste0("'", chordata_classes, "'"), sep=", "), ")")
    chordata_families <- glue("(", collapse(paste0("'", chordata_families_to_rm(), "'"), sep = ", "), ")")

    ## Drop table if it already exists
    if (db_has_table(db, glue::glue("{db_table}_clean")))
        db_drop_table(db, glue::glue("{db_table}_clean"))

    ## create tables to infer higher classification and whether species are marine
    dbExecute(db, glue::glue("CREATE TABLE {db_table}_clean AS ",
                             "SELECT DISTINCT ON (uuid) * FROM {db_table} ",
                             "WHERE within_eez IS TRUE;"))
    dbExecute(db, glue::glue("ALTER TABLE {db_table}_clean ",
                             "ADD PRIMARY KEY (uuid);"))
    dbExecute(db, glue::glue("CREATE INDEX ON {db_table}_clean (scientificname)"))
    dbExecute(db, glue::glue("UPDATE {db_table}_clean ",
                             "SET phylum = 'chordata' ",
                             "WHERE phylum IS NULL AND (",
                             "class IN {chordata_classes} OR ",
                             "family IN {chordata_families})"))

    ## get all species names, clean them up, and get worms info
    q <- dbSendQuery(db, glue::glue("SELECT DISTINCT scientificname FROM {db_table}_clean"))
    dbFetch(q) %>%
        all_idigbio_species_name_cleanup() %>%
        dplyr::distinct(cleaned_scientificname, .keep_all = TRUE) %>%
        dplyr::filter(grepl("\\s", cleaned_scientificname)) %>%
        dplyr::mutate(is_binomial = is_binomial(cleaned_scientificname)) %>%
        dplyr::group_by(cleaned_scientificname) %>%
        add_worms(remove_vertebrates = FALSE)  %>%
        dplyr::mutate(worms_kingdom = add_kingdom(worms_id)) %>%
        dplyr::ungroup() %>%
        dplyr::copy_to(db, ., name = glue::glue("{db_table}_species"), temporary = FALSE,
                       overwrite = TRUE, indexes = list("scientificname"))

    ## worms info to idigbio records
    map(list("worms_kingdom", "worms_phylum", "worms_class",
             "worms_order", "worms_family", "worms_valid_name", "worms_id"),
        function(x) dbExecute(db, glue::glue("ALTER TABLE {db_table}_clean ADD COLUMN {x} TEXT DEFAULT NULL;"))
        )
    dbExecute(db, glue::glue("ALTER TABLE {db_table}_clean ADD COLUMN is_marine BOOL DEFAULT NULL;"))
    dbExecute(db, glue::glue("UPDATE {db_table}_clean ",
                             "SET (worms_valid_name, worms_id, is_marine, worms_kingdom, worms_phylum,",
                             "     worms_class, worms_order, worms_family) = ",
                             "(SELECT worms_valid_name, worms_id, is_marine, worms_kingdom, ",
                             "        worms_phylum, worms_class, worms_order, worms_family ",
                             " FROM {db_table}_species ",
                             "  WHERE {db_table}_clean.scientificname = {db_table}_species.scientificname);"))

}

idigbio_kingdom_stats <- function(db_table) {
    db <- sok_db()

    tbl <- tbl(db, db_table)

    ## number of species and records per kingdom
    tbl %>%
        dplyr::filter(is_marine) %>%
        dplyr::filter(!is.na(worms_id)) %>%
        dplyr::mutate(sub_kingdom = case_when(
                          worms_phylum == "chordata" &
                          worms_class %in% c("appendicularia",
                                             "ascidiacea",
                                             "holocephali",
                                             "leptocardii",
                                             "thaliacea") ~ "animalia",
                          worms_phylum == "chordata" &
                          worms_class %in% c("actinopterygii",
                                             "aves",
                                             "elasmobranchii",
                                             "mammalia",
                                             "myxini",
                                             "petromyzonti",
                                             "reptilia") ~ "animalia - vertebrates",
                          TRUE ~ worms_kingdom
                      )) %>%
        dplyr::group_by(sub_kingdom, worms_phylum) %>%
        dplyr::summarize(
                   n_samples = n(),
                   n_spp = n_distinct(worms_valid_name)
               )

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

filter_records_by_geo <- function(recs, area) {

    ## extract name of area based on name of variable
    area <- match.arg(area, c("eez", "gom", "pnw"))

    ## build the column name
    col_name <- paste0("within_", area)

    recs %>%
        ## only keep records within map limits
        dplyr::filter(!!sym(col_name)) %>%
        add_worms() %>%
        parse_year()
}


n_spp_from_idigbio <- function(idigbio_records) {
    idigbio_records %>%
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
        filter(!is.na(year)) %>%
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
