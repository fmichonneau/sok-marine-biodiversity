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
