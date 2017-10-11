make_grid <- function(map_geojson, cellsize = .5) {
    gom_sp <- geojsonio::geojson_sp(map_geojson)
    gom_bb <- get_bounding_box(gom_sp, cellsize = cellsize)
    bb_to_df(gom_bb)
}


internal_obis_hook <- function(wkt_coords, attempt = 0) {
    res <- try(robis::occurrence(geometry = wkt_coords),
               silent = TRUE)
    while (inherits(res, "try-error") && attempt <= 3) {
        v3("attempt ", attempt, " sleeping ...")
        Sys.sleep(exp(runif(1) * attempt))
        res <- internal_obis_hook(wkt_coords, attempt + 1)
    }
    if (!inherits(res, "try-error")) {
        if (nrow(res) > 0) {
            res
        } else {
            NULL
        }
    } else stop(res)
}


fetch_hook_obis_by_geo <- function(key, namespace) {
    coords <- unlist(strsplit(key, "\\|"))
    coords_qry <- list(xmin = as.numeric(coords[1]),
                       ymax = as.numeric(coords[2]),
                       xmax = as.numeric(coords[3]),
                       ymin = as.numeric(coords[4]))
    coords_poly <- list(
        c(coords_qry$xmin, coords_qry$ymin),
        c(coords_qry$xmin, coords_qry$ymax),
        c(coords_qry$xmax, coords_qry$ymax),
        c(coords_qry$xmax, coords_qry$ymin),
        c(coords_qry$xmin, coords_qry$ymin)
    )
    wkt_coords <- wellknown::polygon(coords_poly, fmt = 3)
    internal_obis_hook(wkt_coords, 0)
}


store_obis_by_geo <- function(coords, store_path = "data/obis_by_geo") {
    storr::storr_external(storr::driver_rds(store_path, mangle_key = TRUE),
                          fetch_hook_obis_by_geo)
}

internal_fill_store_obis_by_geo <- function(k, list_phyla) {
    res <- store_obis_by_geo()$get(k)
    phyla_to_keep <- na.omit(list_phyla$phylum[list_phyla$common_phylum != "to_drop"])
    if (nrow(res) > 0) {
        res[tolower(res$phylum) %in% phyla_to_keep &
            (!tolower(res$class) %in% chordata_classes_to_rm()), ]
    } else {
        NULL
    }
}

fill_store_obis_by_geo <- function(map_geojson, list_phyla, cellsize = .5) {
    map_grid <- make_grid(map_geojson, cellsize)

    res <- lapply(map_grid$key, internal_fill_store_obis_by_geo, list_phyla)

    dplyr::bind_rows(res) %>%
        dplyr::mutate_if(is.character, tolower) %>%
        dplyr::mutate(cleaned_scientificname = tolower(cleanup_species_names(scientificName)),
                      is_binomial = is_binomial(cleaned_scientificname))
}

obis_data_types <- function() {
    tibble::tribble(
                ~name, ~type,
                "uuid",  "INT",   ## is `id` in OBIS
                "decimalLongitude", "REAL",
                "decimalLatitude", "REAL",
                ##"lifestage", "TEXT",
                "basisOfRecord", "TEXT",
                "datecollected", "TIMESTAMP", ## is `eventDate` in OBIS
                "institutionCode", "TEXT",
                ##"collectionCode", "TEXT",
                "catalogNumber", "TEXT",
                ##"locality", "TEXT",
                ##"sex", "TEXT",
                ##"identifiedBy", "TEXT",
                ##"individualCount", "INT",
                ##"datasetName", "TEXT",
                "phylum", "TEXT",
                "order", "TEXT",
                "family", "TEXT",
                "genus", "TEXT",
                "scientificName", "TEXT",
                "originalScientificName", "TEXT",
                "scientificNameAuthorship", "TEXT",
                ##"obisID", "INT",
                ##"resourceID", "INT",
                "yearcollected", "INT",
                ##"species", "TEXT",
                "qc", "INT",
                "aphiaID", "INT",
                "speciesID", "INT",
                ##"dynamicProperties", "TEXT",
                ##"accessRights", "TEXT",
                ##"collectionID", "TEXT",
                ##"continent", "TEXT",
                ##"countryCode", "TEXT",
                ##"county", "TEXT",
                ##"fieldNumber", "TEXT",
                ##"geodeticDatum", "TEXT",
                ##"habitat", "TEXT",
                ## "higherClassification", "TEXT",
                ## "higherGeography", "TEXT",
                ## "identificationQualifier", "TEXT",
                ## "institutionID", "TEXT",
                ## "island", "TEXT",
                ## "islandGroup", "TEXT",
                ## "language", "TEXT",
                ## "modified", "TEXT",
                ## "occurrenceID", "TEXT",
                ## "occurrenceRemarks", "TEXT",
                ## "occurrenceStatus", "TEXT",
                ## "recordedBy", "TEXT",
                ## "references", "TEXT",
                ## "scientificNameID", "TEXT",
                ## "specificEpithet","TEXT",
                ## "stateProvince", "TEXT",
                ## "taxonRank","TEXT",
                ## "type","TEXT",
                ## "typeStatus", "TEXT",
                ## "vernacularName", "TEXT",
                ## "waterBody", "TEXT",
                "class", "TEXT",
                "depth", "REAL",
                "minimumDepthInMeters", "REAL",
                "maximumDepthInMeters", "REAL"
            ) %>%
        dplyr::mutate(name = tolower(name))
}


create_obis_db <- function(coords, db_table, gom_phyla) {
    obis_types <- setNames(obis_data_types()$type,
                           obis_data_types()$name)

    ## Before putting all the records in the database we need to make
    ## sure they are cached. I am not sure why it's needed here and
    ## not for iDigBio; but it seems that the data retrieval is slower
    ## for the OBIS API.
    ## EDIT: I seems it was a quirk from trying to
    ## functionalize the handling of the DB connection. Commenting it
    ## out now, but if needs to be recreated from scratch, keeping it
    ## here in case it's useful.
    ##
    ## for (q in names(coords)) {
    ##    invisible(store_obis_by_geo()$get(q))
    ## }

    ## Then we can do what we were doing with iDigBio records
    db_begin(sok_db)
    on.exit(db_rollback(sok_db, db_table))

    if (db_has_table(sok_db, db_table))
        db_drop_table(sok_db, db_table)

    db_create_table(sok_db, db_table, types = obis_types, temporary = FALSE)

    lapply(names(coords), function(q) {
        v2("Getting OBIS records for ", q,  appendLF = FALSE)
        r <- store_obis_by_geo()$get(q)
        if (!is.null(r)) {
            r <- r %>%
                dplyr::rename_all(tolower) %>%
                ## harmonize fields across databases
                dplyr::rename(uuid = id) %>% ## rename id --> uuid
                dplyr::rename_if(grepl("eventdate", names(.)),
                                 function(x) ## rename "eventdate" -> "datecollected"
                           gsub(".+", "datecollected", x)) %>%
                ## apparently some records are missing some fields, so
                ## we standardize them to their intersect
                dplyr::select(UQ(intersect(names(.),
                                           names(obis_types)))) %>%
                ## to make it comparable to iDigBio, content gets lowercased
                dplyr::mutate_if(is.character, tolower)
            db_insert_into(sok_db, db_table, r)
        }
        v2(" DONE.")
    })

    db_create_indexes(sok_db, db_table, indexes = list(c("phylum", "class", "order", "family", "scientificname"),
                                                    c("phylum"), c("class"), c("family"), c("order"),
                                                    c("scientificname")),
                      unique = FALSE)
    db_analyze(sok_db, db_table)
    db_commit(sok_db)
    on.exit(NULL)
}
