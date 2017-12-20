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

get_coords_idigbio_query <- function(map_eez, cellsize = .5) {
    bb_eez <- generate_bounding_boxes(map_eez, cellsize = cellsize)
    coords_to_query(bb_eez)
}

## for all the coordinates of the bounding boxes, find the iDigBio
## records they contain coords: output of get_coords_idigbio_query
## map_eez: map in GeoJSON db_table: name of table in the postgres
## database that will store the results use_cache: if TRUE, this uses
## the results from the iDigBio storr; if false, the entire storr is
## destroyed before
create_records_db <- function(coords, db_table) {
    idig_types <- structure(c("TEXT", "TEXT", "TEXT", "TEXT", "TEXT",
                              "TEXT", "TEXT", "TEXT", "TEXT", "TEXT",
                              "TEXT", "REAL", "REAL", "TEXT", "TEXT",
                              "BOOLEAN", "BOOLEAN", "BOOLEAN"),
                            .Names = c("uuid", "catalognumber",
                                  "datecollected", "institutioncode",
                                  "phylum", "class", "order", "family", "genus",
                                  "scientificname", "country",
                                  "decimallatitude", "decimallongitude",
                                  "data.dwc:fieldNumber", "data.dwc:recordedBy",
                                  "within_eez", "within_gom", "within_pnw"
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

    dbExecute(sok_db(), glue::glue("DELETE FROM {db_table} a USING (
      SELECT MIN(ctid) as ctid, uuid
        FROM {db_table}
        GROUP BY uuid HAVING COUNT(*) > 1
      ) dups
      WHERE a.uuid = dups.uuid
      AND a.ctid <> dups.ctid"))
    db_create_indexes(sok_db(), db_table, indexes = list(c("phylum", "class", "family", "scientificname"),
                                                         c("country"), c("uuid"),
                                                         c("decimallatitude", "decimallongitude")),
                      unique = FALSE)
    dbExecute(sok_db(), glue::glue("ALTER TABLE {db_table} ADD PRIMARY KEY (uuid);"))
    db_analyze(sok_db(), db_table)
    db_commit(sok_db())
    add_within_polygon_to_db(db_table)
    on.exit(NULL)
}


add_worms_to_db <- function(db_table) {
    db <- sok_db()

    ## Drop table if it already exists
    if (db_has_table(db, glue::glue("{db_table}_worms")))
        db_drop_table(db, glue::glue("{db_table}_worms"))

    v3(glue::glue("Creating {db_table}_worms ... "), appendLF = FALSE)
    dbExecute(db, glue::glue("CREATE TABLE {db_table}_worms AS ",
                             "SELECT * FROM {db_table} ",
                             "WHERE (within_eez IS TRUE OR ",
                             "  within_pnw IS TRUE OR within_gom IS TRUE);"))
    dbExecute(db, glue::glue("ALTER TABLE {db_table}_worms ",
                             "ADD PRIMARY KEY (uuid);"))
    v3("DONE.")

    v3(glue::glue("Fetching WoRMS info for {db_table}_worms ... "), appendLF = FALSE)
     ## get all species names, clean them up, and get worms info
    if (grepl("idigbio", db_table)) {
        dbExecute(db, glue::glue("CREATE INDEX ON {db_table}_worms (scientificname)"))
        wrm_res <- dbSendQuery(db, glue::glue("SELECT DISTINCT scientificname FROM {db_table}_worms;")) %>%
            dbFetch() %>%
            all_idigbio_species_name_cleanup() %>%
            dplyr::mutate(cleaned_scientificname = cleanup_species_names(cleaned_scientificname)) %>%
            dplyr::distinct(cleaned_scientificname, .keep_all = TRUE) %>%
            add_worms(remove_vertebrates = FALSE)
        join_q <- glue::glue("WHERE {db_table}_worms.scientificname = {db_table}_species.scientificname")
        idx <- list("cleaned_scientificname")
        types <- structure(c("TEXT", "TEXT", "TEXT",
                             "TEXT", "BOOLEAN", "TEXT", "BOOLEAN", "TEXT",
                             "TEXT", "TEXT", "TEXT", "TEXT", "TEXT"),
                           .Names = c("scientificname", "cleaned_scientificname", "worms_id",
                             "worms_valid_name", "is_fuzzy", "rank", "is_marine",
                             "worms_phylum", "worms_class", "worms_order",
                             "worms_family", "phylum", "worms_kingdom")
                           )
    } else if (grepl("obis", db_table)){
        dbExecute(db, glue::glue("ALTER TABLE {db_table}_worms ALTER aphiaid TYPE TEXT;"))
        dbExecute(db, glue::glue("CREATE INDEX ON {db_table}_worms (aphiaid)"))
        wrm_res <-  dbSendQuery(db, glue::glue("SELECT DISTINCT aphiaid FROM {db_table}_worms;")) %>%
            dbFetch() %>%
            dplyr::filter(!is.na(aphiaid)) %>%
            add_worms_by_id(remove_vertebrates = FALSE)
        join_q <- glue::glue("WHERE {db_table}_worms.aphiaid = {db_table}_species.worms_id")
        idx <- list("worms_id")
        types <- structure(c("TEXT", "TEXT", "BOOLEAN",
                             "TEXT", "TEXT", "TEXT",  "TEXT",
                             "TEXT", "TEXT", "TEXT", "TEXT", "TEXT"),
                           .Names = c("aphiaid", "worms_id", "is_marine",
                             "worms_valid_name", "rank", "is_fuzzy",
                             "worms_phylum", "worms_class", "worms_order",
                             "worms_family", "phylum", "worms_kingdom")
                           )
    } else stop(glue::glue("invalid table name: {db_table}."))

    wrm_res %>%
        dplyr::mutate(worms_kingdom = add_kingdom(worms_id)) %>%
        dplyr::copy_to(db, ., name = glue::glue("{db_table}_species"), temporary = FALSE,
                       overwrite = TRUE, indexes = idx, types = types)
    dbExecute(db, glue::glue("DELETE FROM {db_table}_species a USING (
      SELECT MIN(ctid) as ctid, worms_id
        FROM {db_table}_species
        GROUP BY worms_id HAVING COUNT(*) > 1
      ) dups
      WHERE a.worms_id = dups.worms_id
      AND a.ctid <> dups.ctid"))
    v3("DONE.")

    v3(glue::glue("Adding WoRMS info to {db_table}_worms ... "), appendLF = FALSE)
    ## add worms info to records
    purrr::map(list("worms_kingdom", "worms_phylum", "worms_class",
             "worms_order", "worms_family", "worms_valid_name", "worms_id", "rank"),
        function(x) dbExecute(db, glue::glue("ALTER TABLE {db_table}_worms ADD COLUMN {x} TEXT DEFAULT NULL;"))
        )

    dbExecute(db, glue::glue("ALTER TABLE {db_table}_worms ADD COLUMN is_marine BOOL DEFAULT NULL;"))
    dbExecute(db, glue::glue("UPDATE {db_table}_worms ",
                             "SET (worms_valid_name, worms_id, is_marine, rank, worms_kingdom, worms_phylum,",
                             "     worms_class, worms_order, worms_family) = ",
                             "(SELECT worms_valid_name, worms_id, is_marine, rank, worms_kingdom, ",
                             "        worms_phylum, worms_class, worms_order, worms_family ",
                             " FROM {db_table}_species ",
                             " {join_q} );"))
    db_analyze(db, glue::glue("{db_table}_worms"))
    v3("DONE.")
}

filter_by_geo <- function(tbl, geo) {

    geo <- match.arg(geo, c("within_eez", "within_gom", "within_pnw"))
    geo <- rlang::sym(geo)

    tbl %>%
        ## First let's keep only the geographic zone of interest
        dplyr::filter(!!geo)
}


extract_inverts_from_db <- function(db_table, geo) {

    db <- sok_db()

    add_worms_to_db(db_table)

    db_worms <- db %>% tbl(glue::glue("{db_table}_worms"))

    db_worms %>%
        filter_by_geo(geo) %>%
        add_sub_kingdom() %>%
        dplyr::filter(sub_kingdom == "animalia - invertebrates",
                      is_marine) %>%
        ## select needed columns
        dplyr::select(uuid, phylum, class, order, family, genus, scientificname,
                      decimallatitude, decimallongitude, datecollected, institutioncode,
                      within_eez, within_gom, within_pnw,
                      worms_phylum, worms_class, worms_order, worms_family,
                      worms_valid_name, worms_id, is_marine, rank,
                      dplyr::contains("depth", ignore.case = TRUE)) %>%
        dplyr::collect(n = Inf) %>%
        dplyr::mutate(datecollected = as.Date(datecollected),
                      uuid = as.character(uuid)) %>%
        dplyr::filter(!is.na(worms_phylum)) %>%
        parse_year()
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


get_idigbio_mms_records <- function(list_phyla) {
    db <- sok_db()
    res <- dbSendQuery(db, glue::glue(
                                     "SELECT * FROM us_idigbio_worms ",
                                     "WHERE \"data.dwc:recordedBy\" ~* '(\\y(mms|blm)\\y)' ",
                                     "AND within_eez IS TRUE")
                       ) %>%
        dbFetch()

    arth_class_to_rm <- tibble::tibble(
                                    worms_phylum = "arthropoda",
                                    worms_class = arthropod_classes_to_rm())


    res %>%
        add_sub_kingdom() %>%
        dplyr::filter(sub_kingdom == "animalia - invertebrates",
                      is_marine) %>%
        ## remove ambiguous arthropods
        dplyr::anti_join(arth_class_to_rm, by = c("worms_phylum", "worms_class")) %>%
        dplyr::left_join(list_phyla, by = "phylum") %>%
        dplyr::filter(!is.na(worms_phylum)) %>%
        parse_year()

}

summary_idigbio_mms_by_phylum <- function(idig_mms) {
    idig_mms %>%
        dplyr::count(phylum, sort = TRUE)
}

summary_idigbio_mms_by_year <- function(idig_mms) {
    idig_mms %>%
        dplyr::count(year)
}


mms_samples_through_time <- function(idig_mms) {
    idigbio_mms_records %>%
        dplyr::filter(!is.na(year)) %>%
        dplyr::count(worms_phylum, year) %>%
        dplyr::group_by(year) %>%
        dplyr::arrange(dplyr::desc(n), .by_group = TRUE) %>%
        dplyr::top_n(5, n) %>%
        dplyr::ungroup() %>%
        ggplot() + geom_col(aes(x = year, y = n, fill = worms_phylum))
}

list_species_only_collected_by_mms <- function(idig_mms, idig_recs) {
    ## remove the blm/mms records from full iDigBio list
    idig_res_no_mms <- dplyr::anti_join(idig_recs, idig_mms, by = "uuid")

    ## extract species for blm/mms set
    idig_mms_spp <- idig_mms %>%
        dplyr::filter(rank == "Species" |
                      rank == "Subspecies") %>%
        dplyr::distinct(worms_phylum, worms_valid_name)

    ## extract species for iDigBio list that don't contain blm/mms records
    idig_no_mms_spp <- dplyr::distinct(idig_res_no_mms, worms_phylum, worms_valid_name)

    ## remove species from
    dplyr::anti_join(idig_mms_spp, idig_no_mms_spp,
                     by = c("worms_phylum", "worms_valid_name")) %>%
        tibble::as_tibble()
}
