## key the OBIS occurrences from the AphiaID (worms_id)
fetch_hook_obis_occurrences <- function(key, namespace) {
    if (!is.na(key))
        res <- robis::occurrence(aphiaid = key)
    else return(NULL)
    if (nrow(res) == 0)
        return(NULL)
    else
        res
}


store_obis_occurrences <- function(store_path = "data/obis_occurrences_storr") {
    invisible(storr_external(driver_rds(store_path),
              fetch_hook_obis_occurrences))
}

fetch_spp_from_obis <- function(wrm, feather_out) {
    stopifnot(inherits(wrm, "data.frame"))
    stopifnot("worms_id" %in% names(wrm))
    res <- lapply(wrm$worms_id, function(x)
        store_obis_occurrences()$get(x))
    res <- dplyr::bind_rows(res)
    feather::write_feather(res, feather_out)
}

spp_not_in_obis <- function(wrm, obis_feather) {
    obis_qry <- feather::read_feather(obis_feather) %>%
        dplyr::mutate(scientificName = tolower(scientificName)) %>%
        dplyr::group_by(scientificName) %>%
        dplyr::summarize(
                   uuid_lst = first_5(id),
                   n_obis = n()
        )
    res <- wrm %>%
        dplyr::filter(is_marine == TRUE) %>%
        dplyr::mutate(worms_valid_name = tolower(worms_valid_name)) %>%
        dplyr::left_join(obis_qry, by = c("worms_valid_name" = "scientificName"))
    stopifnot(all(res$is_marine))
    stopifnot(all(res$is_binomial))
    res
}

get_n_records_in_db <- function(worm, db, field_name) {
    ## worm: taxonomic database with WoRMS information
    ## db: data frame of the results of the query for all the taxonomic names
    ## field_name: character vector of length 1 giving the name of the
    ## column containing the number of record for a given species in
    ## the database (e.g., `n_idigibio`)

    if (inherits(db, "character")) {
        db <- feather::read_feather(db)
        names(db) <- tolower(names(db))
    }

    summary_db <- db %>%
        dplyr::mutate(scientificname = tolower(scientificname)) %>%
        dplyr::group_by(scientificname) %>%
        dplyr::tally(.) %>%
        dplyr::rename_(.dots = setNames("n", field_name))

    res <- worm %>%
        dplyr::filter(is_marine == TRUE) %>%
        dplyr::mutate(worms_valid_name = tolower(worms_valid_name)) %>%
        dplyr::left_join(summary_db, by = c("worms_valid_name" = "scientificname"))

    stopifnot(all(res$is_marine))
    stopifnot(all(res$is_binomial))
    res
}
