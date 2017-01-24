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
    names(res) <- tolower(names(res))
    res <- dplyr::rename(res, uuid = id) %>%
        dplyr::distinct(uuid, .keep_all = TRUE) %>%
        dplyr::select_("uuid", "decimallatitude",
                       "decimallongitude",
                       "phylum", "family",
                       "genus", "species",
                       "scientificname")
    feather::write_feather(res, feather_out)
}


get_n_records_in_db <- function(worm, db, field_name) {
    ## worm: taxonomic database with WoRMS information
    ## db: data frame of the results of the query for all the taxonomic names
    ## field_name: character vector of length 1 giving the name of the
    ## column containing the number of record for a given species in
    ## the database (e.g., `n_idigibio`)

    db <- db %>%
        dplyr::mutate(scientificname = tolower(scientificname))

    summary_db <- db %>%
        dplyr::group_by(scientificname) %>%
        dplyr::tally(.) %>%
        dplyr::rename_(.dots = setNames("n", field_name))

    summary_db_in_us <- db %>%
        dplyr::filter(is_in_eez == TRUE) %>%
        dplyr::group_by(scientificname) %>%
        dplyr::tally(.) %>%
        dplyr::rename_(.dots = setNames("n", paste0(field_name, "_in_us")))

    summary_db <- dplyr::left_join(summary_db, summary_db_in_us,
                                   by = "scientificname")

    res <- worm %>%
        dplyr::filter(is_marine == TRUE) %>%
        dplyr::mutate(worms_valid_name = tolower(worms_valid_name)) %>%
        dplyr::distinct(worms_valid_name, .keep_all = TRUE) %>%
        dplyr::left_join(summary_db, by = c("worms_valid_name" = "scientificname"))

    stopifnot(all(res$is_marine))
    stopifnot(all(res$is_binomial))
    res
}
