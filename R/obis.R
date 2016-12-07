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
