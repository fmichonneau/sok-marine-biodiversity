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
