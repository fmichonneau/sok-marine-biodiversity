## Storr for the WoRMS ids: given a taxon name, what is its WoRMS id? ----------
fetch_hook_worms_ids <- function(key, namespace) {
    wid <- get_wormsid(searchterm = key, ask = FALSE)
    if (identical(attr(wid, "match"), "found")) {
        return(wid)
    } else if (identical(attr(wid, "match"), "not found") ||
               identical(attr(wid, "match"), "multi match")) {
        wid <- get_wormsid(searchterm = key, ask = FALSE, accepted = FALSE)
        if (identical(attr(wid, "match"), "not found")) {
            message(key, " not found.")
            return(NA)
        }
        res <- worms_records(ids = wid)$valid_AphiaID
        if (is.null(res)) {
            message("Can't find synonyms for: ", key)
            return(NA)
        }
        if (length(res) > 1) stop("More than one record for ", key)
        return(res)
    } else stop("I don't know what to do ", attr(wid, "match"))
}

store_worms_ids <- function(store_path = "data/worms_ids_storr") {
    invisible(storr_external(driver_rds(store_path),
                             fetch_hook_worms_ids))
}

## storr for the WoRMS info: given a WoRMS id, what does WoRM know about it ----
fetch_hook_worms_info <- function(key, namespace) {
    if (is.na(key)) return(NA)
    worms_records(ids = key)
}

store_worms_info <- function(store_path = "data/worms_info") {
    invisible(storr_external(driver_rds(store_path),
                             fetch_hook_worms_info))
}

## Storr for the WoRMS synonyms: given a WoRMS id, what are the synonyms? ------
fetch_hook_worms_synonyms <- function(key, namespace) {
    worms_synonyms(key)$scientificname
}

store_synonyms <- function(store_path = "data/synonym_storr") {
    invisible(storr_external(driver_rds(store_path),
                             fetch_hook_worms_synonyms))
}

## Fill the two storrs to get all the synonyms from the IRL species list
get_synonym_store <- function(irl_checklist) {
    species <- irl_checklist$`SCIENTIFIC NAME`
    lapply(species, function(x) {
        wid <- store_worms_ids()$get(x)
        store_synonyms()$get(wid)
    })
}

## Storr for the higher classification: given a WoRMS id, what is the
## higher classification
fetch_hook_worms_classification <- function(key, namespace) {
    taxizesoap:::classification_s.wormsid(key)
}

store_worms_classification <- function(store_path = "data/worms_classification_storr") {
    invisible(storr_external(driver_rds(store_path),
                             fetch_hook_worms_classification))
}

get_classification_store <- function(irl_checklist) {
    species <- irl_checklist$`SCIENTIFIC NAME`
    lapply(species, function(x) {
        wid <- store_worms_ids()$get(x)
        store_worms_classification()$get(wid)
    })
}
