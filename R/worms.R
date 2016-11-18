## Storr for the WoRMS ids: given a taxon name, what is its WoRMS id? ----------
fetch_hook_worms_ids <- function(key, namespace) {
    if (!identical(tolower(key), key))
        stop("key must be lowercase: ", key)
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
        if (identical(res, "0"))
            return(NA)
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


rescue_store <- function(store) {
    all_content <- store$list()
    is_missing <- vapply(all_content,
                         function(x) {
        res <- try(store$get(x), silent = TRUE)
        inherits(res, "try-error")
    }, logical(1))
    message(sum(is_missing), " records need to be rescued...")

    to_redo <- all_content[is_missing]
    sapply(to_redo, function(x) store$del(x))
    sapply(to_redo, function(x) store$get(x))
}


add_worms <- function(sp_list) {
    stopifnot(inherits(sp_list, "data.frame"))
    stopifnot(all(c("is_binomial", "cleaned_scientificname") %in%
                  names(sp_list)))

    spp <- sp_list %>%
        dplyr::filter(is_binomial == TRUE) %>%
        dplyr::select(cleaned_scientificname) %>%
        unique

    wid <- valid_name <- vector("character", nrow(spp))
    marine <- vector("logical", nrow(spp))

    for (i in seq_len(nrow(spp))) {
        wid[i] <- store_worms_ids()$get(tolower(spp[i, 1]))
        if (!is.na(wid[i]) && !identical(wid[i], "0")) {
            w_info <- store_worms_info()$get(wid[i])
            if (nrow(w_info) > 1) browser()
            marine[i] <- (identical(w_info$isMarine, "1") | identical(w_info$isBrackish, "1"))
            valid_name[i] <- w_info$valid_name
        } else {
            marine[i] <- NA
            valid_name[i] <- NA
        }
    }
    to_add <- data.frame(
        cleaned_scientificname = spp,
        worms_id = wid,
        is_marine = marine,
        worms_valid_name = valid_name,
        stringsAsFactors = FALSE
    )
    dplyr::left_join(sp_list, to_add, by = "cleaned_scientificname")
}
