
fetch_hook_bold_specimens <- function(key, namespace) {
    res <- try(bold_specimens(taxon = key), silent = TRUE)
    if (inherits(res, "try-error")) {
        message("no record for ", key)
        return(NA)
    }
    res
}

store_bold_specimens <- function(store_path = "data/bold_specimens") {
    invisible(storr_external(driver_rds(store_path),
                             fetch_hook_bold_specimens))
}

get_store_bold_specimens <- function(irl_checklist, store = store_bold_specimens()) {
    species <- irl_checklist$`SCIENTIFIC NAME`
    lapply(species, function(x) store$get(x))
    invisible(store)
}

get_irl_species_in_bold <- function(store = store_bold_specimens()) {
    st <- store
    sp_nm <- st$list()
    sp_match <- lapply(sp_nm, function(x) st$get(x))
    in_bold <- vapply(sp_match, function(x) !is.null(nrow(x)), logical(1))
    data.frame(species = sp_nm,
               in_bold = in_bold)
}
