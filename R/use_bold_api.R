
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

extract_classification <- function(df, what) {
    what <- df$rank[df$rank %in% what]
    if (length(what) < 1)
        return(NA)
    else
        return(as.character(df[df$rank == what[1], "name"]))
}

get_irl_species_in_bold <- function(store = store_bold_specimens()) {
    st <- store
    sp_nm <- st$list()
    sp_match <- lapply(sp_nm, function(x) st$get(x))
    in_bold <- vapply(sp_match, function(x) !is.null(nrow(x)), logical(1))
    classification <- lapply(sp_nm, function(x) {
        wid <- store_worms_ids()$get(x)
        if (is.na(wid)) return(c(phylum = NA, class = NA, family = NA, species = x))
        res <- store_worms_classification()$get(wid)[[1]]
        data.frame(phylum = extract_classification(res, "Phylum"),
                   class = extract_classification(res, c("Class", "Subclass", "Superclass")),
                   family = extract_classification(res, "Family"),
                   species = x,
                   stringsAsFactors = FALSE)
    })
    classification <- do.call("rbind", classification)
    res <- data.frame(species = sp_nm,
                      in_bold = in_bold)
    dplyr::left_join(res, classification, by = "species")
}
