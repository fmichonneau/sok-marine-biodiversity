fetch_itis_name <- function(key, namespace) {
    is_lower_case(key)
    message("getting info about: ", key, appendLF = FALSE)
    nm <- ritis::search_scientific(key)
    message(" ... DONE")
    nm <- nm[is_binomial(nm$combinedName), ]
    tsns <- lapply(nm$tsn, ritis::accepted_names)
    res <- dplyr::bind_rows(tsns)
    if (nrow(res) == 0) {
        return(key)
    } else {
        res <- unique(res$acceptedName)
        if (length(res) > 1)
            browser()
        return(res)
    }
}

store_itis_name <- function(path = "data/itis_name_storr") {
    invisible(
        storr::storr_external(
                  storr::driver_rds(path),
                  fetch_itis_name
              )
    )
}
