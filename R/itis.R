fetch_itis_name <- function(key, namespace) {
    is_lower_case(key)
    message("getting info about: ", key, appendLF = FALSE)
    nm <- ritis::search_scientific(key)
    ## only keep the binomial name
    nm <- nm[is_binomial(nm$combinedName), ]
    ## actually, only keep the exact matches
    nm <- nm[grepl(paste0("^", key, "$"), nm$combinedName, ignore.case = TRUE), ]
    if (nrow(nm) == 0) {
        message(" NO MATCH...")
        return(NA_character_)
    }
    tsns <- lapply(nm$tsn, ritis::accepted_names)
    ##res <- dplyr::bind_rows(tsns)
    n_res <- sapply(tsns, nrow)
    if (all(n_res == 0)) {
        message(", result is: ", key)
        return(key)
    } else {
        if (sum(n_res == 0) == 0) {
            res <- dplyr::bind_rows(tsns)
            res <- unique(res$acceptedName)
            if (length(res) > 1)
                browser()
        } else if (sum(n_res) > 0) {
            res <- nm$combinedName[n_res == 0]
        } else
            res <- NA_character_
        if (length(res) != 1) {
            warning("More than one result: ",
                    paste(res, collapse = ", "),
                    ". Using: ", res[1])
            res <- res[1]
        }
        message(" result is: ", res)
        return(tolower(res))
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
