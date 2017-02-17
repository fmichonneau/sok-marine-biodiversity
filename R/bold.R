keep_marine_taxa <- function(worms) {
    res <- worms %>%
        dplyr::filter(!is.na(is_marine),
                      is_marine == TRUE,
                      worms_valid_name != "not in worms") %>%
        dplyr::select(worms_valid_name) %>%
        unique
    res
}

## this function takes a 1-column data frame, named with the content
## of `col_nm`, and that contains the species names to look up in BOLD.
## it returns a data frame.
internal_add_bold <- function(res, col_nm) {
    bold_rcrd <- bold_bin <- numeric(nrow(res))

    for (i in seq_len(nrow(res))) {
        bold <- store_bold_specimens_per_species()$get(tolower(res[[col_nm]][i]))
        bold_rcrd[i] <- ifelse(is.null(bold) || inherits(bold, "character"),
                               0, nrow(bold))
        bold_bin[i] <- ifelse(is.null(bold) || inherits(bold, "character"),
                              0, length(unique(na.omit(bold$bin_uri))))
    }
    res <- data.frame(worms_valid_name = res,
                      n_bold_records = bold_rcrd,
                      n_bins = bold_bin,
                      stringsAsFactors = FALSE)
    res
}

add_bold <- function(worms, use_worms = TRUE) {
    if (use_worms) {
        res <- keep_marine_taxa(worms)
        col_nm <- "worms_valid_name"
    } else {
        res <- worms %>%
            dplyr::filter(is_binomial == TRUE) %>%
            dplyr::select(cleaned_scientificname) %>%
            unique
        col_nm <- "cleaned_scientificname"
    }

    res <- internal_add_bold(res, col_nm)

    wrm <- dplyr::select_(worms, "rank", "taxon_name", col_nm) %>%
        unique %>%
        dplyr::filter_(.dots = lazyeval::interp(~ !is.na(var.), var. = as.name(col_nm)))
    dplyr::left_join(res, wrm, by = col_nm)
}
