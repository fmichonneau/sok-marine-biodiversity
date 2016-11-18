add_bold <- function(worms) {
    res <- worms %>%
        dplyr::filter(!is.na(is_marine),
                      is_marine == TRUE,
                      worms_valid_name != "not in worms") %>%
        dplyr::select(worms_valid_name) %>%
        unique

    bold_rcrd <- bold_bin <- numeric(nrow(res))

    for (i in seq_len(nrow(res))) {
        bold <- store_bold_specimens_per_species()$get(tolower(res$worms_valid_name[i]))
        bold_rcrd[i] <- ifelse(is.null(bold) || inherits(bold, "character"),
                               0, nrow(bold))
        bold_bin[i] <- ifelse(is.null(bold) || inherits(bold, "character"),
                              0, length(na.omit(bold$bin_uri)))
    }
    res <- data.frame(worms_valid_name = res,
                      n_bold_records = bold_rcrd,
                      n_bins = bold_bin,
                      stringsAsFactors = FALSE)
    wrm <- dplyr::select(worms, rank, taxon_name, worms_valid_name) %>%
        unique %>%
        dplyr::filter(!is.na(worms_valid_name))
    dplyr::left_join(res, wrm, by = "worms_valid_name")
}
