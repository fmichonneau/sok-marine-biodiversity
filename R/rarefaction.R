rarefaction_data <- function(idig) {

    res <- idig %>%
        filter(is_marine == TRUE, is_binomial == TRUE, !is.na(worms_id)) %>%
        dplyr::filter(!is.na(year)) %>%
        dplyr::filter(phylum != "annelida") %>%
        dplyr::mutate(year_window = cut(year, seq(min(year, na.rm = TRUE), max(year, na.rm = TRUE), by = 10)))  %>%
        dplyr::group_by(year_window, worms_valid_name) %>%
        dplyr::tally() %>%
        dplyr::filter(!is.na(year_window)) %>%
        tidyr::spread(year_window, n)

    res <- as.data.frame(res[, -1], stringsAsFactors = FALSE, row.names = res[, 1])
    res[is.na(res)] <- 0
    res <- res[colSums(res) > 1000, ]
    res
}

rarefy_idigbio <- function(idig) {
    res <- rarefaction_data(idig)
    write.csv(res, file = "/tmp/test.csv")
    max_rare <- min(colSums(res))
    vegan::rarecurve(t(res), step = 20, sample = 500)
}
