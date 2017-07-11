get_classification_from_wid <- function(wid, verbose = FALSE) {
    lapply(wid, function(i) {
        if (verbose) message("getting classification for aphiaid ", i)
        res <- store_worms_classification()$get(as.character(i))
        res <- res[[as.character(i)]]
        res %>% mutate_if(is.factor, as.character)
    })
}

unfold_classification <- function(classif, rank) {
    empty_classif <- data_frame(Phylum = character(0),
                                Class = character(0),
                                Order = character(0),
                                Family = character(0))
    cols_keep <- c("Phylum", "Class", "Order", "Family")
    to_keep <- intersect(classif$rank, cols_keep)
    dplyr::distinct(classif, rank, .keep_all = TRUE) %>%
        tidyr::spread(rank, name) %>%
            dplyr::select_(.dots = to_keep) %>%
            dplyr::bind_rows(empty_classif)
}


add_classification <- function(data) {
    stopifnot(exists("worms_id", data))
    data %>%
        dplyr::filter(!is.na(worms_id)) %>%
        dplyr::mutate(classification = get_classification_from_wid(worms_id, verbose = FALSE),
                      classification_df = purrr::map(classification, unfold_classification)) %>%
        tidyr::unnest(classification_df) %>%
        dplyr::select(-classification) %>%
        dplyr::mutate(taxon_name = Phylum, rank = "Phylum")
}
