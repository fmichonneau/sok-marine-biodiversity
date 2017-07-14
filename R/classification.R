get_classification_from_wid <- function(wid, verbose = FALSE) {
    if (is.na(wid)) {
        return(
            data_frame(phylum = NA_character_,
                       class = NA_character_,
                       order = NA_character_,
                       family = NA_character_)
        )
    }
    if (verbose) message("getting classification for aphiaid ", wid)
    classif <- store_worms_classification()$get(wid)
    empty_classif <- data_frame(phylum = character(0),
                                class  = character(0),
                                order  = character(0),
                                family = character(0))
    classif <- classif %>%
        dplyr::mutate(rank = tolower(rank),
                      scientificname = tolower(scientificname))

    to_keep <- intersect(classif$rank, names(empty_classif))

    classif %>%
        dplyr::distinct(rank, .keep_all = TRUE) %>%
        dplyr::select(-AphiaID) %>%
        tidyr::spread(rank, scientificname) %>%
        dplyr::select_(.dots = to_keep) %>%
        dplyr::bind_rows(empty_classif)
}


add_classification <- function(data) {
    stopifnot(exists("worms_id", data))
    browser()
    data %>%
        dplyr::mutate(classif = purrr::map(worms_id,
                                           get_classification_from_wid,
                                           verbose = TRUE)) %>%
        tidyr::unnest(classif)
}
