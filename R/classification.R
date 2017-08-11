get_classification_from_wid <- function(wid) {
    if (is.na(wid) || identical(wid, "0")) {
        return(
            data_frame(worms_phylum = NA_character_,
                       worms_class = NA_character_,
                       worms_order = NA_character_,
                       worms_family = NA_character_)
        )
    }
    v2("getting classification for aphiaid ", wid)
    classif <- store_worms_classification()$get(wid)

    empty_classif <- data_frame(phylum = character(0),
                                class  = character(0),
                                order  = character(0),
                                family = character(0))
    names(empty_classif) <- paste0("worms_", names(empty_classif))

    classif <- classif %>%
        dplyr::mutate(rank = paste0("worms_", tolower(rank)),
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
    ## if there is already a 'phylum' column in "data",
    ## then the WoRMS phylum gets added as 'phylum1'
    data %>%
        dplyr::mutate(classif = purrr::map(worms_id,
                                           get_classification_from_wid)) %>%
        tidyr::unnest(classif)
}
