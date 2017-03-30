get_n_records_in_db <- function(worm, db, field_name, region = c("gom", "pnw")) {
    ## worm: taxonomic database with WoRMS information
    ## db: data frame of the results of the query for all the taxonomic names
    ## field_name: character vector of length 1 giving the name of the
    ## column containing the number of record for a given species in
    ## the database (e.g., `n_idigibio`)
    ## region: either "gom" for Gulf of Mexico, or "pnw" for Pacific North West.

    region <- match.arg(region)
    region <- paste0("_in_", region)

    db <- db %>%
        dplyr::mutate(scientificname = tolower(scientificname))

    summary_db <- db %>%
        dplyr::group_by(scientificname) %>%
        dplyr::tally(.) %>%
        dplyr::rename_(.dots = setNames("n", field_name))

    summary_db_in_us <- db %>%
        dplyr::filter(is_in_eez == TRUE) %>%
        dplyr::group_by(scientificname) %>%
        dplyr::tally(.) %>%
        dplyr::rename_(.dots = setNames("n", paste0(field_name, "_in_us")))

    summary_db_in_region <- db %>%
        dplyr::filter_(lazyeval::interp("x == TRUE", x = as.name(paste0("is", region)))) %>%
        dplyr::group_by(scientificname) %>%
        dplyr::tally(.) %>%
        dplyr::rename_(.dots = setNames("n", paste0(field_name, region)))

    summary_db <- dplyr::left_join(summary_db, summary_db_in_us,
                                   by = "scientificname")

    summary_db <- dplyr::left_join(summary_db, summary_db_in_region,
                                   by = "scientificname")

    res <- worm %>%
        dplyr::filter(is_marine == TRUE) %>%
        dplyr::mutate(worms_valid_name = tolower(worms_valid_name)) %>%
        dplyr::distinct(worms_valid_name, .keep_all = TRUE) %>%
        dplyr::left_join(summary_db, by = c("worms_valid_name" = "scientificname"))

    stopifnot(all(res$is_marine))
    stopifnot(all(res$is_binomial))
    res
}
