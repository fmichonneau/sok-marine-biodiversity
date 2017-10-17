assert_all_marine <- function(d) {
    if (!exists("is_marine", d))
        stop("columns `is_marine` missing from this dataset.")
    if (any(is.na(d$is_marine)))
        stop("Missing info about whether species is marine")
    if (!all(d$is_marine))
        stop("Non marine records are included")
}


validate_records <- function(recs, list_phyla) {

    recs_name <- deparse(substitute(recs))
    v1("Working with: ", recs_name)

    ## UUID are unique
    duplicated_uuid <- any(duplicated(recs$uuid))

    if (duplicated_uuid)
        stop("Duplicated uuid")

    ## all records are within the EEZ
    within_eez <- all(recs$is_in_eez)

    if (!within_eez)
        stop("Stop records aren't within the EEZ")

    ## only phyla from list are represented (and they are all represented)
    recs_phyla <- unique(recs$phylum)

    check_phyla <- all(recs_phyla %in% list_phyla$common_phylum[list_phyla$common_phylum != "to_drop"]) &&
        length(setdiff(recs_phyla, list_phyla$common_phylum[list_phyla$common_phylum != "to_drop"])) == 0L

    if (!check_phyla)
        stop("Some phyla aren't what they should be.")

    ## only records with non-empty worms_valid_name field
    only_accepted_worms <- sum(is.na(recs$worms_valid_name)) == 0L

    ## no records with the same identification, coordinates, date


    ## proportion of records without geographic information
    v1("Proportion of records with no goegrahpic information: ",
       mean(is.na(recs$decimallatitude) |
            is.na(recs$decimallongitude)))

    ## validate higher classification
    phylum_mismatch <- recs %>%
        dplyr::filter(phylum != worms_phylum)
    if (nrow(phylum_mismatch) > 0) {
        phylum_mismatch_file <- paste0("data-validation/",
                                       recs_name, "_phylum_mismatch.csv")
        message(nrow(phylum_mismatch), " records have phylum mismatch. ",
                "Writing to ", phylum_mismatch_file)
        readr::write_csv(phylum_mismatch, phylum_mismatch_file)
    }

    ## check there aren't any fishes
    if (any(recs$worms_class %in% chordata_classes_to_rm()) ||
        any(recs$worms_family %in% chordata_families_to_rm()))
        stop("This dataset smells fishy.")

    ## check there aren't any insects
    if (any(recs$worms_class %in% arthropod_classes_to_rm()))
        stop("There is a bug in this dataset.")

    ## records with dates outside range
    if (any(na.omit(recs$year) > 2017)  || any(na.omit(recs$year) < 1850))
        warning("Some years are outside allowed range.")

    year_is_na <- is.na(recs$year)
    message("Proportion of records with missing year data: ",
            mean(year_is_na), " (n = ", sum(year_is_na), ")")


}
