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

validate_store <- function(st, allowed = NULL) {
    rendered_store <- sQuote(deparse(substitute(st)))

    keys <- st$list()
    if (length(keys) < 1) {
        v1("The store ", rendered_store, " is empty as this time.")
        return(NA)
    }
    classes <- purrr::map(keys, function(x) try(class(st$get(x)), silent = TRUE))
    ## check there are no errors
    test_broken <- purrr::map_lgl(unlist(classes), function(x) inherits("try-error", x))
    if (any(test_broken)) {
        broken_keys <- keys[test_broken]
        stop("errors detected in store: ", rendered_store, " for keys: ",
             paste(broken_key, collapse = ", "))
    }
    test_error <- grepl("error", unlist(classes), ignore.case = TRUE)
    if (any(test_error)) {
        error_keys <- keys[test_error]
        stop("some keys seem broken in store: ", rendered_store, " for keys: ",
             paste(error_key, collapse = ", "))
    }
    if (!is.null(allowed)) {
        test_allowed <- all(unlist(classes) %in% allowed)
        if (!test_allowed) {
            stop("some keys are of different class than allowed values in store: ",
                 rendered_store, " for keys: ",
                 paste(keys[!test_allowed], collapse = ", "))
        }
    }
    TRUE
}

## function to get all classes from an existing storr
get_classes <- function(st) unique(unlist(map(st$list(), function(x) class(st$get(x)))))

validate_stores <- function(store_path = "data/") {
    all_stores <- list.files(path = store_path, pattern = "stor")
    ## TODO: use all_stores to compare with data frame below to issue warning if
    ## some stores are not being covered by the integrity tests. I'll need to
    ## take into account that some stores are too big, and this test would eat
    ## up all the memory, so adjustments might be needed.
    tbl_df_classes <- c("tbl_df", "tbl", "data.frame")
    ## data frame that contains: the name of the store function (`store_name`),
    ## and the allowable classes for the objects contained in the store.
    store_details <-
        tibble::tribble(
           ~ store_name, ~ allowed, ~use_arg,
           "store_itis_geo", tbl_df_classes, FALSE,
           "store_itis_comments", tbl_df_classes, FALSE,
           "store_itis_classification", tbl_df_classes, FALSE,
           "store_idigbio_by_geo", "data.frame", TRUE,
           "store_idigbio_species_occurrences", "data.frame", FALSE,
           "store_worms_classification", tbl_df_classes, FALSE,
           "store_worms_ids", c("character", tbl_df_classes), FALSE,
           ##"store_idigbio_uuid", , FALSE, ## too big
           ##"store_obis_by_geo", , FALSE, ## to big
           ## "store_red_list" currently not used
           ## "gbif_occ_storr" currently not used
           "store_obis_occurrences", c("NULL", "data.frame"), FALSE,
           "store_worms_info", c("list", "data.frame"), FALSE,
           "store_synonyms", c("logical", "character") , FALSE,
           "store_bold_specimens_per_species", c("NULL", "character", "data.frame"), FALSE
           )
    purrr::pmap(store_details, function(store_name, allowed, use_arg) {
               st <- get(store_name)
               if (use_arg)
                   validate_store(st(NULL), allowed)
               else validate_store(st(), allowed)
           })
}
