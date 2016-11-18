fetch_spp_from_idigbio <- function(wrm) {
    stopifnot(inherits(wrm, "data.frame"))
    stopifnot("worms_valid_name" %in% names(wrm))
    fields <- idigbio_fields()
    split_species <- split(na.omit(wrm$worms_valid_name),
                           ceiling(seq_along(na.omit(wrm$worms_valid_name))/150))
    res <- lapply(split_species, function(x) {
        qry <- list(scientificname = as.list(x))
        ridigbio::idig_search_records(rq = qry, fields = fields)
    })
    dplyr::bind_rows(res)
}

extract_species_from_idigbio <- function(koz_idig, koz) {
    res <- species_list_from_idigbio(koz_idig) %>%
        dplyr::filter(!duplicated(cleaned_scientificname))
    koz <- koz %>%
        dplyr::mutate(worms_valid_name = tolower(worms_valid_name)) %>%
        dplyr::select(rank, taxon_name, worms_valid_name)
    dplyr::left_join(res, koz, by = c("cleaned_scientificname" = "worms_valid_name"))
}


first_5 <- function(x) {
    n <- ifelse(length(x) > 5, 5, length(x))
    paste(x[seq_len(n)], collapse = ", ")
}

## calculate proportion of species listed in Kozloff not in iDigBio
spp_not_in_idigbio <- function(wrm, idig_qry) {
    idig_qry <- idig_qry %>%
        dplyr::group_by(scientificname) %>%
        dplyr::summarize(
                   uuid_lst = first_5(uuid),
                   n_idigbio = n()
        )

    res <- wrm %>%
        dplyr::mutate(worms_valid_name = tolower(worms_valid_name)) %>%
        dplyr::left_join(idig_qry, by = c("worms_valid_name" = "scientificname"))
    stopifnot(all(res$is_marine))
    stopifnot(all(res$is_binomial))
    res
}


calc_prop_spp_not_in_idigbio <- function(not_idig) {
    sum(is.na(not_idig$uuid_lst))/nrow(not_idig)
}
