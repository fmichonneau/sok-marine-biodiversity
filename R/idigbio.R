idigbio_fields <- function() {
    c('uuid',
      'catalognumber',
      'datecollected',
      'institutioncode',
      ## 'phylum', # no need for phylum, full of garbage
      'data.dwc:phylum',
      'data.dwc:class',
      'data.dwc:order',
      'data.dwc:family',
      'data.dwc:genus',
      'scientificname',
      'datecollected',
      'country',
      'geopoint')
}

store_idigbio_species_occurrences <- function(store_path = "data/idigbio_occurrences_storr") {
    invisible(storr_external(driver_rds(store_path),
                             fetch_hook_idigbio_by_sp))
}

fetch_hook_idigbio_by_sp <- function(key, namespace) {
    stopifnot(identical(key, tolower(key)))
    spp <- union(key, cleanup_species_names(key, rm_subgenus = TRUE))

    message("Getting iDigBio records for ", paste(spp, collapse = ", "))

    qry <- list(scientificname = as.list(spp),
                basisofrecord = "PreservedSpecimen"
                )
    r <- try(ridigbio::idig_search_records(rq = qry, fields = idigbio_fields()),
             silent = TRUE)

    if (inherits(r, "try-error")) stop("something is wrong")

    r <- r %>%
        dplyr::rename(decimallatitude = geopoint.lat,
                      decimallongitude = geopoint.lon)
    names(r) <- gsub("^.+:", "", names(r))
    r
}


fetch_spp_from_idigbio <- function(wrm) {
    stopifnot(inherits(wrm, "data.frame"))
    stopifnot("worms_valid_name" %in% names(wrm))
    wrm %>%
        dplyr::filter(!is.na(worms_valid_name)) %>%
        dplyr::pull(worms_valid_name) %>%
        purrr::map_df(function(worms_valid_name)
                   store_idigbio_species_occurrences()$get(tolower(worms_valid_name))
                   )
}


summarize_raw_idigbio <- function(idig, list_spp) {
    assertthat::assert_that(inherits(idig, "data.frame"))
    assertthat::assert_that(exists("country", idig))
    assertthat::assert_that(exists("species_name", idig))
    assertthat::assert_that(exists("species_name", list_spp))
    assertthat::assert_that(exists("order", list_spp))
    assertthat::assert_that(exists("family", list_spp))

    idig <- idig %>%
        filter(country == "united states") %>%
        group_by(species_name) %>%
        tally() %>%
        rename(n_idigbio = n)

    list_spp %>%
        mutate(scientificname = tolower(species_name)) %>%
        select(order, family, species_name) %>%
        left_join(idig, by = "species_name")

}

calc_prop_spp_not_in_idigbio <- function(not_idig) {
    sum(is.na(not_idig$uuid_lst))/nrow(not_idig)
}

plot_spp_not_in_idigbio <- function(koz_not_idig) {
    koz_not_idig %>%
        group_by(phylum) %>%
        summarize(
            not_in_idigbio = sum(is.na(n_idigbio))/n(),
            n_spp_not_in_idigbio = sum(is.na(n_idigbio)),
            n_spp_total = n()
        ) %>%
        ggplot(.) +
        geom_bar(aes(x = reorder(phylum, not_in_idigbio), y = not_in_idigbio), stat = "identity") +
        geom_text(aes(x = reorder(phylum, not_in_idigbio), y = .9,
                      label = paste(n_spp_not_in_idigbio, n_spp_total, sep = "/"))) +
        coord_flip() +
        xlab("Phylum") +
        ylab("Proportion of species not in iDigBio")
}

plot_spp_not_in_idigbio_combined <- function(koz_not_idig, gom_not_idig) {

    res <- dplyr::bind_rows("kozloff" = koz_not_idig,
                     "gom" = gom_not_idig,
                     .id = "list") %>%
        group_by(list, phylum) %>%
        summarize(
            p_not_in_idigbio = sum(is.na(n_idigbio))/n(),
            p_not_in_idigbio_us = sum(is.na(n_idigbio_in_us))/n(),
            n_spp_not_in_idigbio = sum(is.na(n_idigbio)),
            n_spp_not_in_idigio_us = sum(is.na(n_idigbio_in_us)),
            n_spp_total = n(),
            n_spp_total_us = n()
        )

    abd_spp <- res %>%
        group_by(phylum) %>%
        summarize(n = sum(n_spp_total)) %>%
        filter(n > 50)

    res %>%
        gather(idig_status, n, -list, -phylum) %>%
        mutate(n_p = if_else(grepl("^p", idig_status), "p",
                             ifelse(grepl("total", idig_status), "N", "n")),
               in_us = ifelse(grepl("_us$", idig_status), "US", "World")) %>%
        select(-idig_status) %>%
        spread(n_p, n) %>%
        filter(phylum %in% abd_spp$phylum,
               phylum != "Nematoda") %>%
        ggplot(aes(x = reorder(phylum, p), y = p)) +
        geom_bar(aes(fill = list), stat = "identity", position = "dodge") +
        geom_text(aes(y = .005, label = paste(n, N, sep = "/")),
                  position = position_dodge(.6),
                  hjust = 0, vjust = rep(c(2, -1), 20)) +
        coord_flip() +
        xlab("Phylum") +
    ylab("Proportion of species not in iDigBio") +
        facet_wrap(~ in_us)

}
