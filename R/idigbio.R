fetch_spp_from_idigbio <- function(wrm) {
    stopifnot(inherits(wrm, "data.frame"))
    stopifnot("worms_valid_name" %in% names(wrm))
    fields <- idigbio_fields()
    split_species <- split_by_n(na.omit(wrm$worms_valid_name), 150)
    res <- lapply(split_species, function(x) {
        qry <- list(scientificname = as.list(x))
        ridigbio::idig_search_records(rq = qry, fields = fields)
    })
    res <- dplyr::bind_rows(res) %>%
        dplyr::distinct(uuid, .keep_all = TRUE) %>%
        dplyr::rename(decimallatitude = geopoint.lat,
                      decimallongitude = geopoint.lon)
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

calc_prop_spp_not_in_idigbio <- function(not_idig) {
    sum(is.na(not_idig$uuid_lst))/nrow(not_idig)
}

plot_spp_not_in_idigbio <- function(koz_not_idig) {
    koz_not_idig %>%
        group_by(taxon_name) %>%
        summarize(
            not_in_idigbio = sum(is.na(n_idigbio))/n(),
            n_spp_not_in_idigbio = sum(is.na(n_idigbio)),
            n_spp_total = n()
        ) %>%
        ggplot(.) +
        geom_bar(aes(x = reorder(taxon_name, not_in_idigbio), y = not_in_idigbio), stat = "identity") +
        geom_text(aes(x = reorder(taxon_name, not_in_idigbio), y = .9,
                      label = paste(n_spp_not_in_idigbio, n_spp_total, sep = "/"))) +
        coord_flip() +
        xlab("Phylum") +
        ylab("Proportion of species not in iDigBio")
}

plot_spp_not_in_idigbio_combined <- function(koz_not_idig, gom_not_idig) {

    res <- dplyr::bind_rows("kozloff" = koz_not_idig,
                     "gom" = gom_not_idig,
                     .id = "list") %>%
        group_by(list, taxon_name) %>%
        summarize(
            not_in_idigbio = sum(is.na(uuid_lst))/n(),
            n_spp_not_in_idigbio = sum(is.na(uuid_lst)),
            n_spp_total = n()
        )

    abd_spp <- res %>%
        group_by(taxon_name) %>%
        summarize(n = sum(n_spp_total)) %>%
        filter(n > 50)


    res %>%
        filter(taxon_name %in% abd_spp$taxon_name,
               taxon_name != "Nematoda") %>%
        ggplot(aes(x = reorder(taxon_name, not_in_idigbio), y = not_in_idigbio)) +
        geom_bar(aes(fill = list), stat = "identity", position = "dodge") +
        geom_text(aes(y = .005, label = paste(n_spp_not_in_idigbio, n_spp_total, sep = "/")),
                  position = position_dodge(.6),
                  hjust = 0, vjust = rep(c(2, -1), 10)) +
        coord_flip() +
        xlab("Phylum") +
        ylab("Proportion of species not in iDigBio")
}
