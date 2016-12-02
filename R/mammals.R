make_binomial <- function(genus, species) {
    res <- paste(ifelse(is.na(genus), "", genus),
                 ifelse(is.na(species), "", species))
    gsub("\\s+$", "", res)
}

read_idigbio_mammals <- function(file) {
    res <- feather::read_feather(file)
    res %>%
        dplyr::mutate(cleaned_scientificname =
                          make_binomial(`dwc:genus`, `dwc:specificEpithet`)) %>%
        dplyr::group_by(cleaned_scientificname) %>%
        dplyr::filter(nzchar(cleaned_scientificname)) %>%
        dplyr::summarize(
                   n_records = n(),
                   uuids = first_5(coreid)
               ) %>%
        dplyr::mutate(
                   taxon_name = "mammalia",
                   rank = "",
                   is_binomial = is_binomial(cleaned_scientificname)
               )
}
