read_kozloff <- function(koz_raw) {
    readxl::read_excel(koz_raw, col_types = rep("text", 43)) %>%
        .[, c(1:3, 6:25)] %>%
        dplyr::select(
            counter = Counter,
            phylum = Phylum,
            class = Class,
            order = Order,
            scientific_name_verbatim = ScientificName,
            worms_valid_name = ScientificName_accepted) %>%
        dplyr::mutate(is_marine = rep(TRUE, nrow(.)),
                      rank = rep("phylum", nrow(.)),
                      taxon_name = phylum,
                      is_binomial = is_binomial(worms_valid_name)) %>%
        dplyr::filter(is_binomial == TRUE)
}


calc_kozloff_idigbio_overlap <- function(koz, idig_worms) {
    setdiff(koz$worms_valid_name, idig_worms$worms_valid_name)
}


fetch_kozloff_from_idigbio <- function(koz) {
    fields <- idigbio_fields()
    split_species <- split(koz$worms_valid_name, ceiling(seq_along(koz$worms_valid_name)/150))
    res <- lapply(split_species, function(x) {
        qry <- list(scientificname = as.list(x))
        idig_search_records(rq = qry, fields = fields)
    })
    bind_rows(res)
}

extract_species_from_kozloff_idigbio <- function(koz_idig) {
    res <- species_list_from_idigbio(koz_idig)
}
