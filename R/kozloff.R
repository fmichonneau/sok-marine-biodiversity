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
                      cleaned_valid_name = cleanup_species_names(worms_valid_name, rm_subgenus = TRUE),
                      is_binomial = is_binomial(cleaned_valid_name)) %>%
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

extract_species_from_kozloff_idigbio <- function(koz_idig, koz) {
    res <- species_list_from_idigbio(koz_idig) %>%
        filter(!duplicated(cleaned_scientificname))
    koz <- koz %>%
        mutate(worms_valid_name = tolower(worms_valid_name)) %>%
        dplyr::select(phylum, class, order, rank, taxon_name, worms_valid_name)
    left_join(res, koz, by = c("cleaned_scientificname" = "worms_valid_name"))
}

first_5 <- function(x) {
    n <- ifelse(length(x) > 5, 5, length(x))
    paste(x[seq_len(n)], collapse = ", ")
}

## calculate proportion of species listed in Kozloff not in iDigBio
data_kozloff_not_in_idigbio <- function(koz, idig_qry) {
    idig_qry <- idig_qry %>%
        group_by(scientificname) %>%
        summarize(
            uuid_lst = first_5(uuid)
        )

    res <- koz %>%
        mutate(worms_valid_name = tolower(worms_valid_name)) %>%
        left_join(idig_qry, by = c("worms_valid_name" = "scientificname"))
}


calc_prop_kozloff_not_in_idigbio <- function(koz_not_idig) {
    sum(is.na(koz_not_idig$uuid_lst))/nrow(koz_not_idig)
}

graph_kozloff_not_in_idigbio <- function(koz_not_idig) {
    koz_not_idig %>%
        group_by(phylum) %>%
        summarize(
            not_in_idigbio = sum(is.na(uuid_lst))/n(),
            n_spp_not_in_idigbio = sum(is.na(uuid_lst)),
            n_spp_total = n()
        ) %>%
        ggplot(.) +
        geom_bar(aes(x = reorder(phylum, not_in_idigbio), y = not_in_idigbio), stat = "identity") +
        geom_text(aes(x = reorder(phylum, not_in_idigbio), y = .8,
                      label = paste(n_spp_not_in_idigbio, n_spp_total, sep = "/"))) +
        coord_flip() +
        xlab("Phylum") +
        ylab("Proportion of species not in iDigBio")
}
