gom_taxa_to_keep <- function() {
    c(
    "Phylum-Porifera" = "Porifera",
    "Phylum-Cnidaria" = "Octocorallia",
    "Phylum-Cnidaria" = "Scleractinia",
    "Phylum-Cnidaria" = "Actiniaria",
    "Phylum-Cnidaria" = "Antipatharia",
    "Phylum-Cnidaria" = "Cerithiantharia",
    "Phylum-Cnidaria" = "Medusozoa",
    "Phylum-Cnidaria" = "Hydroids",
    "Phylum-Cnidaria" = "Siphonophora",
    "Phylum-Ctenophora" = "Ctenophora",
    "Phylum-Platyhelminthes" = "Turbellaria",
    "Phylum-Platyhelminthes" = "Trematodes of fishes",
    "Phylum-Platyhelminthes" = "Trematodes of reptiles",
    "Phylum-Platyhelminthes" = "Trematodes of birds",
    "Phylum-Platyhelminthes" = "Trematodes of mammals",
    "Phylum-Platyhelminthes" = "Trematodes",
    "Phylum-Platyhelminthes" = "Cestodes-Table 1 (adults)",
    "Phylum-Platyhelminthes" = "Cestodes-Table 2 (larvae)",
    "Phylum-Dicymedia" = "Dicyemida",
    "Phylum-Gnathostomulida" = "Gnathostomulida",
    "Phylum-Rotifera" = "Rotifera",
    "Phylum-Acanthocephala" = "Acanthocephala",
    "Phylum-Nemertea" = "Nemertea",
    "Phylum-Mollusca" = "Mollusca-Aplacophora",
    "Phylum-Mollusca" = "Mollusca-Polyplacophora",
    "Phylum-Mollusca" = "Mollusca-Gastropoda",
    "Phylum-Mollusca" = "Mollusca-Cephalopoda",
    "Phylum-Mollusca" = "Mollusca-Bivalvia",
    "Phylum-Mollusca" = "Mollusca-Scaphopoda",
    "Phylum-Annelida" = "Polychaeta",
    "Phylum-Annelida" = "Echiura",
    "Phylum-Sipuncula" = "Sipuncula",
    "Phylum-Tardigrada" = "Tardigrada",
    "Phylum-Arthropoda" = "Pycnogonida",
    "Phylum-Arthropoda" = "Cephalocarida-Mystacocarida",
    "Phylum-Arthropoda" = "Cirripedia",
    "Phylum-Arthropoda" = "Branchiura",
    "Phylum-Arthropoda" = "Copepoda",
    "Phylum-Arthropoda" = "Ostracoda-Myodocopida",
    "Phylum-Arthropoda" = "Ostracoda-Podocopa",
    "Phylum-Arthropoda" = "Leptostraca",
    "Phylum-Arthropoda" = "Stomatopoda",
    "Phylum-Arthropoda" = "Lophogastrida",
    "Phylum-Arthropoda" = "Mysida",
    "Phylum-Arthropoda" = "Amphipoda",
    "Phylum-Arthropoda" = "Isopoda",
    "Phylum-Arthropoda" = "Tanaidacea",
    "Phylum-Arthropoda" = "Cumacea",
    "Phylum-Arthropoda" = "Euphausiacea",
    "Phylum-Arthropoda" = "Decapoda",
    "Phylum-Gastrotricha" = "Gastrotricha",
    "Phylum-Nematoda" = "Nematoda",
    "Phylum-Priapulida" = "Priapulida",
    "Phylum-Kinorhyncha" = "Kinorhyncha",
    "Phylum-Phoronida" = "Phoronida",
    "Phylum-Brachiopoda" = "Brachiopoda",
    "Phylum-Entoprocta" = "Entoprocta",
    "Phylum-Bryozoa" = "Bryozoa",
    "Phylum-Chaetognatha" = "Chaetognatha",
    "Phylum-Hemichordata" = "Hemichordata",
    "Phylum-Echinodermata" = "Echinodermata",
    "Phylum-Chordata" = "Cephalochordata",
    "Phylum-Chordata" = "Tunicata",
    "Phylum-Chordata" = "Appendicularia")
}

get_gom_species <- function(file) {
    res <- suppressMessages(readxl::read_excel(path = file))
    ## Keep only the rows that match taxa listed in `taxa_to_keep`
    res$Chapter <- gsub("\\s+$", "", res$Chapter)
    res <- res[res$Chapter %in% gom_taxa_to_keep(), ]
    ## Keep only the rows without a y in `Sort higher taxa` that indicates higher level taxa
    res <- res[is.na(res$`Sort higher taxa`), ]
    ## Extract taxon names...
    ## First remove extra spaces (a few names have additional spaces)
    res$Taxon <- gsub("\\s{2, }", " ", res$Taxon)
    ## ... and quotation marks
    res$Taxon <- gsub("\\\"|\\\'|“|”", "", res$Taxon)
    ## Second extract the first part of the Taxon name
    taxa <- gsub("([A-z]+\\s?(\\(([A-z]+|\\?)\\)\\s)?((cf\\.|(spp?\\. ex\\. gr\\.)|\\?)\\s)?[a-z]+(\\sf\\.[a-z]+)?),?\\s(.+)", "\\1", res$Taxon)
    taxa <- gsub(", species indet\\.", "", taxa)
    taxa <- gsub(" = .+$", "", taxa)
    taxa[grepl("Genus and", taxa)] <- NA_character_
    names(res)[match("Taxon", names(res))] <- "verbatim_scientificname"
    res$scientificname <- taxa
    res$cleaned_scientificname <- cleanup_species_names(taxa, rm_subgenus = TRUE)
    res$is_binomial <- is_binomial(res$cleaned_scientificname)
    res$higher <- names(gom_taxa_to_keep())[match(res$Chapter, gom_taxa_to_keep())]
    if (any(is.na(res$higher)))
        stop("Can't match some of the higher taxa")
    res <- tidyr::extract_(res, col = "higher", into = c("rank", "taxon_name"), "([[:alnum:]]+)-([[:alnum:]]+)")
    res
}

extract_species_from_gom_idigbio <- function(gom_idig, gom_wrm) {
    res <- species_list_from_idigbio(gom_idig) %>%
        dplyr::filter(!duplicated(cleaned_scientificname))
    gom <- gom_wrm %>%
        dplyr::mutate(worms_valid_name = tolower(worms_valid_name)) %>%
        dplyr::select(rank, taxon_name, worms_valid_name)
    dplyr::left_join(res, gom, by = c("cleaned_scientificname" = "worms_valid_name"))
}


standardize_gom <- function(gom_spp) {
    dplyr::select_(gom_spp,
                   "cleaned_scientificname",
                   "is_binomial",
                   "rank",
                   "taxon_name")
}

summarize_richness_per_db <- function(bold_db, idig_db, obis_db, gbif_db) {

    idig_ <- idig_db %>%
        dplyr::group_by(taxon_name) %>%
        dplyr::summarize(
            n_idigbio = sum(!is.na(n_idigbio)),
            n_idigbio_in_us = sum(!is.na(n_idigbio_in_us)),
            n_idigbio_in_gom = sum(!is.na(n_idigbio_in_gom))
        )

    obis_ <- obis_db %>%
        dplyr::group_by(taxon_name) %>%
        dplyr::summarize(
            n_obis = sum(!is.na(n_obis)),
            n_obis_in_us = sum(!is.na(n_obis_in_us)),
            n_obis_in_gom = sum(!is.na(n_obis_in_gom))
        )

    gbif_ <- gbif_db %>%
        group_by(taxon_name) %>%
        summarize(
            n_gbif = sum(!is.na(n_gbif)),
            n_gbif_in_us = sum(!is.na(n_gbif_in_us)),
            n_gbif_in_gom = sum(!is.na(n_gbif_in_gom))
        )

    bold_db %>%
        dplyr::group_by(taxon_name) %>%
        dplyr::summarize(
            n_barcoded = sum(n_bold_records > 0),
            n_total = n()
        ) %>%
        dplyr::left_join(idig_, by = "taxon_name") %>%
        dplyr::left_join(obis_, by = "taxon_name") %>%
        dplyr::left_join(gbif_, by = "taxon_name") %>%
        dplyr::filter(taxon_name %in% c("Arthropoda", "Mollusca", "Annelida",
                                        "Cnidaria", "Echinodermata", "Platyhelminthes",
                                        "Porifera", "Bryozoa", "Nematoda", "Chordata"
                                        ))

}

plot_richness_per_db <- function(smry_db, data_source) {
    smry_db %>%
        dplyr::mutate(n_idigbio_us_diff = n_idigbio - n_idigbio_in_us,
                      n_obis_us_diff = n_obis - n_obis_in_us,
                      n_gbif_us_diff = n_gbif - n_gbif_in_us,
                      n_idigbio_gom_diff = n_idigbio_in_us - n_idigbio_in_gom,
                      n_obis_gom_diff = n_obis_in_us - n_obis_in_gom,
                      n_gbif_gom_diff = n_gbif_in_us - n_gbif_in_gom
                      ) %>%
        dplyr::select(-n_idigbio, -n_obis, -n_gbif,
                      -n_idigbio_in_us, -n_obis_in_us, -n_gbif_in_us
                      ) %>%
        tidyr::gather(source_n_spp, n_spp, -taxon_name) %>%
        dplyr::mutate(source_db = gsub("(n_[a-z]+)(_.+)?", "\\1", source_n_spp)) %>% #write_csv("/tmp/nb.csv"); return(NULL)
        ggplot(aes(x = factor(source_db,
                              levels = rev(c("n_total", "n_obis", "n_gbif", "n_idigbio", "n_barcoded")),
                              labels = c("BOLD", "iDigBio", "GBIF", "OBIS", "Total")
                              ),
                   y = n_spp,
                   fill = factor(source_n_spp,
                                 levels = rev(c("n_total",
                                                "n_obis_in_gom", "n_obis_gom_diff", "n_obis_us_diff",
                                                "n_gbif_in_gom", "n_gbif_gom_diff", "n_gbif_us_diff",
                                                "n_idigbio_in_gom", "n_idigbio_gom_diff", "n_idigbio_us_diff",
                                                "n_barcoded"
                                                )),
                                 labels = rev(c("Total",
                                                "OBIS in GoM", "OBIS in US EEZ", "OBIS global",
                                                "GBIF in GoM", "GBIF in US EEZ", "GBIF global",
                                                "iDigBio in GoM", "iDigBio in US EEZ", "iDigBio global",
                                                "BOLD"
                                                ))
                                 )
                   )
               ) +
            geom_bar(stat = "identity", position = "stack") +
        facet_grid(factor(taxon_name,
                          levels = c("Arthropoda", "Mollusca", "Annelida", "Cnidaria", "Echinodermata",
                                     "Platyhelminthes", "Porifera", "Bryozoa", "Nematoda", "Chordata")) ~ .) +
        xlab("Phylum") + ylab("Number of species") +
        labs(title = "Number of species in databases", subtitle = data_source,
             caption = "(only marine taxa)") +
        scale_fill_viridis(name = "Data source", option = "inferno", discrete = TRUE) +
        coord_flip()
}
