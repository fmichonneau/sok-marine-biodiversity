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

if (FALSE) {

   ## koz_idig <- koz_idigbio_species_list %>%

    koz_summ <- kozloff_bold %>%
        group_by(taxon_name) %>%
        summarize(
            p_barcoded = sum(n_bold_records > 0)/n(),
            n_barcoded = sum(n_bold_records > 0),
            n_total = n()
        ) %>%
        gather(n, n_brcded, -taxon_name, -p_barcoded)  %>%
        dplyr::select(- p_barcoded)


    gom_idig <- gom_spp_not_in_idigbio %>%
        group_by(taxon_name) %>%
        summarize(
            n_idigbio = sum(!is.na(n_idigbio))
        )

    gom_obis <- gom_spp_not_in_obis %>%
        group_by(taxon_name) %>%
        summarize(
            n_obis = sum(!is.na(n_obis))
        )

    gom_bold %>%
        group_by(taxon_name) %>%
        summarize(
            #p_barcoded = sum(n_bold_records > 0)/n(),
            n_barcoded = sum(n_bold_records > 0),
            n_total = n()
        ) %>%
        left_join(gom_idig, by = "taxon_name") %>%
        left_join(gom_obis, by = "taxon_name") %>%
        gather(counts, n_cat, -taxon_name)  %>%
        #bind_rows("gom" = ., "koz" = koz_summ, .id = "source") %>%
        ggplot(aes(x = reorder(taxon_name, n_cat), y = n_cat, fill = counts)) +
        geom_bar(stat = "identity", position = "dodge") +
        #facet_grid(~ source) +
        coord_flip()


    n_spp_gom <- length(unique(gom_worms$worms_valid_name))
    n_spp_gom_bold <- sum(gom_bold$n_bold_records > 0)
    n_spp_gom_idigbio <- sum(!is.na(gom_spp_not_in_idigbio$n_idigbio))
    n_spp_gom_bold_idigbio <- sum(gom_bold$worms_valid_name[gom_bold$n_bold_records > 0] %in%
                                  gom_spp_not_in_idigbio$worms_valid_name[!is.na(gom_spp_not_in_idigbio$n_idigbio)])
    fit1 <- eulerr::eulerr(c("GOM" = n_spp_gom,
                             "BOLD" = n_spp_gom_bold,
                             "idigbio" = n_spp_gom_idigbio,
                             "GOM&BOLD" = n_spp_gom_bold,
                             "GOM&idigbio" = n_spp_gom_idigbio,
                             "GOM&idigbio&BOLD" = n_spp_gom_bold_idigbio))
    plot(fit1)

}
