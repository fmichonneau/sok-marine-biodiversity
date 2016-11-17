taxa_to_keep <- c(
    "Porifera",
    "Octocorallia",
    "Scleractinia",
    "Actiniaria",
    "Antipatharia",
    "Cerithiantharia",
    "Medusozoa",
    "Hydroids",
    "Siphonophora",
    "Ctenophora",
    "Turbellaria",
    "Trematodes of fishes",
    "Trematodes of reptiles",
    "Trematodes of birds",
    "Trematodes of mammals",
    "Trematodes",
    "Cestodes-Table 1 (adults)",
    "Cestodes-Table 2 (larvae)",
    "Dicyemida",
    "Gnathostomulida ",
    "Rotifera",
    "Acanthocephala",
    "Nemertea",
    "Mollusca-Aplacophora",
    "Mollusca-Polyplacophora",
    "Mollusca-Gastropoda",
    "Mollusca-Cephalopoda",
    "Mollusca-Bivalvia",
    "Mollusca-Scaphopoda",
    "Polychaeta",
    "Echiura",
    "Sipuncula ",
    "Tardigrada",
    "Pycnogonida",
    "Cephalocarida-Mystacocarida",
    "Cirripedia",
    "Branchiura",
    "Copepoda",
    "Ostracoda-Myodocopida",
    "Ostracoda-Podocopa",
    "Leptostraca",
    "Stomatopoda",
    "Lophogastrida ",
    "Mysida",
    "Amphipoda",
    "Isopoda",
    "Tanaidacea ",
    "Cumacea ",
    "Euphausiacea",
    "Decapoda",
    "Gastrotricha",
    "Nematoda",
    "Priapulida",
    "Kinorhyncha",
    "Phoronida",
    "Brachiopoda ",
    "Entoprocta ",
    "Bryozoa ",
    "Chaetognatha ",
    "Hemichordata",
    "Echinodermata",
    "Cephalochordata",
    "Tunicata",
    "Appendicularia")

get_gom_species <- function(file) {
    res <- suppressMessages(readxl::read_excel(path = file))
    ## Keep only the rows that match taxa listed in `taxa_to_keep`
    res <- res[grepl(paste(gsub("\\s", "", taxa_to_keep), collapse = "|"), res$Chapter), ]
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
    res
}

add_worms_gom <- function(gom) {
    res <- gom %>%
        dplyr::filter(is_binomial == TRUE) %>%
        dplyr::select(cleaned_scientificname) %>%
        unique
    to_add <- add_worms_info(res)
    dplyr::left_join(gom, to_add, by = "cleaned_scientificname")

}
