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
            n_idigbio_in_us = sum(!is.na(n_idigbio_in_us))
        )

    obis_ <- obis_db %>%
        dplyr::group_by(taxon_name) %>%
        dplyr::summarize(
            n_obis = sum(!is.na(n_obis)),
            n_obis_in_us = sum(!is.na(n_obis_in_us))
        )

    gbif_ <- gbif_db %>%
        group_by(taxon_name) %>%
        summarize(
            n_gbif = sum(!is.na(n_gbif)),
            n_gbif_in_us = sum(!is.na(n_gbif_in_us))
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

plot_richness_per_db <- function(smry_db) {
    smry_db %>%
        tidyr::gather(source_n_spp, n_spp, -taxon_name) %>%
        ggplot(aes(x = reorder(taxon_name, n_spp), y = n_spp, fill = source_n_spp)) +
        geom_bar(stat = "identity", position = "dodge") +
        xlab("Phylum") + ylab("Number of species") +
        labs(title = "Number of species in databases", subtitle = "Gulf Of Mexico",
             caption = "(only marine taxa)") +
        coord_flip()
}


## plot_proportion_per_db <- function(smry_db) {
##     # TODO
##     smry_

## }

#gom_not_in_idigbio <- function(gom)

##
if (FALSE) {



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


## test caching
if (FALSE) {
    ## first let's figure out a reasonable resolution
    eez_shp <- rgdal::readOGR(dsn = "data-raw/USA-EEZ", "eez")
    eez_df <- ggplot2::fortify(eez_shp, region = "geoname")

    pt_grid <- expand.grid(
        x = seq(-130, -70, by = .1),
        y = seq(22, 48, by = .1)
    )
    ggplot() +
        geom_map(data = eez_df, map = eez_df, aes(x = long, y = lat, map_id = id)) +
        geom_point(data = pt_grid, aes(x = x, y = y), color = "red", size = .2) +
        coord_quickmap(xlim = c(-84, -80), ylim = c(23, 27))

    ## let's use 1000 random points
    rnd <- gom_idigbio %>%
        sample_n(50) %>%
        mutate(
            lat = round(decimallatitude, 1),
            long = round(decimallongitude, 1)
        )

    ## map_usa_ <- remake::fetch("map_usa")


    lawn_direct <- function() {
        res <- is_in_eez(rnd, map_usa_)
        rnd$is_in_eez <- rnd$uuid %in% res$features$properties$uuid
        rnd
    }

    lawn_cached <- function() {
        rnd <- rnd %>%
            rowwise() %>%
            mutate(
                is_in_eez = eez_coords_store$get(paste(lat, long, sep = "|"))
            )
        rnd
    }

    microbenchmark::microbenchmark(
                        lawn_direct(),
                        lawn_cached(),
                        n = 10
                    )


}
