assemble_bold_dataset_raw <- function(list_taxa) {
    res <- lapply(list_taxa, function(x) {
        taxid <- bold::bold_tax_name(x)
        message("Looking for ", x)
        if (! exists("taxid", taxid))
            return(NULL)
        bold_tax_id(taxid$taxid, dataTypes = "basic,stats")
    })
    tibble::as_tibble(dplyr::bind_rows(res))
}

assemble_bold_dataset <- function(list_taxa) {
    bd <- assemble_bold_dataset_raw(list_taxa)
    dplyr::select_(bd, "taxon", "tax_rank", "stats.publicspecies",
                   "stats.publicbins", "stats.specimenrecords",
                   "stats.barcodespecimens", "stats.barcodespecies",
                   "stats.barcodespecies")
}


assemble_barcode_data <- function(worms, bold, other) {
    worms <- read.csv(file = worms, stringsAsFactors = FALSE)
    other <- read.csv(file = other, stringsAsFactors = FALSE)

    res <- worms %>%
        dplyr::select_("taxon", "accepted_species_marine") %>%
        dplyr::rename_("accepted_species" = "accepted_species_marine") %>%
        dplyr::bind_rows(other)

    bold <- assemble_bold_dataset(na.omit(res$taxon[-match("Animalia", res$taxon)]))
    bold <- filter(bold, tax_rank %in% c("phylum", "class"))
    res %>%
        dplyr::left_join(bold) %>%
        ## almost all platyhelminthes, rotifera,  and nematoda in BOLD are terrestrial
        dplyr::filter(! taxon %in% c("Arthropoda", "Chordata", "Platyhelminthes", "Nematoda", "Rotifera")) %>%
        dplyr::mutate(stats.specimenrecords = as.numeric(stats.specimenrecords),
                      stats.barcodespecimens =  as.numeric(stats.barcodespecimens),
                      stats.barcodespecies = as.numeric(stats.barcodespecies)) %>%
        dplyr::mutate(prop_bin = stats.publicbins/accepted_species,
                      prop_named = stats.publicspecies/accepted_species,
                      prop_dark = stats.barcodespecies/stats.publicspecies,
                      mean_specimens_species = stats.barcodespecimens/stats.barcodespecies,
                      mean_specimens_bins = stats.barcodespecimens/stats.publicbins
                      )
}

plot_barcoded_species <- function(div_data) {
    div_data %>%
        dplyr::select(taxon, prop_bin, prop_named, prop_dark) %>%
        tidyr::gather(prop_type, prop, -taxon) %>%
        filter(!is.na(prop)) %>%
        ggplot(.) +
        geom_bar(aes(x = reorder(taxon, prop), y = prop, fill = prop_type), stat = "identity", position = "dodge") +
        coord_flip()
}

plot_mean_individuals <- function(div_data) {
    div_data %>%
        dplyr::select(taxon, starts_with("mean")) %>%
        tidyr::gather(div_index, mean_specimens, -taxon) %>%
        ggplot(.) +
        geom_bar(aes(x = reorder(taxon, mean_specimens), y = mean_specimens, fill = div_index), stat = "identity",
                 position = "dodge") +
        coord_flip()
}


if (FALSE) {
    db <- src_sqlite("data/bold_database.sqlite")
    bld <- tbl(db, sql("SELECT phylum_reg, class_reg, species_reg, bin_guid, COUNT(*) FROM bold_table GROUP BY bin_guid "))

}
