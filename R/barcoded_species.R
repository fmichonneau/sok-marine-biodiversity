assemble_barcode_data <- function(worms, bold, other) {
    worms <- read.csv(file = worms, stringsAsFactors = FALSE)
    bold <- read.csv(file = bold, stringsAsFactors = FALSE)
    other <- read.csv(file = other, stringsAsFactors = FALSE)
    res <- worms %>%
        dplyr::select_("phylum", "accepted_species_marine") %>%
        dplyr::rename_("accepted_species" = "accepted_species_marine",
                       "group" = "phylum") %>%
        dplyr::bind_rows(other) %>%
        dplyr::left_join(bold) %>%
        ## almost all platyhelminthes, rotifera,  and nematoda in BOLD are terrestrial
        dplyr::filter(! group %in% c("Arthropoda", "Chordata", "Platyhelminthes", "Nematoda", "Rotifera")) %>%
        dplyr::mutate(prop_bin = number_BIN/accepted_species,
                      prop_named = number_named_species/accepted_species)
}

plot_barcoded_species <- function(div_data) {
    div_data %>%
        dplyr::select(group, prop_bin, prop_named) %>%
        gather(prop_type, prop, -group) %>%
        filter(!is.na(prop)) %>%
        ggplot(.) +
        geom_bar(aes(x = reorder(group, prop), y = prop, fill = prop_type), stat = "identity", position = "dodge") +
        coord_flip()

}
