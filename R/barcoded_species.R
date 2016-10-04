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
    bld <- tbl(db, sql("SELECT phylum_reg, class_reg, species_reg, bin_guid, COUNT(*)
                        FROM bold_table
                        GROUP BY bin_guid
                       "))
    res <- collect(bld, n = Inf)

    phylum_with_10 <- res %>%
        group_by(phylum_reg) %>%
        tally %>%
        filter(n >=  10) %>%
        .[["phylum_reg"]]

    res %>% filter(class_reg != "Insecta",  class_reg != "Collembola", class_reg != "Arachnida") %>%
        filter(count > 0, !is.na(bin_guid)) %>%
        filter(!grepl("phyta$", phylum_reg)) %>%
        filter(phylum_reg %in% phylum_with_10) %>%
        ggplot(.) +
        geom_point(aes(x=reorder(phylum_reg, count),  y=count), size = .3, position = "jitter", alpha=.3) +
        coord_flip() +
        ylim(c(0, 200))

    ## proportion of BINs with more than 10 spcms
    inverts <- res %>%
        group_by(phylum_reg) %>%
        summarize(
            n_bins = n(),
            more_than_5 = sum(count > 10)
        ) %>%
        mutate(prop_more_than_5 = more_than_5/n_bins) %>%
        select(phylum_reg, prop_more_than_5) %>%
        rename(taxon = phylum_reg)

    verts <- res %>% filter(phylum_reg == "Chordata") %>%
        group_by(class_reg) %>%
        summarize(
            n_bins = n(),
            more_than_5 = sum(count > 10)
        ) %>%
        mutate(prop_more_than_5 = more_than_5/n_bins) %>%
        select(class_reg, prop_more_than_5) %>%

        rename(taxon = class_reg)

    bind_rows(inverts = inverts, verts = verts, .id = "group") %>%
        filter(taxon != "Chordata") %>%
        filter(!is.na(taxon),
               prop_more_than_5 > 0) %>%
        ggplot() +
        geom_bar(aes(x = reorder(taxon, prop_more_than_5), y = prop_more_than_5, fill = group), stat = "identity") +
        coord_flip()

    ## taxonomic expertise: how many people are involved in the identification of each BIN?
    exprt_qry <- tbl(db, sql("SELECT phylum_reg, class_reg, bin_guid, COUNT(DISTINCT sampleid), COUNT(DISTINCT taxonomist_reg), COUNT(DISTINCT inst_reg), COUNT(DISTINCT species_reg) FROM bold_table WHERE (phylum_reg IN ('Echinodermata', 'Cnidaria')) OR (class_reg IN ('Amphibia', 'Reptilia', 'Mammalia', 'Polychaeta')) GROUP BY bin_guid" ))
    exprt <- collect(exprt_qry) %>%
        rename(num_spcm = `COUNT(DISTINCT sampleid)`,
               num_taxonomist = `COUNT(DISTINCT taxonomist_reg)`,
               num_institution = `COUNT(DISTINCT inst_reg)`,
               num_species_reg = `COUNT(DISTINCT species_reg)`)

    exprt %>%
        ggplot(.) +
        geom_point(aes(x = num_spcm, y = num_institution, colour = phylum_reg), position = "jitter") +
        xlim(c(0, 200)) + ylim(c(0, 15))

    exprt %>%
        ggplot(.) +
        geom_point(aes(x = class_reg, y = num_species_reg), position = "jitter", alpha = .1) +
        coord_flip() + ylim(c(0, 10))

    spnm_qry <- tbl(db, sql("SELECT * FROM bold_table
                             WHERE (
                                     (
                                       (phylum_reg IN ('Echinodermata', 'Cnidaria', 'Nemertea', 'Ctenophora', 'Porifera')
                                        OR class_reg IN ('Amphibia', 'Reptilia', 'Mammalia', 'Polychaeta', 'Malacostraca', 'Pycnognoida')
                                        OR order_reg IN ('Sessilia')
                                        AND order_reg NOT IN ('Thermosbaenacea', 'Spelaeogriphacea'))
                                      )
                                      AND (
                                        (country_reg IN ('United States')
                                         OR ((lon > -125 AND lat > 22) AND (lon < -65 and lat < 50))
                                        )
                                      )
                                   )"))
    spnm <- collect(spnm_qry)

   res <- spnm %>%
        filter(!is.na(species_reg), !is.na(class_reg), !is.na(bin_guid)) %>%
        group_by(phylum_reg, class_reg, bin_guid) %>%
        summarize(
            n_spp = length(unique(species_reg)),
            n_vague = sum(grepl("sp\\.|BOLD\\:", unique(species_reg)))
        ) %>%
       filter(!is.na(n_spp), !is.na(n_vague), n_spp > 0, n_vague > 0) %>%
       ungroup(.) %>%
       group_by(phylum_reg, class_reg) %>%
       summarize(
           n =  n(),
           prop_vague = sum(n_vague)/sum(n_spp)
        )

    ## All malacostraca
    mala <- bold_specimens(taxon = "Malacostraca", geo = "United States")
    mala_clean <- mala %>%
        ## remove mostly terrestrial taxa
        filter(!order_name %in% c("Isopoda", "Amphipoda")) %>%
        ## remove Hawaii/Alaska and other territories
        filter(lon > -125, lon < -50) %>%
        ## remove a few continental decapods
        filter(!((lon > -90 & lat > 35) &  (lon < -80 & lat < 45)))
    ## check on a map
    mala_clean %>%
        ggplot(.) +
        annotation_map(states, fill = "gray40", colour = "gray40") +
        geom_point(aes(x = lon, y = lat), position = "jitter") +
        coord_map(projection = "mercator")


    ## All echinoderms
    echino <- bold_specimens(taxon = "Echinodermata")

    echino_clean <- echino %>%
        filter(lon > -125, lon < -50,
               lat > 22, lat < 48)

    echino_clean %>%
        ggplot(.) +
        annotation_map(states, fill = "gray40", colour = "gray40") +
        geom_point(aes(x = lon, y = lat), position = "jitter") +
        coord_map(projection = "mercator")

    ## Polychaeta
    polyc <- bold_specimens(taxon = "Polychaeta")

    polyc_clean <- polyc %>%
        filter(lon > -125, lon < -50,
               lat > 22, lat < 48) %>%
        filter(! ((lon > -120 & lat > 35) & (lon < -100 &  lat < 40)))

    polyc_clean %>%
         ggplot(.) +
        annotation_map(states, fill = "gray40", colour = "gray40") +
        geom_point(aes(x = lon, y = lat), position = "jitter", alpha = .2) +
        coord_map(projection = "mercator")

    ## Cnidaria
    cnid <- bold_specimens(taxon = "Cnidaria")

    cnid_clean <- cnid %>%
        filter(lon > -125, lon < -50,
               lat > 22, lat < 48)

    cnid_clean %>%
         ggplot(.) +
        annotation_map(states, fill = "gray40", colour = "gray40") +
        geom_point(aes(x = lon, y = lat), position = "jitter", alpha = .2) +
        coord_map(projection = "mercator")

    ## Nemertea
    nemert <- bold_specimens(taxon = "Nemertea")

    nemert_clean <- nemert %>%
         filter(lon > -125, lon < -50,
                lat > 22, lat < 48)
    nemert_clean %>%
     ggplot(.) +
        annotation_map(states, fill = "gray40", colour = "gray40") +
        geom_point(aes(x = lon, y = lat), position = "jitter", alpha = .2, colour = "red") +
        coord_map(projection = "mercator")


    ## Mammals, Reptiles, Amphibians
    tetra <- bold_specimens(taxon = "Mammalia|Reptilia|Amphibia", geo = "United States")

    tetra_clean <-  tetra %>%
        filter(lon > -125, lon < -50)

    tetra_clean %>%
        ggplot(.) +
        annotation_map(states, fill = "gray40", colour = "gray40") +
        geom_point(aes(x = lon, y = lat, colour = class_name), position = "jitter") +
        coord_map(projection = "mercator")


    ## put it all together
    all_taxa <- bind_rows(select(tetra_clean, -image_ids),
              select(nemert_clean, -image_ids),
              select(cnid_clean, -image_ids),
              select(polyc_clean, -image_ids),
              select(echino_clean, -image_ids),
              select(mala_clean, -image_ids))

    all_taxa %>%
        filter(nchar(bin_uri) == 12L) %>%
        group_by(phylum_name, bin_uri) %>%
        summarize(
            p_unid = sum(nchar(species_name) < 2)/n(),
            n_spp = length(unique(species_name)),
            n_spcm = n()
        ) %>%
        filter(n_spp > 2)

        write_csv("/tmp/res.tmp") %>%
        ggplot(.) +
        geom_point(aes(x = phylum_name, y = n_spp), position = "jitter")


    states <- map_data("state")





    mala_clean %>%

        write_csv(., "/tmp/temp.csv") %>%
        group_by(bin_uri) %>%
        summarize(
            n_spp =  length(unique(species_name)),
            n_spcm = n()
        ) %>%
        ggplot(.) +
        geom_bar(aes(x = n_spcm))

}
