preprocess_gbif <- function(gom_gbif, koz_gbif) {
    bind_rows(gom = gom_gbif, koz = koz_gbif, .id = "fauna") %>%
        select(fauna, taxon_name, taxonrank, scientificname, year, is_in_eez) %>%
        filter(taxonrank == "SPECIES") %>%
        mutate(phylum = tolower(taxon_name)) %>%
        select(-taxonrank, -taxon_name)
}

## no date info in OBIS?
## preprocess_obis <- function(gom_obis, koz_obis) {
##     res <- bind_rows(gom = gom_obis, koz = koz_obis, .id = "fauna")
##     res
## }


preprocess_idigbio <- function(gom_idig, koz_idig) {
    res <- bind_rows(gom = gom_idig, koz = koz_idig, .id = "fauna")  %>%
        mutate(parsed_date = parse_date_time(datecollected, c("Y", "ymd", "ym", "%Y-%m-%d%H:%M:%S%z"))) %>%
        mutate(year = year(parsed_date)) %>%
        select(scientificname, is_in_eez, year, `data.dwc:phylum`, fauna) %>%
        rename(phylum = `data.dwc:phylum`) %>%
        mutate(phylum = tolower(phylum))
}

make_knowledge_through_time <- function(gom_idig, koz_idig, gom_gbif, koz_gbif,
                                        gom_spp, koz_spp) {

    ## Combine the data from the public databases (GBIF and iDigBio)
    idig <- preprocess_idigbio(gom_idig, koz_idig)
    gbif <- preprocess_gbif(gom_gbif, koz_gbif)

    res <- bind_rows(idigbio = idig, gbif = gbif, .id = "database") %>%
        mutate(year = replace(year, year > 2017 | year < 1800, NA)) %>%
        filter(!is.na(year) & is_in_eez == TRUE)

    ## Combine the species lists from GOM and Kozloff
    spp_total <- bind_rows(gom = gom_spp, koz = koz_spp, .id = "fauna") %>%
        filter(is_marine == TRUE, is_binomial == TRUE, !is.na(worms_id)) %>%
        group_by(fauna, taxon_name) %>%
        tally() %>%
        mutate(phylum = tolower(taxon_name)) %>%
        rename(n_spp_total = n) %>%
        select(-taxon_name)

    ## Get number of samples per phylum and year
    n_samples <- res %>%
        group_by(database, fauna, phylum, year) %>%
        arrange(year) %>%
        summarize(n_samples = n()) %>%
        mutate(cum_n_samples = cumsum(n_samples))

    ## Get number of "new" species by phylum through time
    n_species <- res %>%
        group_by(database, fauna, phylum, scientificname, year) %>%
        arrange(year) %>%
        tally() %>%
        mutate(min_year = min(year)) %>%
        select(-n) %>%
        distinct(database, fauna, phylum, scientificname, min_year, .keep_all = TRUE) %>%
        group_by(database, fauna, phylum, min_year) %>%
        arrange(min_year) %>%
        summarize(
            n_new_spp = n()
        ) %>%
        mutate(cum_n_new_spp = cumsum(n_new_spp)) %>%
        rename(year =  min_year) %>%
        left_join(spp_total, by = c("fauna", "phylum")) %>%
        mutate(cum_p_new_spp = cum_n_new_spp/n_spp_total)

    left_join(n_samples, n_species, by = c("database", "fauna", "phylum", "year"))



}

filter_phyla <- function(ktt) {
    ktt %>%
        group_by(phylum) %>%
        filter(cum_n_new_spp ==  max(cum_n_new_spp, na.rm = TRUE)) %>%
        filter(cum_n_new_spp > 100) %>%
        unique(x = .$phylum)
}


plot_cum_samples_through_time <- function(knowledge_through_time)  {
    phy_to_keep <- filter_phyla(knowledge_through_time)

    knowledge_through_time %>%
        filter(phylum %in% phy_to_keep) %>%
        ggplot(aes(x = year, y = cum_n_samples, colour = phylum)) +
        geom_line() +
        facet_grid(fauna ~ database)  +
        xlim(c(1850, 2017))
}

plot_cum_spp_through_time <- function(knowledge_through_time) {

    phy_to_keep <- filter_phyla(knowledge_through_time)

    knowledge_through_time %>%
        filter(phylum %in% phy_to_keep,
               !is.na(cum_n_new_spp)) %>%
        ggplot( aes(x = year, y = cum_n_new_spp, colour = phylum)) +
        geom_line() +
        facet_grid(fauna ~ database)  +
        xlim(c(1850, 2017))

}

plot_new_effort <- function(knowledge_through_time) {

    phy_to_keep <- filter_phyla(knowledge_through_time)

    knowledge_through_time %>%
        filter(phylum %in% phy_to_keep) %>%
        ggplot(aes(x = n_samples, y = n_new_spp, group = phylum, colour = year,
                   shape = interaction(database, fauna))) +
        geom_point() +

        geom_abline(aes(intercept = 0, slope = 1)) +
        scale_y_log10() + scale_x_log10() +
        facet_wrap(~ phylum)
}

plot_new_effort_proportion <- function(knowledge_through_time) {

    phy_to_keep <- filter_phyla(knowledge_through_time)

     knowledge_through_time %>%
        filter(phylum %in% phy_to_keep,
               !is.na(cum_p_new_spp)) %>%
        ggplot( aes(x = year, y = cum_p_new_spp, colour = phylum)) +
        geom_line() +
        facet_grid(fauna  ~ database)  +
        xlim(c(1850, 2017))
}
