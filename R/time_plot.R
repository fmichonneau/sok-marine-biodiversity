make_knowledge_through_time <- function(idig_data_gom, idig_data_koz) {
    res <- bind_rows(gom = idig_data_gom,
                     koz = idig_data_koz, .id = "source")  %>%
        mutate(parsed_date = parse_date_time(datecollected, c("Y", "ymd", "ym", "%Y-%m-%d%H:%M:%S%z"))) %>%
        mutate(year = year(parsed_date)) %>%
        mutate(year = replace(year, year > 2017 | year < 1800, NA)) %>%
        filter(!is.na(year))

    ## Get number of samples per phylum and year
    n_samples <- res %>%
        group_by(source, phylum, year) %>%
        arrange(year) %>%
        summarize(n_samples = n()) %>%
        mutate(cum_n_samples = cumsum(n_samples))

    ## Get number of "new" species by phylum through time
    n_species <- res %>%
        group_by(source, phylum, scientificname, year) %>%
        arrange(year) %>%
        tally() %>%
        mutate(min_year = min(year)) %>%
        select(-n) %>%
        distinct(source, phylum, scientificname, min_year, .keep_all = TRUE) %>%
        group_by(source, phylum, min_year) %>%
        arrange(min_year) %>%
        summarize(
            n_spp = n()
        ) %>%
        mutate(cum_n_spp = cumsum(n_spp)) %>%
        rename(year =  min_year)

    left_join(n_samples, n_species, by = c("source", "phylum", "year"))



}

plot_cum_samples_through_time <- function(knowledge_through_time)  {
    knowledge_through_time %>%
    ggplot(aes(x = year, y = cum_n_samples, colour = phylum)) +
        geom_line() +
        facet_grid(~ source)
}

plot_cum_spp_through_time <- function(knowledge_through_time) {

    phy_to_keep <- knowledge_through_time %>%
        group_by(phylum) %>%
        filter(cum_n_spp ==  max(cum_n_spp, na.rm = TRUE)) %>%
        filter(cum_n_spp > 100) %>%
        unique(x = .$phylum)

    knowledge_through_time %>%
        filter(phylum %in% phy_to_keep,
               !is.na(cum_n_spp)) %>%
        ggplot( aes(x = year, y = cum_n_spp, colour = phylum)) +
        geom_line() +
        facet_grid(~ source)

}
