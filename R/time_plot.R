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


idigbio_add_year <- function(idig) {
    idig %>%
        mutate(parsed_date = parse_date_time(datecollected, c("Y", "ymd", "ym", "%Y-%m-%d%H:%M:%S%z")),
               year = year(parsed_date)) %>%
        mutate(year = replace(year, year > 2017 | year < 1850, NA)) %>%
        filter(!is.na(year))
}

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

    res <- dplyr::bind_rows(idigbio = idig, gbif = gbif, .id = "database") %>%
        dplyr::mutate(year = replace(year, year > 2017 | year < 1800, NA)) %>%
        dplyr::filter(!is.na(year) & is_in_eez == TRUE)

    ## Combine the species lists from GOM and Kozloff
    spp_total <- dplyr::bind_rows(gom = gom_spp, koz = koz_spp, .id = "fauna") %>%
        dplyr::filter(is_marine == TRUE, is_binomial == TRUE, !is.na(worms_id)) %>%
        dplyr::group_by(fauna, taxon_name) %>%
        dplyr::tally() %>%
        dplyr::mutate(phylum = tolower(taxon_name)) %>%
        dplyr::rename(n_spp_total = n) %>%
        dplyr::select(-taxon_name)

    ## Get number of samples per phylum and year
    n_samples <- res %>%
        dplyr::group_by(database, fauna, phylum, year) %>%
        dplyr::arrange(year) %>%
        dplyr::summarize(n_samples = n()) %>%
        dplyr::mutate(cum_n_samples = cumsum(n_samples))

    ## Get number of "new" species by phylum through time
    n_species <- res %>%
        dplyr::group_by(database, fauna, phylum, scientificname, year) %>%
        dplyr::arrange(year) %>%
        dplyr::tally() %>%
        dplyr::mutate(min_year = min(year)) %>%
        dplyr::select(-n) %>%
        dplyr::distinct(database, fauna, phylum, scientificname, min_year, .keep_all = TRUE) %>%
        dplyr::group_by(database, fauna, phylum, min_year) %>%
        dplyr::arrange(min_year) %>%
        dplyr::summarize(
            n_new_spp = n()
        ) %>%
        dplyr::mutate(cum_n_new_spp = cumsum(n_new_spp)) %>%
        dplyr::rename(year =  min_year) %>%
        dplyr::left_join(spp_total, by = c("fauna", "phylum")) %>%
        dplyr::mutate(cum_p_new_spp = cum_n_new_spp/n_spp_total)

    dplyr::left_join(n_samples, n_species, by = c("database", "fauna", "phylum", "year"))

}


make_knowledge_through_time_idigbio <- function(idigbio) {

    idigbio <- idigbio %>%
        add_worms() %>%
        idigbio_add_year() %>%
        dplyr::filter(is_marine == TRUE, is_binomial == TRUE, !is.na(worms_id))


    spp_total <-  idigbio %>%
        dplyr::group_by(clean_phylum) %>%
        dplyr::summarize(
            n_spp_total = n_distinct(worms_valid_name)
        )

    n_samples <- idigbio %>%
        dplyr::group_by(clean_phylum, year) %>%
        dplyr::arrange(year) %>%
        dplyr::summarize(
            n_samples = n()
        ) %>%
        dplyr::mutate(cum_n_samples = cumsum(n_samples))

    n_species <- idigbio %>%
        dplyr::group_by(clean_phylum, worms_valid_name, year) %>%
        dplyr::arrange(year) %>%
        dplyr::tally() %>%
        dplyr::mutate(min_year = min(year)) %>%
        dplyr::select(-n) %>%
        dplyr::distinct(clean_phylum, worms_valid_name, min_year, .keep_all = TRUE) %>%
        dplyr::group_by(clean_phylum, min_year) %>%
        dplyr::arrange(min_year) %>%
        dplyr::summarize(
            n_new_spp = n()
        ) %>%
        dplyr::rename(year = min_year) %>%
        dplyr::right_join(n_samples, by = c("clean_phylum", "year")) %>%
        dplyr::mutate(n_new_spp = replace(n_new_spp, is.na(n_new_spp), 0)) %>%
        dplyr::mutate(cum_n_new_spp = cumsum(n_new_spp)) %>%
        dplyr::left_join(spp_total, by = "clean_phylum") %>%
        dplyr::mutate(cum_p_new_spp = cum_n_new_spp/n_spp_total) %>%
        dplyr::rename(phylum = clean_phylum)

    n_species


}

calc_n_spp_comparison <- function(idig_time) {
    res <- idig_time %>%
        group_by(year) %>%
        summarize(n_new_spp = sum(n_new_spp)) %>%
        mutate(cum_n_new_spp = cumsum(n_new_spp))

    list(
        n_spp_1970_1960 = res[res$year == 1970, "cum_n_new_spp"] - res[res$year == 1960, "cum_n_new_spp"],
        n_spp_2017_2000 = res[res$year == max(res$year), "cum_n_new_spp"] - res[res$year == 2000, "cum_n_new_spp"]
    )

}

filter_phyla <- function(ktt, n_min = 100) {
    ktt %>%
        group_by(phylum) %>%
        filter(cum_n_new_spp ==  max(cum_n_new_spp, na.rm = TRUE)) %>%
        filter(cum_n_new_spp > n_min) %>%
        unique(x = .$phylum)
}

plot_cum_samples_through_time <- function(knowledge_through_time, facet = TRUE)  {
    phy_to_keep <- filter_phyla(knowledge_through_time)

    res <- knowledge_through_time %>%
        filter(phylum %in% phy_to_keep) %>%
        ggplot(aes(x = year, y = cum_n_samples, colour = phylum)) +
        geom_line(size = .7) +
        xlim(c(1850, 2017)) +
        ylab("Cumulative Number of Samples") +
        xlab("Year") +
        theme_ipsum(base_family = "Ubuntu Condensed") +
        scale_colour_hc(name = "", labels = capitalize(phy_to_keep))

     if (facet)
        res <- res + facet_grid(fauna ~ database)

    res

}

plot_cum_spp_through_time <- function(knowledge_through_time, facet = TRUE) {

    phy_to_keep <- filter_phyla(knowledge_through_time)

    res <- knowledge_through_time %>%
        filter(phylum %in% phy_to_keep,
               !is.na(cum_n_new_spp)) %>%
        ggplot(aes(x = year, y = cum_n_new_spp, colour = phylum)) +
        geom_line(size = .7) +
        xlim(c(1850, 2017)) +
        ylab("Cumulative Number of Species") +
        xlab("Year") +
        theme_ipsum(base_family = "Ubuntu Condensed")  +
        scale_colour_hc()

    if (facet)
        res <- res + facet_grid(fauna ~ database)

    res

}


plot_cum_frac_through_time <- function(knowledge_through_time) {

    phy_to_keep <- filter_phyla(knowledge_through_time)

     knowledge_through_time %>%
        filter(phylum %in% phy_to_keep,
               !is.na(cum_p_new_spp)) %>%
        ggplot( aes(x = year, y = cum_p_new_spp, colour = phylum)) +
        geom_line() +
        facet_grid(fauna  ~ database)  +
        xlim(c(1850, 2017))
}




plot_new_effort <- function(knowledge_through_time) {

    phy_to_keep <- filter_phyla(knowledge_through_time)

    knowledge_through_time %>%
        filter(phylum %in% phy_to_keep,
               year > 1850) %>%
        ggplot(aes(x = n_samples, y = n_new_spp, group = phylum, colour = year,
                   shape = interaction(database, fauna))) +
        geom_point() +

        geom_abline(aes(intercept = 0, slope = 1)) +
        scale_y_log10() + scale_x_log10() +
        facet_wrap(~ phylum)
}


plot_change_trend_through_time <- function(knowledge_through_time) {

    phy_to_keep <- filter_phyla(knowledge_through_time)[-c(3, 7)]

    res <- knowledge_through_time %>%
        filter(phylum %in% phy_to_keep,
               year > 1850) %>%
        mutate(year_cat = cut(year, breaks = seq(1850, 2020, by = 20),
                              labels = paste(seq(1850, 2000, by = 20),
                                             seq(1869, 2019, by = 20),
                                             sep = "-"))) %>%
        split(list(.$year_cat, .$fauna, .$database, .$phylum)) %>%
        purrr::map( ~ lm(I(log(n_new_spp + 1, base = 10)) ~ I(log(n_samples + 1, base = 10)) - 1, data = .)) %>%
        purrr::map(summary) %>% {
            tibble(
                mdl  = names(.),
                slope = map_dbl(map(., "coefficients"), 1),
                se = map_dbl(map(., "coefficients"), 2),
                r2 = map_dbl(., "r.squared")
            )
        }

    res$mdl %>%
        strsplit("\\.") %>%
        lapply(., function(x) set_names(x, c("dates", "fauna", "database", "phylum"))) %>%
        map_df(~ as.data.frame(t(.), stringsAsFactors = FALSE)) %>%
        bind_cols(res) %>%
        select(- mdl) %>%
        ggplot(aes(x = dates, y = slope, colour = phylum)) +
        geom_point() +
        facet_grid(database ~ fauna)
}


plot_institutions_through_time <- function(idig_records) {
    idig <- idig_records %>%
        idigbio_add_year() %>%
        dplyr::filter(is_marine == TRUE, is_binomial == TRUE, !is.na(worms_id))

    inst_to_keep <- idig %>%
        dplyr::count(institutioncode, sort = TRUE) %>%
        dplyr::filter(n  >= 1000) %>%
        dplyr::distinct() %>%
        .[["institutioncode"]]

    tmp_data <- data_frame(institutioncode = inst_to_keep)

    idig %>%
        dplyr::filter(institutioncode %in% inst_to_keep) %>%
        dplyr::count(institutioncode, year) %>%
        tidyr::complete(year = 1850:2017, institutioncode,  fill = list(n = 0)) %>%
        dplyr::filter(year >= 1999) %>%
        dplyr::group_by(institutioncode) %>%
        dplyr::arrange(year) %>%
        dplyr::mutate(cum_n = cumsum(n)) %>% filter(year == 2017) %>% arrange(desc(cum_n))
        dplyr::ungroup() %>%
        ggplot(aes(x = year, y = cum_n, #color = institutioncode,
                   fill = institutioncode)) +
        geom_col() + facet_wrap(~ institutioncode, scales = "free_y")

}
