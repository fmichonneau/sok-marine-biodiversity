make_knowledge_through_time <- function(recs) {

    recs <- recs %>%
        dplyr::filter(!is.na(year))

    ## get total number of species per phylum
    spp_total <-  recs %>%
        dplyr::group_by(phylum) %>%
        dplyr::summarize(
            n_spp_total = n_distinct(worms_valid_name)
        )

    ## get number of samples per year and phylum and the cumulative
    ## number of samples per phylum
    n_samples <- recs %>%
        dplyr::group_by(phylum, year) %>%
        dplyr::arrange(year) %>%
        dplyr::summarize(
            n_samples = n()
        ) %>%
        dplyr::mutate(cum_n_samples = cumsum(n_samples))

    ## get number of species per phylum per year
    n_spp <- recs %>%
        dplyr::distinct(phylum, worms_valid_name, year) %>%
        dplyr::count(phylum, year) %>%
        dplyr::rename(n_spp = n)

    n_species <- recs %>%
        dplyr::group_by(phylum, worms_valid_name, year) %>%
        dplyr::arrange(year) %>%
        dplyr::tally() %>%
        dplyr::mutate(min_year = min(year)) %>%
        dplyr::select(-n) %>%
        dplyr::distinct(phylum, worms_valid_name, min_year, .keep_all = TRUE) %>%
        dplyr::group_by(phylum, min_year) %>%
        dplyr::arrange(min_year) %>%
        dplyr::summarize(
            n_new_spp = n()
        ) %>%
        dplyr::rename(year = min_year) %>%
        dplyr::right_join(n_samples, by = c("phylum", "year")) %>%
        dplyr::mutate(n_new_spp = replace(n_new_spp, is.na(n_new_spp), 0)) %>%
        dplyr::mutate(cum_n_new_spp = cumsum(n_new_spp)) %>%
        dplyr::left_join(spp_total, by = "phylum") %>%
        dplyr::left_join(n_spp, by = c("phylum", "year")) %>%
        dplyr::mutate(cum_p_new_spp = cum_n_new_spp/n_spp_total)

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

filter_phyla <- function(ktt, n_min = 220) {
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

plot_samples_vs_spp_through_time <- function(knowledge_through_time) {

    phy_to_keep <- filter_phyla(knowledge_through_time)

    res <- knowledge_through_time %>%
        dplyr::ungroup() %>%
        #dplyr::filter(phylum %in% phy_to_keep,
        #              !is.na(cum_n_new_spp)) %>%
        dplyr::group_by(year) %>%
        dplyr::summarize(n_spp = sum(n_new_spp, na.rm = TRUE),
                         n_samples = sum(n_samples, na.rm = TRUE)) %>%
        dplyr::mutate(period = cut(year, breaks = seq(1850, 2030, by = 20),
                                   labels = paste(seq(1850, 2010, by = 20),
                                                  c(seq(1870, max(year, na.rm = TRUE), by = 20),
                                                    max(year, na.rm = TRUE)),
                                                  sep = "-"), right = FALSE))
    res %>%
        ggplot(aes(x = n_samples, y = n_spp, colour = period)) +
        geom_point() +
        geom_line(data = data_frame(samples = 1:100000, species = samples),
                  aes(x = samples, y = species), color = "#2b2b2b", linetype = 2,
                  inherit.aes = FALSE) +
        geom_line(data = data_frame(samples = 1:100000, species = samples/100),
                  aes(x = samples, y = species), color = "#2b2b2b", linetype = 3,
                  inherit.aes = FALSE) +
        geom_text(data = data_frame(x = c(1.39, 139), y = c(1.5, 1.5),
                                    label = c("1:1", "1:100")),
                  aes(x = x, y = y, label = label),
                  angle = 45, hjust = 0,  vjust = 0, size = 3,
                  color = "#2b2b2b", family = "Ubuntu Condensed") +
        scale_x_log10() + scale_y_log10(limits = c(1, 600)) +
        xlab("Number of Samples") +
        ylab("Number of Species Recorded for the First Time") +
        theme_ipsum(base_family = "Ubuntu Condensed") +
        scale_color_viridis(name = "", discrete = TRUE)


}

plot_samples_vs_spp_std <- function(knowledge_through_time) {

    phy_to_keep <- filter_phyla(knowledge_through_time)

    knowledge_through_time %>%
        dplyr::ungroup() %>%
        dplyr::filter(phylum %in% phy_to_keep) %>%
        dplyr::mutate(prop_new_spp = n_new_spp/n_samples,
                      mean_new_prop = roll_meanr(prop_new_spp, n = 10, fill = 1),
                      prop_spp = roll_meanr(n_spp/n_samples, n = 10)
                      ) %>%
        ggplot(aes(x = year, y = mean_new_prop, colour = phylum)) +
        geom_point() +
        geom_point(aes(x = year, y = prop_new_spp), size = .5, alpha = .7) +
        facet_wrap(~phylum) +
        scale_colour_hc(name = "")  +
        geom_smooth(se = FALSE)

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
        dplyr::filter(!is.na(year))

    inst_to_keep <- idig %>%
        dplyr::count(institutioncode, sort = TRUE) %>%
        dplyr::filter(n  >= 1000) %>%
        dplyr::distinct()

    idig %>%
        dplyr::semi_join(inst_to_keep, by = "institutioncode") %>%
        dplyr::count(institutioncode, year) %>%
        tidyr::complete(year = 1850:2017, institutioncode,  fill = list(n = 0)) %>%
        dplyr::group_by(institutioncode) %>%
        dplyr::arrange(year) %>%
        dplyr::mutate(cum_n = cumsum(n)) %>%
        dplyr::ungroup() %>%
        ggplot(aes(x = year, y = cum_n,
                   fill = institutioncode)) +
        geom_col() + facet_wrap(~ institutioncode, scales = "free_y") +
        scale_fill_viridis(discrete = TRUE)

}

plot_identification_level_through_time <- function(idig_records) {
    idig_records %>%
        dplyr::filter(!is.na(year)) %>%
        ## count subspecies as species level identification
        dplyr::mutate(rank = replace(rank, rank == "Subspecies", "Species")) %>%
        dplyr::count(year, worms_phylum, rank) %>%
        dplyr::group_by(year, worms_phylum) %>%
        dplyr::mutate(p = n/sum(n),
               n_lots = sum(n)) %>%
        dplyr::filter(rank == "Species",
                      worms_phylum %in% c("annelida", "arthropoda", "porifera",
                                          "cnidaria", "echinodermata", "mollusca")) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(worms_phylum = capitalize(worms_phylum)) %>%
        ggplot(aes(x = year, y = p, colour = worms_phylum, fill = worms_phylum)) +
        geom_point(aes(size = n_lots)) +
        geom_hline(yintercept = 1) +
        geom_smooth(method = "lm", formula = y ~ splines::bs(x, degree = 3), show.legend = FALSE) +
        facet_wrap(~ worms_phylum) +
        guides(color = FALSE, fill = FALSE) +
        scale_size_continuous(name = "Number of specimens") +
        labs(x = "Year", y = "Proportion of specimens identified at the species level") +
        theme(legend.position = "top") +
        scale_colour_hc() + scale_fill_hc()

}
