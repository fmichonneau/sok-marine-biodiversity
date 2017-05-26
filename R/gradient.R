plot_gradient <- function(idig) {

    idig %>%
        dplyr::mutate(coast = if_else(decimallongitude > -100, "east", "west")) %>%
        dplyr::mutate(
                   lat_grad = ntile(decimallatitude, 100)
               ) %>%
        dplyr::group_by(coast, clean_phylum, lat_grad) %>%
        dplyr::summarize(n_spp = n_distinct(cleaned_scientificname),
                         n_obs = n()) %>%
        dplyr::filter(clean_phylum %in% c("arthropoda", "mollusca", "annelida", "echinodermata", "cnidaria")) %>%
        dplyr::mutate(p_samp = n_spp/n_obs) %>%
        ggplot(aes(x = lat_grad, y = n_spp, color = clean_phylum, shape = coast, size = n_obs)) +
        geom_point() + facet_grid(clean_phylum ~ coast) +
        geom_smooth(aes(weight = n_obs))

}
