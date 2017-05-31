prepare_sampling_effort_data <- function(id) {
      id %>%
        mutate(east_west = ifelse(x > -100, "east", "west")) %>%
        rename(latitude = y, longitude = x)
}


plot_sampling_effort <- function(id) {
    id %>%
        prepare_sampling_effort_data %>%
        ggplot(aes(x = n_specimen, y = n_species, shape = east_west, colour = latitude)) +
        scale_colour_viridis(option = "magma", direction = -1, end = .95) +
        geom_point(alpha = .6, position = position_jitter(width = .01, height = .01)) +
        geom_abline(slope = 1, intercept = 0) +
        xlab("Number of records") + ylab("Number of species") +
        scale_x_log10() + scale_y_log10() +
        scale_shape(name = "", labels = c("East coast", "West coast")) +
        theme_ipsum(base_family = "Ubuntu Condensed")

}

model_sampling_effort <- function(id) {
    id <- prepare_sampling_effort_data(id)
    mdl <- glm(log(n_species) ~ log(n_specimen) * latitude * east_west - log(n_specimen):latitude:east_west, data = id)

}

calc_prop_singleton_species <- function(idig) {
    res <- idig %>%
        group_by(worms_valid_name) %>%
        tally

    format_output(nrow(dplyr::filter(res, n == 1))/nrow(res)*100)
}

calc_prop_species_not_collected_since <- function(idig, max_year) {
    res <- idig %>%
        dplyr::filter(is_marine == TRUE) %>%
        idigbio_parse_year() %>%
        group_by(worms_valid_name) %>%
        summarize(
            last_collected = max(year, na.rm = TRUE)
        )
    format_output(nrow(dplyr::filter(res, last_collected < max_year))/nrow(res)*100)
}
