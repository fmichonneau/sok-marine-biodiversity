prepare_sampling_effort_data <- function(id) {
      id %>%
        mutate(east_west = ifelse(x > -100, "east", "west")) %>%
        rename(latitude = y, longitude = x)
}


plot_sampling_effort <- function(id) {
    id %>%
        prepare_sampling_effort_data %>%
        ggplot(aes(x = n_specimen, y = n_species, shape = east_west, colour = latitude)) +
        geom_point(position = "jitter") +
        geom_abline(slope = 1, intercept = 0) +
        xlab("Number of records") + ylab("Number of species") +
        scale_x_log10() + scale_y_log10() +
        theme_bw()

}

model_sampling_effort <- function(id) {
    id <- prepare_sampling_effort_data(id)
    mdl <- glm(log(n_species) ~ log(n_specimen) * latitude * east_west - log(n_specimen):latitude:east_west, data = id)

}

calc_prop_singleton_species <- function(idig_dates) {
    res <- idig_dates %>%
        group_by(scientificname) %>%
        tally

    format_output(nrow(dplyr::filter(res, n == 1))/nrow(res)*100)
}

calc_prop_species_not_collected_since <- function(idig_dates, date) {
    res <- idig_dates %>%
        group_by(scientificname) %>%
        summarize(
            last_collected = max(year, na.rm = TRUE)
        )
    format_output(nrow(dplyr::filter(res, last_collected< date))/nrow(res)*100)
}
