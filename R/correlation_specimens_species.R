prepare_sampling_effort_data <- function(id) {
    id %>%
        dplyr::mutate(east_west = ifelse(x > -100, "east", "west")) %>%
        dplyr::rename(latitude = y, longitude = x) %>%
        dplyr::filter(!(is.na(latitude) | is.na(longitude)))
}


plot_sampling_effort <- function(id) {
    id %>%
        prepare_sampling_effort_data() %>%
        ggplot(aes(x = n_specimen, y = n_species, shape = east_west, colour = latitude)) +
        scale_colour_viridis(option = "magma", direction = -1, end = .95, name = "Latitude") +
        geom_point(alpha = .45, position = position_jitter(width = .02, height = .02)) +
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

## proportion of identified species known by `n_specimens`.
calc_prop_nspecimens_species <- function(recs, n_specimens) {
    res <- recs %>%
        dplyr::filter(!is.na(worms_valid_name)) %>%
        dplyr::count(phylum, worms_valid_name, sort = TRUE)

    pdf(file = paste0("figures/distribution_", n_specimens, "_specimens.pdf"))
    print(
        ggplot(res, aes(n, fill = phylum)) +
        geom_bar() + xlim(c(1, 100)) + scale_y_log10()
    )
    dev.off()

    format_output(nrow(dplyr::filter(res, n <= n_specimens))/nrow(res)*100)
}

calc_prop_species_not_collected_since <- function(idig, max_year) {
    res <- idig %>%
        dplyr::filter(is_marine == TRUE, !is.na(year)) %>%
        group_by(worms_valid_name) %>%
        summarize(
            last_collected = max(year, na.rm = TRUE)
        )
    format_output(nrow(dplyr::filter(res, last_collected < max_year))/nrow(res)*100)
}

calc_records_rare_phyla <- function(idig, obis, wrms_stats, gom_phyla) {

    ## OBIS is included here, but I'm not doing anything with it at
    ## this stage

    summary_db <- . %>%
        dplyr::filter(is_marine == TRUE,
                      phylum %in% gom_phyla) %>%
        dplyr::group_by(phylum) %>%
        dplyr::summarize(
                   n_records = n(),
                   n_spp = n_distinct(worms_valid_name)
               ) %>%
        dplyr::arrange(desc(n_records))  %>%
        dplyr::mutate(is_rare = dplyr::if_else(n_records < 500, TRUE, FALSE))

    idig_res <- idig %>% summary_db
    obis_res <- obis %>% summary_db

    res <- dplyr::bind_rows(idigbio = idig_res, obis = obis_res, .id = "database")

    idig_rare_phyla <- dplyr::filter(idig_res, is_rare == TRUE) %>%
        dplyr::pull(phylum)

    idig_worms <- idig_res %>%
        dplyr::mutate(phylum = replace(phylum,
                                       phylum %in% c("kinorhyncha", "loricifera",
                                                     "nematomorpha", "priapulida"),
                                       "cephalorhyncha"),
                      phylum = replace(phylum, phylum == "echiura", "annelida")) %>%
        dplyr::group_by(phylum) %>%
        dplyr::summarize_if(is.integer, sum) %>%
        dplyr::left_join(dplyr::mutate(wrms_stats, phylum = tolower(phylum)), by = "phylum") %>%
        dplyr::mutate(is_rare = if_else(n_records < 500,  TRUE, FALSE))

    calc_p_coll <- . %>%
        dplyr::summarize(p_spp_collected = sum(n_spp)/sum(accepted_species_marine_non_fossil)) %>%
        dplyr::pull(p_spp_collected) %>%
        format_output(x = . * 100)

    list(
        ## number of phyla with less than 500 in iDigBio
        n_rare_phyla = length(idig_rare_phyla),
        ## proportion of records in iDigBio representing rare phyla
        prop_recs_rare_phyla = format_output(sum(idig_res[idig_res$is_rare, "n_records"])/sum(idig_res$n_records) * 100),
        ## proportion of the global number of species represented by rare phyla
        prop_spp_rare_phyla_collected = idig_worms %>%
            dplyr::filter(is_rare == TRUE) %>% calc_p_coll,
        ## proportion of the global number of species in iDigBio represented by common phyla
        prop_spp_common_phyla_collected = idig_worms %>%
            dplyr::filter(is_rare == FALSE) %>% calc_p_coll,
        ## global number of species for rare phyla
        total_spp_rare_phyla_collected = idig_worms %>%
            dplyr::filter(is_rare == TRUE) %>%
            dplyr::pull(accepted_species_marine_non_fossil) %>%
            sum(),
        ## global number of species for common phyla
        total_spp_common_phyla_collected = idig_worms %>%
            dplyr::filter(is_rare == FALSE) %>%
            dplyr::pull(accepted_species_marine_non_fossil) %>%
            sum()
    )
}

calc_n_idigbio_phyla <- function(idig) {
    n_distinct(idig$phylum)
}
