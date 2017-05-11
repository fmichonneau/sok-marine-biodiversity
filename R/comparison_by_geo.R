compare_with_geo <- function(spp_list, geo_list) {

    res <- list(
        ## in spp_list but not in geo_list
        not_in_geo =
            geo_list %>%
            dplyr::anti_join(spp_list, by = "worms_valid_name") %>%
            dplyr::filter(is_binomial == TRUE, is_marine == TRUE) %>%
            dplyr::distinct(worms_valid_name, .keep_all = TRUE) %>%
            dplyr::select(worms_id, worms_valid_name,
                          phylum = taxon_name),
        ## in geo_list but not in spp_list
        not_in_list =
            as_data_frame(spp_list) %>%
            dplyr::anti_join(geo_list, by = "worms_valid_name") %>%
            dplyr::filter(is_binomial == TRUE, is_marine == TRUE) %>%
            dplyr::distinct(worms_valid_name, .keep_all = TRUE) %>%
            dplyr::select(worms_id, worms_valid_name,
                          phylum = taxon_name) %>%
            dplyr::mutate(phylum = tolower(phylum)),
        spp_list =
            as_data_frame(spp_list) %>%
            dplyr::distinct(worms_valid_name, .keep_all = TRUE) %>%
            dplyr::select(worms_id, worms_valid_name, phylum = taxon_name) %>%
            dplyr::mutate(phylum = tolower(phylum)),
        geo_list =
            as_data_frame(geo_list) %>%
            dplyr::distinct(worms_valid_name, .keep_all = TRUE) %>%
            dplyr::select(worms_id, worms_valid_name,
                          phylum = taxon_name)
    )

    res %>%
        dplyr::bind_rows(.id = "data_source") %>%
        dplyr::count(data_source, phylum) %>%
        dplyr::filter(phylum %in% c("porifera", "platyhelminthes",
                                    "nematoda", "mollusca", "echinodermata",
                                    "cnidaria", "chordata", "bryozoa", "arthropoda",
                                    "annelida")) %>%
        tidyr::spread(data_source, n) %>%
        dplyr::mutate(ymin_geo = -not_in_geo,
                      ymin_list = 0,
                      ymax_geo = geo_list - not_in_geo,
                      ymax_list = spp_list) %>%
        dplyr::select(phylum, starts_with("y")) %>%
        tidyr::gather(xx, n_spp, -phylum) %>%
        tidyr::separate(xx, into = c("ym", "list_orig"), sep = "_") %>%
        tidyr::spread(ym, n_spp) %>%
        ggplot(aes(x = reorder(phylum, ymax), ymin = ymin, ymax = ymax, colour = list_orig)) +
        geom_linerange(size = 5, position = position_dodge(width = .5))+
        coord_flip()
}
