compare_with_geo <- function(spp_list, geo_list) {

    list(
        ## in spp_list but not in geo_list
        not_in_geo =
            as_data_frame(geo_list) %>%
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
    ) %>% dplyr::bind_rows(.id = "data_source")
}

generate_upsetr_csv <- function(..., file) {

    has_bold_record <- function(worms_sp) {
        r <- store_bold_specimens_per_species()$get(tolower(worms_sp))
        if (is.null(r) || inherits(r, "character"))
            0L
        else nrow(r)
    }

    d <- list(...)
    d <- lapply(d, function(x) {
        if (exists("taxon_name", x))
            rename_(x, "phylum" = "taxon_name")
    })

    dplyr::bind_rows(d, .id = "database") %>%
        dplyr::mutate(phylum = tolower(phylum)) %>%
        dplyr::count(phylum, worms_valid_name, database) %>%
        tidyr::spread(database, n) %>%
        dplyr::filter(!is.na(worms_valid_name)) %>%
        dplyr::mutate(bold = has_bold_record(worms_valid_name)) %>%
        dplyr::mutate_if(is.integer, funs(if_else(is.na(.), 0L, 1L))) %>%
        readr::write_csv(path = file)

}


if (FALSE) {

    upset(xx, order.by="freq")


}
