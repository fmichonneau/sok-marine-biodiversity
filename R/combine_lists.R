deduplicate_records <- function(data) {
    data %>%
        dplyr::filter(!is.na(worms_valid_name),
                      is_marine == TRUE) %>%
        dplyr::select(phylum, worms_valid_name, decimallatitude,
                      decimallongitude, datecollected, year) %>%
        dplyr::distinct(phylum, worms_valid_name, decimallatitude,
                        decimallongitude, datecollected, .keep_all = TRUE)
}

combine_records <- function(..., map) {
    d <- list(...)
    d <- year_as_integer(d)
    d %>%
        dplyr::bind_rows() %>%
        deduplicate_records() %>%
        add_geo(map = map)
}

## add info on whether a record is on the East coast, West coast, or the Gulf of
## Mexico.  In this function, if a record is in the GOM, it is not on the East
## coast
add_geo <- function(d, map) {
    d <- d %>%
        is_within_gom_records(map = map) %>%
        dplyr::mutate(is_east_coast = if_else(decimallongitude > -100 &
                                             !is_in_gom, TRUE, FALSE),
                      is_west_coast = if_else(decimallongitude < -100,
                                             TRUE, FALSE))

    ## check that no record gets assigned more than one geographic area
    if (any(d$is_in_gom + d$is_east_coast + d$is_west_coast) > 3)
        stop("something's wrong!")
    d
}

combine_species_list <- function(..., map_usa) {
    d <- list(...)
    if (is.null(names(d)) && !all(nzchar(names(names(d))))) {
        stop("arguments need to be named")
    }

    d %>%
        purrr::map_df(function(dt) {
                   dt %>%
                       dplyr::distinct(worms_phylum, worms_valid_name,
                                       worms_id) %>%
                       dplyr::filter(!is.na(worms_phylum) |
                                     !is.na(worms_phylum))
               }, .id = "data_source") %>%
        dplyr::count(data_source, worms_phylum, worms_valid_name,
                     worms_id) %>%
        tidyr::spread(data_source, n) %>%
        find_bold_records(col_nm = "worms_valid_name", map_usa = map_usa)
}


export_species_list <- function(d, file) {

    d %>%
        generate_species_list() %>%
        readr::write_csv(path = file)

    file
}

generate_species_list <- function(d) {
     d %>%
        dplyr::mutate(bold = as.integer(n_bold_records > 0)) %>%
        dplyr::select(-starts_with("n_")) %>%
        dplyr::select(-starts_with("bold_within_north_")) %>%
        dplyr::mutate_if(is.integer, funs(if_else(is.na(.) | . == 0L, 0L, 1L))) %>%
        dplyr::arrange(worms_phylum)
}
