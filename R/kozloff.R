get_kozloff_species <- function(koz_raw) {
    readr::read_csv(koz_raw) %>%
        dplyr::mutate(cleaned_scientificname = cleanup_species_names(scientificname_verbatim, rm_subgenus = TRUE),
                      is_binomial = is_binomial(cleaned_scientificname)) %>%
        dplyr::distinct(cleaned_scientificname, .keep_all = TRUE) %>%
        dplyr::filter(is_binomial == TRUE)
}




map_kozloff <- function(koz_idig) {
    state <- maps::map("world", fill = TRUE, plot = FALSE)
    ## convert the 'map' to something we can work with via geom_map
    IDs <- sapply(strsplit(state$names, ":"), function(x) x[1])
    state <- map2SpatialPolygons(state, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))

    ## this does the magic for geom_map
    state_map <- fortify(state)

    koz_idig %>%
        filter(!is.na(decimallatitude), !is.na(decimallongitude)) %>%
        mutate(`phylum` = gsub("[^a-z]", "", tolower(`phylum`))) %>%
    ggplot(.) +
        geom_point(aes(x = decimallongitude, y = decimallatitude, colour = `phylum`),
                   size = .6, alpha = .3) +
        geom_map(data=state_map, map=state_map,
                 aes(x=long, y=lat, map_id=id),
                 fill="gray20", colour = "gray20", size = .05) +
        coord_quickmap(xlim = c(-128, -60), ylim = c(22, 51)) +
        theme_bw() +
        theme(legend.title = element_blank())
}
