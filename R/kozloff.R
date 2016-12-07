get_kozloff_species <- function(koz_raw) {
    readxl::read_excel(koz_raw, col_types = rep("text", 43)) %>%
        .[, c(1:3, 6:25)] %>%
        dplyr::select(
            counter = Counter,
            phylum = Phylum,
            class = Class,
            order = Order,
            scientific_name_verbatim = ScientificName) %>%
        dplyr::mutate(rank = rep("phylum", nrow(.)),
                      taxon_name = phylum,
                      cleaned_scientificname = cleanup_species_names(scientific_name_verbatim,
                                                                     rm_subgenus = TRUE),
                      is_binomial = is_binomial(cleaned_scientificname)) %>%
        dplyr::filter(is_binomial == TRUE)
}





map_kozloff <- function(koz_idig) {
    state <- map("world", fill = TRUE, plot = FALSE)
    ## convert the 'map' to something we can work with via geom_map
    IDs <- sapply(strsplit(state$names, ":"), function(x) x[1])
    state <- map2SpatialPolygons(state, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))

    ## this does the magic for geom_map
    state_map <- fortify(state)

    koz_idig %>%
        filter(!is.na(geopoint.lon), !is.na(geopoint.lat)) %>%
        mutate(`data.dwc:phylum` = gsub("[^a-z]", "", tolower(`data.dwc:phylum`))) %>%
    ggplot(.) +
        geom_point(aes(x = geopoint.lon, y = geopoint.lat, colour = `data.dwc:phylum`),
                   size = .6, alpha = .3) +
        geom_map(data=state_map, map=state_map,
                 aes(x=long, y=lat, map_id=id),
                 fill="gray20", colour = "gray20", size = .05) +
        coord_quickmap(xlim = c(-128, -60), ylim = c(22, 51)) +
        theme_bw() +
        theme(legend.title = element_blank())
}
