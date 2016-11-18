get_kozloff_species <- function(koz_raw) {
    readxl::read_excel(koz_raw, col_types = rep("text", 43)) %>%
        .[, c(1:3, 6:25)] %>%
        dplyr::select(
            counter = Counter,
            phylum = Phylum,
            class = Class,
            order = Order,
            scientific_name_verbatim = ScientificName,
            worms_valid_name = ScientificName_accepted) %>%
        dplyr::mutate(is_marine = rep(TRUE, nrow(.)),
                      rank = rep("phylum", nrow(.)),
                      taxon_name = phylum,
                      cleaned_scientificname = cleanup_species_names(worms_valid_name, rm_subgenus = TRUE),
                      is_binomial = is_binomial(cleaned_valid_name)) %>%
        dplyr::filter(is_binomial == TRUE)
}




graph_kozloff_not_in_idigbio <- function(koz_not_idig) {
    koz_not_idig %>%
        group_by(phylum) %>%
        summarize(
            not_in_idigbio = sum(is.na(uuid_lst))/n(),
            n_spp_not_in_idigbio = sum(is.na(uuid_lst)),
            n_spp_total = n()
        ) %>%
        ggplot(.) +
        geom_bar(aes(x = reorder(phylum, not_in_idigbio), y = not_in_idigbio), stat = "identity") +
        geom_text(aes(x = reorder(phylum, not_in_idigbio), y = .8,
                      label = paste(n_spp_not_in_idigbio, n_spp_total, sep = "/"))) +
        coord_flip() +
        xlab("Phylum") +
        ylab("Proportion of species not in iDigBio")
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
