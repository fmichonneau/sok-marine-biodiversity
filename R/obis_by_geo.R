make_gom_grid <- function(map_geojson, cellsize = .5) {
    gom_sp <- geojsonio::geojson_sp(map_geojson)
    gom_bb <- get_bounding_box(gom_sp, cellsize = cellsize)
    bb_to_df(gom_bb)
}


fetch_hook_obis_by_geo <- function(key, namespace) {
    coords <- unlist(strsplit(key, "\\|"))
    coords_qry <- list(xmin = as.numeric(coords[1]),
                       ymax = as.numeric(coords[2]),
                       xmax = as.numeric(coords[3]),
                       ymin = as.numeric(coords[4]))
    coords_poly <- list(
        c(coords_qry$xmin, coords_qry$ymin),
        c(coords_qry$xmin, coords_qry$ymax),
        c(coords_qry$xmax, coords_qry$ymax),
        c(coords_qry$xmax, coords_qry$ymin),
        c(coords_qry$xmin, coords_qry$ymin)
    )
    wkt_coords <- wellknown::polygon(coords_poly, fmt = 3)
    robis::occurrence(geometry = wkt_coords)
}


store_obis_by_geo <- function(coords, store_path = "data/obis_by_geo") {
    storr::storr_external(storr::driver_rds(store_path, mangle_key = TRUE),
                          fetch_hook_obis_by_geo)
}

fill_store_obis_by_geo <- function(map_geojson, cellsize = .5, use_cache = TRUE) {
    map_grid <- make_gom_grid(map_geojson, cellsize)

    if (!use_cache)
        store_obis_by_geo()$destroy()

    res <- lapply(map_grid$key, function(k) {
        .r <- store_obis_by_geo()$get(k)
        if (nrow(.r) > 0) {
            .r <- .r[tolower(.r$phylum) %in% gom_phyla() &
                     (!tolower(.r$class) %in% chordata_classes_to_rm()), ]
        } else {
           NULL
        }
    })
    dplyr::bind_rows(res) %>%
        dplyr::mutate(cleaned_scientificname = tolower(cleanup_species_names(scientificName)),
                      is_binomial = is_binomial(cleaned_scientificname),
                      taxon_name = tolower(phylum))
}


}
