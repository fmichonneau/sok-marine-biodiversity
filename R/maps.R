us_raster <- function()
    raster(vals = NA, xmn = -127, ymn = 23, xmx = -61, ymx = 50, res = .5)

make_data_map_sampling_effort <- function() {
    idig <- lapply(store_idigbio_records()$list(), function(x)
        store_idigbio_records()$get(x))
    idig <- bind_rows(idig)
    us_raster <- us_raster()
    pts <- SpatialPoints(data.frame(lon = idig$geopoint.lon,
                                    lat = idig$geopoint.lat))
    r <- rasterize(pts, us_raster, fun = "count")
    gg_r <- as.data.frame(as(r, "SpatialPixelsDataFrame"))
    colnames(gg_r) <- c("value", "x", "y")
    gg_r
}

make_data_map_diversity <- function() {
    idig <- lapply(store_idigbio_records()$list(), function(x)
        store_idigbio_records()$get(x))
    idig <- bind_rows(idig)

    us_raster <- us_raster()
    raster_cell <- mapply(function(x, y) cellFromXY(us_raster, c(x, y)),
                          idig$geopoint.lon, idig$geopoint.lat)

    idig_r <- data.frame(idig, rastercell = raster_cell) %>%
        group_by(rastercell) %>%
        summarize(
            n_spp = length(unique(scientificname))
        )
    us_raster[na.omit(idig_r$rastercell)] <- idig_r$n_spp[!is.na(idig_r$rastercell)]
    gg_r <- as.data.frame(as(us_raster, "SpatialPixelsDataFrame"))
    colnames(gg_r) <- c("value", "x", "y")
    gg_r
}

make_data_map_standardized_diversity <- function(sampling, diversity) {
    sampling <- sampling %>%
        rename(n_specimen = value)
    diversity <- diversity %>%
        rename(n_species = value)

    res <- bind_cols(sampling, dplyr::select(diversity, n_species)) %>%
        dplyr::select(x, y, n_specimen, n_species) %>%
        mutate(value = n_species/n_specimen)

    res
}

make_heatmap_sampling <- function(gg_r) {
    state <- map("world", fill = TRUE, plot = FALSE)
    ## convert the 'map' to something we can work with via geom_map
    IDs <- sapply(strsplit(state$names, ":"), function(x) x[1])
    state <- map2SpatialPolygons(state, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))

    ## this does the magic for geom_map
    state_map <- fortify(state)
    ggplot() +
        geom_raster(data = gg_r, aes(x = x, y = y, fill = value)) +
        geom_map(data=state_map, map=state_map,
                 aes(x=long, y=lat, map_id=id),
                 fill="gray40") +
        coord_quickmap(xlim = c(-128, -60), ylim = c(22, 51)) +
        scale_fill_viridis(trans = "log") +
        theme_bw()
}
