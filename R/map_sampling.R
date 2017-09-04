
us_raster <- function()
    raster(vals = NA, xmn = -130, ymn = 23, xmx = -61, ymx = 50, res = .2)


data_map <- function(recs) {

    us_raster <- us_raster()

    recs <- recs %>%
        dplyr::mutate(rastercell = mapply(function(x, y)
                          cellFromXY(us_raster, c(x, y)),
                          decimallongitude, decimallatitude))

    recs_r <- recs %>%
        dplyr::group_by(rastercell) %>%
        dplyr::summarize(
            n_spp = n_distinct(worms_valid_name),
            n_samples =  n()
        )

    ## not in use
    h_index <- recs %>%
        dplyr::select(phylum, worms_valid_name, rastercell) %>%
        dplyr::group_by(rastercell, worms_valid_name) %>%
        dplyr::summarize(n_records_per_spp = n()) %>%
        dplyr::group_by(rastercell) %>%
        dplyr::mutate(n_records_per_cell = sum(n_records_per_spp),
                      p_i = n_records_per_spp/n_records_per_cell) %>%
        dplyr::group_by(rastercell) %>%
        dplyr::summarize(h_idx = - sum(p_i * log(p_i, 2)))

    recs_r <- dplyr::full_join(recs_r, h_index, by = "rastercell")
    as.data.frame(us_raster, xy = TRUE, na.rm = FALSE) %>%
        dplyr::as_tibble() %>%
        dplyr::mutate(rastercell = row_number()) %>%
        dplyr::full_join(recs_r, by = "rastercell") %>%
        dplyr::select(-layer)
}

data_map_diversity <- function(dm) {
    dplyr::select(dm, x, y, n_spp) %>%
        dplyr::rename(value = n_spp)
}

data_map_samples <- function(dm) {
    dplyr::select(dm, x, y, n_samples) %>%
        dplyr::rename(value = n_samples)
}

data_map_standardized_diversity <- function(sampling, diversity) {
    sampling <- sampling %>%
        rename(n_specimen = value)
    diversity <- diversity %>%
        rename(n_species = value)

    res <- bind_cols(sampling, dplyr::select(diversity, n_species)) %>%
        dplyr::select(x, y, n_specimen, n_species) %>%
        mutate(value = n_species*n_species/n_specimen)

    res
}


make_heatmap <- function(gg_r, title) {

    ## we use the world map to get Canda, Mexico and Caribbean islands
    state <- maps::map("world", fill = TRUE, plot = FALSE)
    ## convert the 'map' to something we can work with via geom_map
    IDs <- sapply(strsplit(state$names, ":"), function(x) x[1])
    state <- map2SpatialPolygons(state, IDs = IDs,
                                 proj4string = CRS("+proj=longlat +datum=WGS84"))

    us_bathy <- suppressMessages(getNOAA.bathy(lon1 = -128, lon2 = -60,
                                               lat1 = 22, lat2 = 51,
                                               keep = TRUE)) %>%
        fortify() %>%
        filter(z < 0 & z > -1500)

    ## this does the magic for geom_map
    state_map <- fortify(state)

    mid_point <-  log(quantile(seq(min(gg_r$value, na.rm = TRUE),
                                   max(gg_r$value, na.rm = TRUE),
                                   by = 1), .02))

    ggplot() +
        geom_raster(data = gg_r, aes(x = x, y = y, fill = value), na.rm = TRUE) +
        scale_fill_gradient2(low = "#5E98AE", mid = "#E3C94A", high = "#D5331E",
                             midpoint = mid_point,
                             breaks = c(1, 10, 100, 1000, 5000), trans = "log",
                             na.value = NA) +
        geom_map(data=state_map, map=state_map,
                 aes(map_id=id),
                 fill="gray20", colour = "gray20", size = .05) +
        geom_contour(data = us_bathy, aes(x = x, y = y, z = z),
                     colour = "gray80", binwidth = 500, size = .1) +
        coord_quickmap(xlim = c(-128, -60), ylim = c(22, 51)) +
        theme_bw(base_family = "Ubuntu Condensed") +
        theme(legend.title = element_blank()) +
        ggtitle(title) +
        xlab("Longitude") + ylab("Latitude")
}

make_heatmap_by_phylum <- function(recs, file = "figures/map_diversity_per_phylum.pdf") {
    uniq_phyla <- unique(recs$phylum)

    res <- parallel::mclapply(uniq_phyla, function(p) {
                         recs_sub <- recs[recs$phylumrg == p, ]
                         if (nrow(recs_sub) < 10) return(NULL)
                         ggr <- make_data_map_diversity(recs_sub)
                         ggr
                     }, mc.cores = 8L)
    has_data <- !vapply(res, is.null, logical(1))
    res <- res[has_data]
    max_limit <- dplyr::bind_rows(res) %>%
        max(.$value)
    names(res) <- uniq_phyla[has_data]
    pmaps <- parallel::mclapply(seq_along(res),
                       function(gg) {
                           make_heatmap_sampling(res[[gg]], names(res)[gg],
                                                 limits = max_limit)
                  }, mc.cores = 8L)
    pdf(file = file)
    on.exit(dev.off())
    for (i in seq_along(pmaps)) {
       print( pmaps[[i]])
    }
}


animated_map <- function(recrds, file = "/tmp/sampling_map.mp4")  {

    gg_r <- split(recrds, recrds$year)
    gg_r <- lapply(gg_r, data_map)
    gg_r <- bind_rows(gg_r, .id = "year")
    gg_r <- dplyr::select(gg_r, year, x, y, value = n_samples)

    state <- maps::map("world", fill = TRUE, plot = FALSE)

    ## convert the 'map' to something we can work with via geom_map
    IDs <- sapply(strsplit(state$names, ":"), function(x) x[1])
    state <- map2SpatialPolygons(state, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))

    us_bathy <- suppressMessages(getNOAA.bathy(lon1 = -128, lon2 = -60,
                                               lat1 = 22, lat2 = 51,
                                               keep = TRUE)) %>%
        fortify() %>%
        filter(z < 0 & z > -1500)

    mid_point <-  log(quantile(seq(min(gg_r$value, na.rm = TRUE),
                                   max(gg_r$value, na.rm = TRUE),
                                   by = 1), .02))

    ## this does the magic for geom_map
    state_map <- fortify(state)

    p <- ggplot() +
        geom_raster(data = gg_r, aes(x = x, y = y, fill = value, frame = year), na.rm = TRUE) +
        scale_fill_gradient2(low = "#5E98AE", mid = "#E3C94A", high = "#D5331E",
                             midpoint = mid_point,
                             breaks = c(1, 10, 100, 1000, 5000), trans = "log",
                             na.value = NA) +
        geom_map(data=state_map, map=state_map,
                 aes(map_id=id),
                 fill="gray20", colour = "gray20", size = .05) +
        geom_contour(data = us_bathy, aes(x = x, y = y, z = z),
                     colour = "gray80", binwidth = 500, size = .1) +
        coord_quickmap(xlim = c(-128, -60), ylim = c(22, 51)) +
        theme_bw(base_family = "Ubuntu Condensed") +
        theme(legend.title = element_blank()) +
        xlab("Longitude") + ylab("Latitude")

    animation::ani.options(ani.width = 1200, ani.height =  800, interval = .2)
    gganimate::gganimate(p, file)
}



bubble_map <- function(recrds, file = "/tmp/sampling_map.mp4")  {

    ## number of frames per year:
    n_frames <- 20

    gg_r <- recrds
    gg_r <- split(gg_r, gg_r$year)
    gg_r <- lapply(gg_r, data_map)
    gg_r <- bind_rows(gg_r, .id = "year")
    gg_r <- dplyr::select(gg_r, year, x, y, value = n_samples)

    state <- maps::map("world", fill = TRUE, plot = FALSE)

    ## convert the 'map' to something we can work with via geom_map
    IDs <- sapply(strsplit(state$names, ":"), function(x) x[1])
    state <- map2SpatialPolygons(state, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))

    us_bathy <- suppressMessages(getNOAA.bathy(lon1 = -128, lon2 = -60,
                                               lat1 = 22, lat2 = 51,
                                               keep = TRUE)) %>%
        fortify() %>%
        filter(z < 0 & z > -1500)

    mid_point <-  log(quantile(seq(min(gg_r$value, na.rm = TRUE),
                                   max(gg_r$value, na.rm = TRUE),
                                   by = 1), .02))

    ## this does the magic for geom_map
    state_map <- fortify(state)

    gg_split <- split(gg_r, gg_r$year)
    ts_l <- lapply(gg_split, function(x) {

        bubbles_start <- x %>%
            dplyr::filter(!is.na(value)) %>%
            dplyr::mutate(color = "red",
                          size = 10 * value,
                          alpha = 1)
        bubbles_end <- bubbles_start %>%
            dplyr::mutate(size = .1,
                          alpha = 0)

        ts <- list(bubbles_start, bubbles_end)
        tf <- tweenr::tween_states(ts, tweenlength = 3, statelength = 1,
                                   ease = "quadratic-out", nframes = n_frames)
        tf
    })
    tf <- bind_rows(ts_l, .id = "year_frame") %>%
        dplyr::mutate(year_frame = as.integer(year_frame),
                      cum_frame = n_frames * (year_frame - min(year_frame)),
                      full_frame = .frame + cum_frame,
                      x_year = -100, y_year = 40)

    p <- ggplot(data = tf, aes(frame = full_frame)) +
        geom_raster(data = gg_r, aes(x = x, y = y, fill = value), na.rm = TRUE,
                    inherit.aes = FALSE) +
        scale_fill_gradient2(low = "#5E98AE", mid = "#E3C94A", high = "#D5331E",
                             midpoint = mid_point,
                             breaks = c(1, 10, 100, 1000, 5000), trans = "log",
                             na.value = NA) +
        geom_map(data=state_map, map=state_map,
                 aes(map_id=id),
                 fill="gray20", colour = "gray20", size = .05, inherit.aes = FALSE) +
        geom_text(aes(x = x_year, y = y_year, label = year_frame),
                  colour = "white", size = 13) +
        geom_contour(data = us_bathy, aes(x = x, y = y, z = z),
                     colour = "gray80", binwidth = 500, size = .1,
                     inherit.aes = FALSE) +
        geom_point(aes(x, y, color = color, size = size, alpha = alpha)) +
        scale_size(range = c(3, 25), guide = FALSE) +
        scale_colour_identity(guide = FALSE) +
        scale_alpha(guide = FALSE) +
        coord_quickmap(xlim = c(-128, -60), ylim = c(22, 51)) +
        theme_bw(base_family = "Ubuntu Condensed") +
        theme(legend.title = element_blank()) +
        xlab("Longitude") + ylab("Latitude")

    animation::ani.options(ani.width = 1600, ani.height = 900, interval = 1/24)
    gganimate::gganimate(p, file)
}
