
raster_from_map <- function(map, res = .2) {
    bbox <- round(lawn::lawn_bbox(map), 0)
    ## let's make the bounding box a little larger to accommodate rounding:
    raster::raster(vals = NA,
                   xmn = bbox[1] - 1, ymn = bbox[2] - 1,
                   xmx = bbox[3] + 1, ymx = bbox[4] + 1,
                   res = res)
}

add_rastercell <- function(d, raster) {
    d %>%
        dplyr::mutate(rastercell = mapply(function(x, y)
                          cellFromXY(raster, c(x, y)),
                          decimallongitude, decimallatitude))
}

data_map <- function(recs, raster) {

    recs <- recs %>%
        filter(!is.na(decimallatitude) &
               !is.na(decimallongitude)) %>%
        add_rastercell(raster)

    if (any(is.na(recs$rastercell))) {
        stop("something is wrong with the limits of the raster.")
    }

    recs_r <- recs %>%
        dplyr::group_by(rastercell) %>%
        dplyr::summarize(
            n_spp = n_distinct(worms_valid_name),
            n_samples =  n()
            )

    if (exists("worms_phylum", recs)) {
        phy_var <- rlang::sym("worms_phylum")
    } else if (exists("phylum", recs))
        phy_var <- rlang::sym("phylum")

    ## not in use
    h_index <- recs %>%
        dplyr::select(!!phy_var, worms_valid_name, rastercell) %>%
        dplyr::group_by(rastercell, worms_valid_name) %>%
        dplyr::summarize(n_records_per_spp = n()) %>%
        dplyr::group_by(rastercell) %>%
        dplyr::mutate(n_records_per_cell = sum(n_records_per_spp),
                      p_i = n_records_per_spp/n_records_per_cell) %>%
        dplyr::group_by(rastercell) %>%
        dplyr::summarize(h_idx = - sum(p_i * log(p_i, 2)))

    recs_r <- dplyr::full_join(recs_r, h_index, by = "rastercell")
    as.data.frame(raster, xy = TRUE, na.rm = FALSE) %>%
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

n_rastercell_per_species <- function(recs, raster) {
    recs <- add_rastercell(recs, raster)

    recs %>%
        dplyr::group_by(phylum, worms_valid_name) %>%
        dplyr::summarize(
            n_cell = n_distinct(rastercell)
            ) %>%
        dplyr::arrange(desc(n_cell))
}

abundance_sample_species <- function(recs) {
    recs %>%
        dplyr::count(worms_phylum, worms_valid_name) %>%
        dplyr::filter(worms_phylum %in% c("arthropoda", "annelida", "mollusca",
                                    "echinodermata", "cnidaria", "porifera")) %>%
        dplyr::group_by(worms_phylum) %>%
        dplyr::mutate(rank_n = max(dplyr::min_rank(n)) - dplyr::min_rank(n)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(worms_phylum = capitalize(worms_phylum)) %>%
        ggplot(aes(x = rank_n, y = n, colour = worms_phylum)) +
        geom_line() +
        geom_point(size = .2) +
        scale_y_log10() +
        xlab("Rank abundance") + ylab("Number of samples") +
        scale_colour_hc(name = "")
}

plot_rank_abundance <- function(recs) {
    recs %>%
        dplyr::filter(worms_phylum %in% c("arthropoda", "annelida", "mollusca",
                                    "echinodermata", "cnidaria", "porifera")) %>%
        dplyr::group_by(worms_phylum) %>%
        dplyr::mutate(rank_n = max(dplyr::min_rank(n_cell)) - dplyr::min_rank(n_cell)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(worms_phylum = capitalize(worms_phylum)) %>%
        ggplot(aes(x=rank_n, y=n_cell, colour=worms_phylum)) +
        geom_line() +
        geom_point(size = .2) +
        scale_y_log10() +
        xlab("Rank abundance") + ylab("Number of cells occupied") +
        scale_colour_hc(name = "")
}

data_map_standardized_diversity <- function(sampling, diversity) {

    sampling <- sampling %>%
        dplyr::rename(n_specimen = value)
    diversity <- diversity %>%
        dplyr::rename(n_species = value)

    res <- dplyr::full_join(sampling, diversity, by = c("x", "y")) %>%
        dplyr::select(x, y, n_specimen, n_species) %>%
        dplyr::mutate(value = n_species*n_species/n_specimen)

    res
}

limits_from_map <- function(map) {
    limits <- round(lawn_bbox(map), 0) + c(-.5, -.5, .5, .5)
    names(limits) <- c("lon1", "lat1", "lon2", "lat2")
    as.list(limits)
}

bathy_from_map <- function(map) {
    limits <- limits_from_map(map)
    suppressMessages(getNOAA.bathy(lon1 = limits$lon1, lon2 = limits$lon2,
                                   lat1 = limits$lat1, lat2 = limits$lat2,
                                   keep = TRUE))
}


make_heatmap <- function(gg_r, title, base_map) {
    limits <- limits_from_map(base_map)

    ## we use the world map to get Canada, Mexico and Caribbean islands
    state <- maps::map("world", fill = TRUE, plot = FALSE)
    ## convert the 'map' to something we can work with via geom_map
    IDs <- sapply(strsplit(state$names, ":"), function(x) x[1])
    state <- map2SpatialPolygons(state, IDs = IDs,
                                 proj4string = CRS("+proj=longlat +datum=WGS84"))

    us_bathy <- bathy_from_map(base_map) %>%
        fortify() %>%
        filter(z < 0 & z > -1500)

    ## this does the magic for geom_map
    state_map <- fortify(state)

    mid_point <-  log(quantile(seq(min(gg_r$value, na.rm = TRUE),
                                   max(gg_r$value, na.rm = TRUE),
                                   by = 1), .02))

    geo_limits <- fortify(geojsonio::geojson_sp(base_map), id = "id")

    ggplot() +
        geom_map(map = geo_limits, data = geo_limits, aes(map_id = id), fill = "gray80") +
        geom_raster(data = gg_r, aes(x = x, y = y, fill = value), na.rm = TRUE) +
        scale_fill_gradient2(low = "#5E98AE", mid = "#E3C94A", high = "#D5331E",
                             midpoint = mid_point,
                             breaks = c(1, 10, 100, 1000, 5000), trans = "log",
                             na.value = NA) +
        geom_map(data=state_map, map=state_map,
                 aes(map_id=id),
                 fill="gray20", colour = "gray20", size = .05) +
        geom_contour(data = us_bathy, aes(x = x, y = y, z = z),
                     colour = "gray30", binwidth = 500, size = .1) +
        coord_quickmap(xlim = c(limits$lon1, limits$lon2),
                       ylim = c(limits$lat1, limits$lat2)) +
        theme_bw(base_family = "Ubuntu Condensed") +
        theme(legend.title = element_blank()) +
        ggtitle(title) +
        xlab("Longitude") + ylab("Latitude")

}


make_heatmap_by_phylum <- function(recs, file, raster, base_map) {

    if (exists("worms_phylum", recs))
        phy_var <- "worms_phylum"
    else if (exists("phylum", recs))
        phy_var <- "phylum"
    else stop("can't find phylum nor worms_phylum")

    uniq_phyla <- unique(recs[[phy_var]])

    res <- parallel::mclapply(uniq_phyla, function(p) {
                         recs_sub <- recs[recs[[phy_var]] == p, ]
                         if (nrow(recs_sub) < 10) return(NULL)
                         ggr <- data_map(recs_sub, raster) %>% data_map_diversity()
                         ggr
                     }, mc.cores = getOption("mc.cores"))

    has_data <- !vapply(res, is.null, logical(1))
    res <- res[has_data]
    max_limit <- dplyr::bind_rows(res) %>%
        max(.$value)

    names(res) <- uniq_phyla[has_data]
    pmaps <- parallel::mclapply(seq_along(res),
                       function(gg) {
                           make_heatmap(res[[gg]], names(res)[gg], base_map = base_map)
                  }, mc.cores = 8L)
    pdf(file = file)
    on.exit(dev.off())
    for (i in seq_along(pmaps)) {
       print( pmaps[[i]])
    }
}




get_bubble_map_data <- function(recrds, raster, out = "data/bubble_map_data.csv") {

    ## number of frames per year:
    n_frames <- 15

    gg_r <- recrds
    gg_r <- split(gg_r, gg_r$year)
    gg_r <- lapply(gg_r, data_map, raster)
    gg_r <- bind_rows(gg_r, .id = "year")


    ## get the cumulative number of samples per year
    cum_n_smpl <- gg_r %>%
        dplyr::mutate(n_samples = replace(n_samples, is.na(n_samples), 0L)) %>%
        split(gg_r$year) %>%
        purrr::map(pluck, "n_samples") %>%
        purrr::transpose() %>%
        purrr::map(cumsum) %>%
        purrr::transpose() %>%
        purrr::simplify_all() %>%
        tibble::as_tibble() %>%
        tidyr::gather() %>%
        dplyr::select(cum_n_samples = value)

    gg_r <- bind_cols(gg_r, cum_n_smpl)
    gg_r <- dplyr::select(gg_r, year, x, y, value = n_samples, cum_n_samples)


    gg_split <- split(gg_r, gg_r$year)
    ts_l <- parallel::mclapply(gg_split, function(x) {

        bubbles_start <- x %>%
            dplyr::filter(!is.na(x)) %>%
            dplyr::mutate(color = "red",
                          size = 10 * value,
                          alpha = 1)
        bubbles_end <- bubbles_start %>%
            dplyr::mutate(size = .1,
                          alpha = 0)

        ts <- list(bubbles_start, bubbles_end)
        tf <- tweenr::tween_states(ts, tweenlength = 3, statelength = 1,
                                   ease = "quadratic-out", nframes = n_frames)
        tibble::as_tibble(tf) %>%
            dplyr::filter(!is.na(value) |
                          cum_n_samples > 0L)
    }, mc.cores = getOption("mc.cores")-1)

    dplyr::bind_rows(ts_l, .id = "year_frame") %>%
        dplyr::mutate_if(is.factor, as.character) %>%
        dplyr::mutate(year_frame = as.integer(year_frame),
                      cum_frame = n_frames * (year_frame - min(year_frame)),
                      full_frame = .frame + cum_frame,
                      x_year = -100, y_year = 40) %>%
            readr::write_csv(out)
}

get_bubble_map_species <- function(recrds) {
    recrds %>%
        dplyr::filter(rank %in% c("Species", "Subspecies")) %>%
        dplyr::group_by(worms_valid_name) %>%
        dplyr::filter(year == min(year)) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup()
}


bubble_map <- function(tf, file = "/tmp/sampling_map.mp4")  {
    tf <- readr::read_csv(tf, col_types = cols(
                                  year_frame = col_integer(),
                                  year = col_character(),
                                  x = col_double(),
                                  y = col_double(),
                                  value = col_integer(),
                                  cum_n_samples = col_double(), #to accomodate sci notation
                                  color = col_character(),
                                  size = col_double(),
                                  alpha = col_double(),
                                  .frame = col_integer(),
                                  cum_frame = col_integer(),
                                  full_frame = col_double(),
                                  x_year = col_double(),
                                  y_year = col_double()
                              )
                          )

    state <- maps::map("world", fill = TRUE, plot = FALSE)

    ## convert the 'map' to something we can work with via geom_map
    IDs <- sapply(strsplit(state$names, ":"), function(x) x[1])
    state <- map2SpatialPolygons(state, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))

    us_bathy <- suppressMessages(getNOAA.bathy(lon1 = -128, lon2 = -60,
                                               lat1 = 22, lat2 = 51,
                                               keep = TRUE)) %>%
        fortify() %>%
        filter(z < 0 & z > -1500)

    mid_point <-  log(quantile(seq(min(tf$cum_n_samples, na.rm = TRUE),
                                   max(tf$cum_n_samples, na.rm = TRUE),
                                   by = 1), .02))

    ## this does the magic for geom_map
    state_map <- fortify(state)

    p <- ggplot(data = tf, aes(frame = full_frame)) +
        geom_raster(aes(x = x, y = y, fill = cum_n_samples)) +
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
                     colour = "gray30", binwidth = 500, size = .1,
                     inherit.aes = FALSE) +
        geom_point(data = dplyr::filter(tf, !is.na(value)),
                   aes(x, y, color = color, size = size, alpha = alpha)) +
        scale_size(range = c(3, 25), guide = FALSE) +
        scale_colour_identity(guide = FALSE) +
        scale_alpha(guide = FALSE) +
        coord_quickmap(xlim = c(-128, -60), ylim = c(22, 51)) +
        theme_bw(base_family = "Ubuntu Condensed") +
        theme(legend.title = element_blank()) +
        xlab("Longitude") + ylab("Latitude")

    animation::ani.options(ani.width = 1600, ani.height = 900, interval = 1/30)
    gganimate::gganimate(p, file)
}
