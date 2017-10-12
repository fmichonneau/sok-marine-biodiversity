extract_latitudinal_ranges <- function(d) {
    extract_lat_range <- . %>%
        dplyr::group_by(phylum, worms_valid_name) %>%
        dplyr::summarize(
                   min_latitude = min(decimallatitude),
                   max_latitude = max(decimallatitude),
                   diff_lat = max_latitude - min_latitude,
                   n_records = n()
               )

    dplyr::bind_rows(
               east = d %>%
                   dplyr::filter(is_east_coast) %>%
                   extract_lat_range,
               west = d %>%
                   dplyr::filter(is_west_coast) %>%
                   extract_lat_range,
               .id = "coast"
           )
}


stats_latitude_extent <- function(lat_data) {
    ## Proportion of species with more than 10 records that have latitudinal
    ## range of more than 5°
    lat_data %>%
        dplyr::filter(n_records >= 10) %>%
        dplyr::group_by(coast, phylum) %>%
        dplyr::summarize(
                   p_more_than_5 = mean(diff_lat > 5)
               )
}

if (FALSE) {
    ## show spread of geographic range for various phyla
    ggplot(filter(tt, n_records >= 5,
                  phylum %in% c("arthropoda", "mollusca",
                                "echinodermata", "annelida",
                                "cnidaria"))) +
        geom_segment(aes(x = reorder(reorder(worms_valid_name, diff_lat),
                                     min_latitude),
                         xend = reorder(reorder(worms_valid_name, diff_lat),
                                        min_latitude),
                         color = phylum, y = min_latitude, yend = max_latitude)) +
        facet_grid(phylum ~ coast)
}

n_spp_per_lat <- function(lat_data) {

    ## number of species for each latitudinal slice based on inferred ranges

    slice_size <- .2

    res <- expand.grid(lat_coast = c("east", "west"),
                lat_degrees = seq(22, 60, by = slice_size),
                stringsAsFactors = FALSE)

    res %>%
        dplyr::mutate(n_spp = pmap_int(., function(lat_coast, lat_degrees, ...) {
                          tmp_data <- dplyr::filter(lat_data, coast == lat_coast)
                          sum(tmp_data$min_latitude >= lat_degrees &
                              tmp_data$max_latitude < (lat_degrees + slice_size))
                      }))
}
plot_gradient <- function(idig) {
    idig %>%
        dplyr::mutate(
                   lat_grad = ntile(decimallatitude, 100),
                   lat_cut = cut(decimallatitude, 100)
               ) %>%
        dplyr::mutate(coast = if_else(decimallongitude > -100, "Atlantic Ocean", "Pacific Ocean")) %>%
        dplyr::mutate(coast = if_else(is_in_gom == TRUE, "gom", coast)) %>%
        dplyr::mutate(coast = factor(coast, levels = c("Pacific Ocean", "Atlantic Ocean"))) %>%
        dplyr::filter(coast != "gom") %>%
        dplyr::group_by(coast, lat_grad) %>%
        dplyr::summarize(
                   lat_cat = mean(decimallatitude),
                   n_spp = n_distinct(worms_valid_name),
                   n_obs = n()
               ) %>%
        dplyr::mutate(p_samp = n_spp/(log(n_obs))) %>%
        ggplot(aes(x = lat_cat, y = n_spp, color = coast, size = n_obs)) +
        geom_point() + facet_grid( ~ coast) +
        geom_smooth(method = "lm", formula = y ~ splines::bs(x, degree = 3),
                    show.legend = FALSE, aes(weight = I(sqrt(n_obs)))) +
        scale_size(range = c(0.2, 3),  name = "Number of observations") +
        scale_colour_hc() + guides(color = FALSE) +
        xlab("Latitude") + ylab("Number of Species")
}

####

if (FALSE) {

    ## attempt at plotting diversity on the side of the plot
    library(ggstance)
    library(ggjoy)
    library(ggplot2)
    library(cowplot)


    richness_east <- remake::fetch("combined_records") %>%
        dplyr::filter(decimallongitude > -100) %>%
        dplyr::mutate(lat_cut = cut(decimallatitude, seq(floor(min(decimallatitude)), ceiling(max(decimallatitude)), by = 1)),
                      lat_unit = ceiling(decimallatitude),
                      lat = ntile(decimallatitude, 100)) %>%
        dplyr::group_by(lat) %>%
        dplyr::summarize(n_spp = n_distinct(worms_valid_name))

    pmain <- remake::fetch("idigbio_map_diversity")

    ydens <- cowplot::axis_canvas(pmain, axis = "y") +
        geom_vridgeline(data = richness_east, aes(y = lat, x = 0, width = n_spp), stat = "identity")

    p1 <- insert_yaxis_grob(pmain, ydens, width = grid::unit(.2, "null"), position = "right")
    ggdraw(p1)
}
