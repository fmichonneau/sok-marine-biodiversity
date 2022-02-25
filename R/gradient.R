extract_latitudinal_ranges <- function(d) {
  extract_lat_range <- . %>%
    dplyr::group_by(phylum, worms_class, worms_order, worms_family, worms_valid_name) %>%
    dplyr::summarize(
      min_latitude = min(decimallatitude),
      max_latitude = max(decimallatitude),
      diff_lat = max_latitude - min_latitude,
      n_records = n()
    )

  dplyr::bind_rows(
    east = d %>%
      dplyr::filter(is_east_coast) %>%
      extract_lat_range(),
    west = d %>%
      dplyr::filter(is_west_coast) %>%
      extract_lat_range(),
    .id = "coast"
  )
}


stats_latitude_extent <- function(lat_data) {
  ## Proportion of species with more than 10 records that have latitudinal
  ## range of more than 5°
  lat_data %>%
    dplyr::filter(n_records >= 5) %>%
    dplyr::group_by(coast, phylum) %>%
    dplyr::summarize(
      median_lat = median(diff_lat),
      p_more_than_5 = mean(diff_lat > 5)
    )
}

if (FALSE) {
  vv <- remake::fetch("combined_records")
  tt <- extract_latitudinal_ranges(vv)
  ## show spread of geographic range for various phyla
  ggplot(filter(
    tt, n_records >= 5,
    phylum %in% c(
      "arthropoda", "mollusca",
      "echinodermata", "annelida",
      "cnidaria"
    )
  )) +
    geom_segment(aes(
      x = reorder(
        reorder(worms_valid_name, diff_lat),
        min_latitude
      ),
      xend = reorder(
        reorder(worms_valid_name, diff_lat),
        min_latitude
      ),
      color = worms_class, y = min_latitude, yend = max_latitude
    )) +
    facet_grid(phylum ~ coast)
}

n_spp_per_lat <- function(lat_data) {

  ## number of species for each latitudinal slice based on inferred ranges

  slice_size <- .2
  min_lat <- floor(min(lat_data$min_latitude))
  max_lat <- ceiling(max(lat_data$min_latitude))

  res <- expand.grid(
    lat_coast = c("east", "west"),
    lat_degrees = seq(min_lat, max_lat, by = slice_size),
    stringsAsFactors = FALSE
  )

  as.tbl(res) %>%
    dplyr::mutate(n_spp = pmap(., function(lat_coast, lat_degrees, ...) {
      .r <- dplyr::filter(
        lat_data, coast == lat_coast,
        (min_latitude < lat_degrees + slice_size &
          max_latitude >= lat_degrees
        )
      )
      tibble(
        n_spp = nrow(.r),
        n_records = sum(.r$n_records)
      )
    })) %>%
    tidyr::unnest() %>%
    dplyr::mutate(n_spp = replace(n_spp, n_spp == 0, NA))
}

plot_inferred_gradient <- function(dat) {
  res <- dat %>%
    dplyr::mutate(
      lat_coast = case_when(
        lat_coast == "east" ~ "Atlantic Ocean",
        lat_coast == "west" ~ "Pacific Ocean"
      ),
      lat_coast = factor(lat_coast, levels = c(
        "Pacific Ocean",
        "Atlantic Ocean"
      ))
    ) %>%
    dplyr::group_by(lat_coast) %>%
    dplyr::mutate(roll_spp = roll_mean(n_spp, 3, fill = NA))

  ggplot(res) +
    geom_point(aes(
      x = lat_degrees, y = n_spp, # size = n_records,
      colour = lat_coast
    ), alpha = .6) +
    geom_smooth(aes(x = lat_degrees, y = roll_spp, colour = lat_coast), se = FALSE) +
    facet_grid(~lat_coast) +
    scale_colour_hc() +
    guides(color = FALSE) +
    scale_size(range = c(0.1, 4), name = "Number of observations") +
    xlab("Latitude") +
    ylab("Number of inferred species")
}

make_gradient_data <- function(lat_data) {
  slice_size <- .5
  min_lat <- floor(min(lat_data$decimallatitude))
  max_lat <- ceiling(max(lat_data$decimallatitude))

  lat_data %>%
    dplyr::filter(!within_gom) %>%
    dplyr::mutate(
      lat_grad = ntile(decimallatitude, 100),
      lat_cut = cut(decimallatitude, seq(min_lat, max_lat, slice_size)),
      lat_coast = case_when(
        is_east_coast ~ "Atlantic Ocean",
        is_west_coast ~ "Pacific Ocean"
      )
    ) %>%
    dplyr::mutate(lat_coast = factor(lat_coast, levels = c("Pacific Ocean", "Atlantic Ocean"))) %>%
    dplyr::group_by(lat_coast, lat_cut) %>%
    dplyr::summarize(
      lat_cat = mean(decimallatitude),
      n_spp = n_distinct(worms_valid_name),
      n_obs = n()
    ) %>%
    dplyr::mutate(
      roll_spp = roll_mean(n_spp, 3, fill = NA),
      roll_n_obs = roll_mean(n_obs, 3, fill = NA)
    )
}

plot_gradient <- function(lat_data) {
  ggplot(lat_data) +
    geom_point(aes(
      x = lat_cat, y = n_spp, # size = n_obs,
      color = lat_coast
    ), alpha = .6) +
    geom_smooth(aes(x = lat_cat, y = roll_spp, colour = lat_coast), se = FALSE) +
    facet_grid(~lat_coast) +
    scale_size(range = c(0.1, 4), name = "Number of observations") +
    scale_colour_hc() +
    guides(color = FALSE) +
    xlab("Latitude") +
    ylab("Number of recorded species")
}

plot_gradient_sampling <- function(lat_data) {
  ggplot(lat_data) +
    geom_point(aes(x = lat_cat, y = n_obs, color = lat_coast), alpha = .6) +
    geom_smooth(aes(x = lat_cat, y = roll_n_obs, color = lat_coast), se = FALSE) +
    facet_grid(~lat_coast) +
    scale_colour_hc() +
    guides(color = FALSE) +
    scale_y_log10() +
    xlab("Latitude") +
    ylab("Number of observations")
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
    dplyr::mutate(
      lat_cut = cut(decimallatitude, seq(floor(min(decimallatitude)), ceiling(max(decimallatitude)), by = 1)),
      lat_unit = ceiling(decimallatitude),
      lat = ntile(decimallatitude, 100)
    ) %>%
    dplyr::group_by(lat) %>%
    dplyr::summarize(n_spp = n_distinct(worms_valid_name))

  pmain <- remake::fetch("idigbio_map_diversity")

  ydens <- cowplot::axis_canvas(pmain, axis = "y") +
    geom_vridgeline(data = richness_east, aes(y = lat, x = 0, width = n_spp), stat = "identity")

  p1 <- insert_yaxis_grob(pmain, ydens, width = grid::unit(.2, "null"), position = "right")
  ggdraw(p1)
}
