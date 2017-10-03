compare_with_geo <- function(spp_list, geo_list, verbose = FALSE) {
    prepare <- . %>%
            dplyr::filter(is_binomial == TRUE, is_marine == TRUE) %>%
            dplyr::distinct(worms_valid_name, .keep_all = TRUE) %>%
            dplyr::select(worms_id, worms_valid_name,
                          worms_phylum) %>%
            dplyr::filter(!is.na(worms_id))
    list(
        ## in spp_list but not in geo_list
        not_in_list =
            as_data_frame(geo_list) %>%
            dplyr::anti_join(spp_list, by = "worms_valid_name") %>%
            prepare()
       ,
        ## in geo_list but not in spp_list
        not_in_geo =
            as_data_frame(spp_list) %>%
            dplyr::anti_join(geo_list, by = "worms_valid_name") %>%
            prepare()
    ) %>% dplyr::bind_rows(.id = "data_source")
}

combined_regional_by_geo <- function(gom_worms, gom_idigbio, gom_obis,
                                     pnw_worms, pnw_idigbio, pnw_obis,
                                     map_usa) {

    list(
        gom = combine_species_list(checklist = gom_worms, idigbio = gom_idigbio, obis = gom_obis,
                                   map_usa),
        pnw = combine_species_list(checklist = pnw_worms, idigbio = pnw_idigbio, obis = pnw_obis,
                                   map_usa)
    ) %>%
        dplyr::bind_rows(.id = "region")
}

plot_upsetr <- function(csv_file, ...) {
    readr::read_csv(csv_file) %>%
        as.data.frame() %>%
        UpSetR::upset(order.by="freq", mainbar.y.label = "Number of species (intersection size)",
                      sets.x.label = "Number of species \n (set size)", ...)
}


compare_database_overlap <- function(gom_worms, kozloff_worms,
                                     idigbio_gom_records, obis_gom_records,
                                     idigbio_kozloff_records, obis_kozloff_records
                                     ) {
    idig_gom <- compare_with_geo(gom_worms, idigbio_gom_records)
    obis_gom <- compare_with_geo(gom_worms, obis_gom_records)

    comp_gom <- dplyr::bind_rows(list(idigbio = idig_gom, obis = obis_gom),
                                 .id = "database")

    idig_koz <- compare_with_geo(kozloff_worms, idigbio_kozloff_records)
    obis_koz <- compare_with_geo(kozloff_worms, obis_kozloff_records)

    comp_koz <- dplyr::bind_rows(list(idigbio = idig_koz, obis = obis_koz),
                                 .id = "database")

    dplyr::bind_rows(list(gom = comp_gom, pnw = comp_koz), .id = "region")
}

## For a data frame d formated as the input for upset, calculate the size of the
## intersect for the lists specified in ...
## For instance for the intersect of bold, checklist, and idigbio, for the
## total list fromm GOM: set_size("bold", "checklist", "idigbio", d=total_gom_species)
set_size_with_checklist <- function(..., include_checklist = TRUE, d) {
    d <- generate_species_list(d)
    sets <- c(...)
    if (length(sets) < 2) stop("at least 2 sets needed")
    if (!"checklist" %in% sets) stop(sQuote("checklist"), " should be included.")
    if (!all(sets %in% names(d)))
        stop("Sets not found: ", sets[!sets %in% names(d)])
    sel_set <- dplyr::select(d, sets)

    ## We want the species that are in the checklist and in at least one other
    ## data source.
    sum(sel_set[["checklist"]] == as.integer(include_checklist) &
        rowSums(sel_set) >=  (2 - as.integer(!include_checklist)))
}


plot_database_overlap <- function(comp_db) {

    phyla_to_keep <- c("mollusca", "arthropoda", "annelida",
                       "cnidaria", "bryozoa", "porifera",
                       "echinodermata", "chordata")

    comp_db <- comp_db %>%
        dplyr::mutate(worms_phylum = tolower(worms_phylum)) %>%
        dplyr::filter(worms_phylum %in% phyla_to_keep)

    ## to have 2 sided plot, we take the negative value of the count
    ## for the records that are NOT in the geographical range in the
    ## database.
    data_for_plot <- comp_db %>%
        dplyr::count(region, database, data_source, worms_phylum) %>%
        dplyr::mutate(number = ifelse(data_source == "not_in_geo", -n, n)) %>%
        dplyr::select(-n) %>%
        tidyr::spread(data_source, number) %>%
        dplyr::mutate(phylum = capitalize(worms_phylum))

    ## Clean up phylum names to use as labels in plot
    phylum_label <- data_for_plot %>%
        ungroup() %>%
        arrange(not_in_geo) %>%
        distinct(phylum) %>%
        .[[1]]

    data_for_plot$phylum <- factor(data_for_plot$phylum, levels = rev(phylum_label))

    offset <- 250
    dodge_width <- .6
    gg <- ggplot(data_for_plot, aes(x = phylum, color = database)) +
        geom_linerange(aes(x = phylum, ymin = -offset, ymax = -offset + not_in_geo),
                       position = position_dodge(width = dodge_width)) +
        geom_point(aes(y = -offset + not_in_geo), size = .7,
                       position = position_dodge(width = dodge_width)) +
        geom_linerange(aes(x = phylum, ymin = offset, ymax = offset + not_in_list),
                       position = position_dodge(width = dodge_width)) +
        geom_point(aes(y = offset + not_in_list), size = .7,
                        position = position_dodge(width = dodge_width)) +
        geom_text(aes(x = phylum, label = phylum, y = 0),
                  family = "Ubuntu Condensed",
                  inherit.aes = FALSE,
                  size = 4) +
        coord_flip() +
        scale_y_continuous(breaks = c(seq(-1500, 0, by = 250) + -offset,
                                      seq(0, 1500, by = 250) + offset),
                           labels = c(abs(seq(-1500, 0, by = 250)), seq(0, 1500, by = 250))
                           ) +
        facet_wrap(~ region, labeller = as_labeller(c(gom = "Gulf of Mexico", pnw = "Pacific NW"))) +
        theme_ipsum(base_family = "Ubuntu Condensed") +
        scale_color_manual(values = c("#C0DA4C", "#3F1A52")) +
        ylab("Number of species") +
        theme(text = element_text(),
               panel.grid.major.y = element_blank(),
               panel.grid.minor = element_blank(),
               panel.grid.major.x = element_line(linetype = "dotted", size = 0.1),
               axis.title = element_blank(),
               plot.title = element_blank(),
               plot.subtitle = element_blank(),
               plot.caption = element_text(size = 8, margin = margin(b = 10, t = 50)),
               axis.text.x = element_text(size = 10),
               axis.text.y = element_blank(),
               strip.text = element_text(size = 18, face = "bold", hjust = 0.030),
               plot.background = element_blank(),
               legend.text  = element_text(family = "Ubuntu Condensed", size = 10),
               panel.spacing.x = unit(5, "lines")
               ) +
            annotation_custom(
                grob = grid::textGrob(label = "Not in list", hjust = -1,
                                      gp = gpar(fontfamily = "Ubuntu Condensed")),
                ymin = 100,
                ymax = 100,
                xmin = -.6,
                xmax = -.6) +
            annotation_custom(
                grob = grid::textGrob(label = "Not in databases", hjust = 1,
                                      gp = gpar(fontfamily = "Ubuntu Condensed")
                                      ),
                ymin = -100 - offset,
                ymax = -100 - offset,
                xmin = -.6,
                xmax = -.6)
    gt <- ggplot_gtable(ggplot_build(gg))
    gt$layout$clip[grepl("panel", gt$layout$name)] <- "off"
    grid.draw(gt)

}


plot_database_overlap_percent <- function(comp_db, full_list) {

    phyla_to_keep <- c("mollusca", "arthropoda", "annelida",
                       "cnidaria", "bryozoa", "porifera",
                       "echinodermata", "chordata")

    comp_db <- comp_db %>%
        dplyr::mutate(worms_phylum = tolower(worms_phylum)) %>%
        dplyr::filter(worms_phylum %in% phyla_to_keep)

    spp_per_phylum <- full_list %>%
        dplyr::count(region, worms_phylum) %>%
        dplyr::rename(n_spp = n)

    ## to have 2 sided plot, we take the negative value of the count
    ## for the records that are NOT in the geographical range in the
    ## database.
    data_for_plot_raw <- comp_db %>%
        dplyr::count(region, database, data_source, worms_phylum) %>%
        dplyr::left_join(spp_per_phylum, by = c("region", "worms_phylum")) %>%
        dplyr::mutate(p = n/n_spp) %>%
        dplyr::mutate(number = ifelse(data_source == "not_in_geo", -p, p))

    prop_for_plot <- data_for_plot_raw %>%
        dplyr::select(-p, -n, -n_spp) %>%
        tidyr::spread(data_source, number)

    nspp_for_plot <- data_for_plot_raw %>%
        dplyr::select(-p, -number, -n_spp) %>%
        tidyr::spread(data_source, n) %>%
        dplyr::rename(
                   "nspp_not_in_geo"  = "not_in_geo",
                   "nspp_not_in_list" = "not_in_list"
               )

    data_for_plot <- dplyr::left_join(prop_for_plot,
                                      nspp_for_plot,
                                      by = c("region", "database", "worms_phylum")) %>%
        dplyr::mutate(phylum = capitalize(worms_phylum))


    ## Clean up phylum names to use as labels in plot
    phylum_label <- data_for_plot %>%
        ungroup() %>%
        arrange(not_in_geo) %>%
        distinct(phylum) %>%
        .[[1]]

    data_for_plot$phylum <- factor(data_for_plot$phylum,
                                   ## levels = rev(phylum_label))
                                   levels = rev(c("Arthropoda", "Mollusca", "Annelida",
                                              "Cnidaria", "Echinodermata", "Chordata",
                                              "Porifera", "Bryozoa")))

    offset <- .5
    dodge_width <- .7
    gg <- ggplot(data_for_plot, aes(x = phylum, color = database)) +
        geom_linerange(aes(x = phylum, ymin = -offset, ymax = -offset + not_in_geo),
                       position = position_dodge(width = dodge_width)) +
        geom_point(aes(y = -offset + not_in_geo), size = .7,
                       position = position_dodge(width = dodge_width)) +
        geom_linerange(aes(x = phylum, ymin = offset, ymax = offset + not_in_list),
                       position = position_dodge(width = dodge_width)) +
        geom_point(aes(y = offset + not_in_list), size = .7,
                        position = position_dodge(width = dodge_width)) +
        geom_text(aes(x = phylum, label = phylum, y = 0),
                  family = "Ubuntu Condensed",
                  inherit.aes = FALSE,
                  size = 4) +
        geom_text(aes(x = phylum, y = -offset + not_in_geo - .05, label = nspp_not_in_geo),
                  position = position_dodge(width = dodge_width),
                  family = "Ubuntu Condensed", hjust = "outward", size = 2.75) +
        geom_text(aes(x = phylum, y = offset + not_in_list + .05, label = nspp_not_in_list),
                  position = position_dodge(width = dodge_width),
                  family = "Ubuntu Condensed", hjust = "outward", size = 2.75) +
        coord_flip() +
        facet_wrap(~ region, labeller = as_labeller(c(gom = "Gulf of Mexico",
                                                      pnw = "Pacific Northwest"))) +
        scale_y_continuous(breaks = c(seq(-1, 0, by = .2) + -offset,
                                      seq(0, 1, by = .2) + offset),
                           labels = c(abs(seq(-1, 0, by = .2)), seq(0, 1, by = .2))
                           ) +

        theme_ipsum(base_family = "Ubuntu Condensed") +
        scale_color_manual(values = c("#C0DA4C", "#3F1A52")) +
        ylab("Number of species") +
        theme(text = element_text(),
               panel.grid.major.y = element_blank(),
               panel.grid.minor = element_blank(),
               panel.grid.major.x = element_line(linetype = "dotted", size = 0.1),
               axis.title = element_blank(),
               plot.title = element_blank(),
               plot.subtitle = element_blank(),
               plot.caption = element_text(size = 8, margin = margin(b = 10, t = 50)),
               axis.text.x = element_text(size = 10),
               axis.text.y = element_blank(),
               strip.text = element_text(size = 18, face = "bold", hjust = 0.030),
               plot.background = element_blank(),
               legend.text  = element_text(family = "Ubuntu Condensed", size = 10),
               panel.spacing.x = unit(5, "lines")
               ) +
            annotation_custom(
                grob = grid::textGrob(label = "Not in list", hjust = -1,
                                      gp = gpar(fontfamily = "Ubuntu Condensed")),
                ymin = .1,
                ymax = .1,
                xmin = -.6,
                xmax = -.6) +
            annotation_custom(
                grob = grid::textGrob(label = "Not in databases", hjust = 1,
                                      gp = gpar(fontfamily = "Ubuntu Condensed")
                                      ),
                ymin = -.1 - offset,
                ymax = -.1 - offset,
                xmin = -.6,
                xmax = -.6)
    gt <- ggplot_gtable(ggplot_build(gg))
    gt$layout$clip[grepl("panel", gt$layout$name)] <- "off"
    grid.draw(gt)

}

get_species_in_common <- function(gom_worms, idigbio_gom_records, obis_gom_records,
                                  kozloff_worms, idigbio_kozloff_records, obis_kozloff_records) {

    list(
        n_spp_gom = gom_worms %>%
            dplyr::select(worms_valid_name) %>%
            dplyr::n_distinct(),
        n_spp_gom_idigbio_obis =  gom_worms %>%
            dplyr::semi_join(idigbio_gom_records, by = "worms_valid_name") %>%
            dplyr::semi_join(obis_gom_records, by = "worms_valid_name") %>%
            dplyr::select(worms_valid_name) %>%
            dplyr::n_distinct(),
        n_spp_gom_obis =  gom_worms %>%
            dplyr::semi_join(idigbio_gom_records, by = "worms_valid_name") %>%
            dplyr::select(worms_valid_name) %>%
            dplyr::n_distinct(),
        ###
        n_spp_kozloff = kozloff_worms %>%
            dplyr::select(worms_valid_name) %>%
            dplyr::n_distinct(),
        n_spp_kozloff_idigbio_obis =  kozloff_worms %>%
            dplyr::semi_join(idigbio_kozloff_records, by = "worms_valid_name") %>%
            dplyr::semi_join(obis_kozloff_records, by = "worms_valid_name") %>%
            dplyr::select(worms_valid_name) %>%
            dplyr::n_distinct(),
        n_spp_kozloff_obis = kozloff_worms %>%
            dplyr::semi_join(idigbio_kozloff_records, by = "worms_valid_name") %>%
            dplyr::select(worms_valid_name) %>%
            dplyr::n_distinct()
        )
}

not_in_list_collected_recently <- function(database_overlap, map_gom, map_pnw) {

    .idig <- . %>%
        fetch_spp_from_idigbio() %>%
        parse_year() %>%
        dplyr::mutate(
                   cleaned_scientificname = cleanup_species_names(scientificname),
                   is_binomial = is_binomial(cleaned_scientificname)
               ) %>%
        add_worms() %>%
        filter(!is.na(worms_valid_name), !is.na(year))

    .obis <- . %>%
        fetch_spp_from_obis(feather_out = NULL) %>%
        dplyr::rename(year = yearcollected) %>%
        dplyr::filter(!is.na(year)) %>%
        dplyr::mutate(
                   cleaned_scientificname = cleanup_species_names(scientificname),
                   is_binomial = is_binomial(cleaned_scientificname)
               ) %>%
        add_worms() %>%
        filter(!is.na(worms_valid_name))

    .gom <- . %>%
        is_within_gom_records(map_gom) %>%
        dplyr::filter(is_in_gom == TRUE) %>%
        dplyr::group_by(phylum, worms_valid_name) %>%
        dplyr::summarize(min_year = min(year))

    .pnw <- . %>%
        is_within_pnw_records(map_pnw) %>%
        dplyr::filter(is_in_pnw == TRUE) %>%
        dplyr::group_by(phylum, worms_valid_name) %>%
        dplyr::summarize(min_year = min(year))

    .keep_min <- . %>%
        dplyr::group_by(worms_valid_name) %>%
        dplyr::filter(min_year ==  min(min_year)) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup()

    ## for Gulf of Mexico
    do_gom <- database_overlap %>%
        dplyr::filter(
                   region == "gom",
                   data_source == "not_in_list"
               )

    spp_gom_in_idig <- do_gom %>%
        .idig %>%
        .gom

    spp_gom_in_obis <- do_gom %>%
        .obis %>%
        .gom

    spp_gom <- bind_rows(idigbio = spp_gom_in_idig,
                         obis =  spp_gom_in_obis,
                         .id = "database") %>%
        .keep_min

    ## for Pacific Northwest
    do_pnw <- database_overlap %>%
        dplyr::filter(
                   region == "pnw",
                   data_source == "not_in_list"
               )

    spp_pnw_in_idig <- do_pnw %>%
        .idig %>%
        .pnw

    spp_pnw_in_obis <- do_pnw %>%
        .obis %>%
        .pnw

    spp_pnw <- bind_rows(idigbio = spp_pnw_in_idig,
                         obis = spp_pnw_in_obis,
                         .id = "database") %>%
        .keep_min

    list(
        gom_in_dbs = spp_gom,
        pnw_in_dbs = spp_pnw
    )
}

n_spp_not_in_lists <- function(db_overlap) {
    list(
        gom_not_in_list = db_overlap %>%
            dplyr::filter(region == "gom", data_source == "not_in_list") %>%
            dplyr::pull(worms_valid_name) %>%
            dplyr::n_distinct(),
        pnw_not_in_list = db_overlap %>%
            dplyr::filter(region == "pnw", data_source == "not_in_list") %>%
            dplyr::pull(worms_valid_name) %>%
            dplyr::n_distinct()
    )
}




## Species that appear to not be in iDigbio for the geographical list,
## either because they do not have GPS coordinates associated with
## their records, or they have records that fall outside the
## geographic area.
## Only GOM for now.
get_not_really_in_database <- function(database_overlap, map_usa) {

    idig_gom_not_in_db <- database_overlap %>%
        dplyr::filter(region == "gom",
                      data_source == "not_in_geo",
                      database == "idigbio")

    idig_gom_spp_records <- idig_gom_not_in_db %>%
        fetch_spp_from_idigbio() %>%
        is_within_eez_records(map_usa)

    ## species that are trully absent
    idig_gom_n_spp_not_in_db <- length(setdiff(tolower(idig_gom_not_in_db$worms_valid_name),
                                               unique(idig_gom_spp_records$scientificname)))

    ## species that have no geographic coordinates
    has_no_coords <- function(lat, lon) {
        is.na(lat) & is.na(lon)
    }
    idig_gom_spp_no_coords <- idig_gom_spp_records %>%
        dplyr::group_by(scientificname) %>%
        dplyr::summarize(
                   n_no_coords = sum(has_no_coords(decimallatitude, decimallongitude)),
                   p_no_coords = mean(has_no_coords(decimallatitude, decimallongitude))
               )

    ## total number of species that have records in idigbio
    idig_gom_n_spp_no_coords <- nrow(idig_gom_spp_no_coords)
    idig_gom_p_spp_no_coords <- mean(idig_gom_spp_no_coords$p_no_coords == 1)

    ## proportion of species that have at least one record within US EEZ
    idig_gom_within_usa <- idig_gom_spp_records %>%
        dplyr::filter(!has_no_coords(decimallatitude, decimallongitude)) %>%
        ## only East coast records
        dplyr::filter(decimallongitude > -100 ) %>%
        dplyr::group_by(scientificname) %>%
        dplyr::summarize(
                   is_in_eez = any(is_in_eez)
               )

    idig_gom_n_spp_within_usa <- nrow(idig_gom_within_usa)
    idig_gom_p_spp_within_usa <- mean(idig_gom_within_usa$is_in_eez)

    ## WARNING: Here the `n` refers to the denominator for the
    ## proportions, not the actual number of records
    list(
        idig_gom_n_spp_not_in_db = nrow(idig_gom_not_in_db),
        idig_gom_p_spp_not_in_db = idig_gom_n_spp_not_in_db/n_distinct(idig_gom_not_in_db$worms_valid_name),
        idig_gom_n_spp_no_coords = idig_gom_n_spp_no_coords,
        idig_gom_p_spp_no_coords = idig_gom_p_spp_no_coords,
        idig_gom_n_spp_within_usa = idig_gom_n_spp_within_usa,
        idig_gom_p_spp_within_usa = idig_gom_p_spp_within_usa
    )
}
