compare_with_geo <- function(spp_list, geo_list, verbose = FALSE) {
    list(
        ## in spp_list but not in geo_list
        not_in_list =
            as_data_frame(geo_list) %>%
            dplyr::anti_join(spp_list, by = "worms_valid_name") %>%
            dplyr::filter(is_binomial == TRUE, is_marine == TRUE) %>%
            dplyr::distinct(worms_valid_name, .keep_all = TRUE) %>%
            dplyr::select(worms_id, worms_valid_name,
                          phylum = taxon_name) %>%
            dplyr::mutate(classification = get_classification_from_wid(worms_id, verbose),
                          classification_df = map(classification, unfold_classification)) %>%
            tidyr::unnest(classification_df)
       ,
        ## in geo_list but not in spp_list
        not_in_geo =
            as_data_frame(spp_list) %>%
            dplyr::anti_join(geo_list, by = "worms_valid_name") %>%
            dplyr::filter(is_binomial == TRUE, is_marine == TRUE) %>%
            dplyr::distinct(worms_valid_name, .keep_all = TRUE) %>%
            dplyr::select(worms_id, worms_valid_name,
                          phylum = taxon_name) %>%
            dplyr::mutate(classification = get_classification_from_wid(worms_id, verbose),
                          classification_df = purrr::map(classification, unfold_classification)) %>%
            tidyr::unnest(classification_df)

    ) %>% dplyr::bind_rows(.id = "data_source")
}

generate_upsetr_csv <- function(..., file) {

    has_bold_record <- function(worms_sp) {
        vapply(worms_sp, function(w) {
            r <- store_bold_specimens_per_species()$get(tolower(w))
            if (is.null(r) || inherits(r, "character"))
                0L
            else nrow(r)
        }, integer(1))
    }

    d <- list(...)
    d <- lapply(d, function(x) {
        if (exists("taxon_name", x))
            rename_(x, "phylum" = "taxon_name")
    })

    dplyr::bind_rows(d, .id = "database") %>%
        dplyr::mutate(phylum = tolower(phylum)) %>%
        dplyr::count(phylum, worms_valid_name, database) %>%
        tidyr::spread(database, n) %>%
        dplyr::filter(!is.na(worms_valid_name)) %>%
        dplyr::mutate(bold = has_bold_record(worms_valid_name)) %>%
        dplyr::mutate_if(is.integer, funs(if_else(is.na(.) | . == 0L, 0L, 1L))) %>%
        readr::write_csv(path = file)

    file
}


plot_upsetr <- function(csv_file, ...) {
    readr::read_csv(csv_file) %>%
        as.data.frame() %>%
        UpSetR::upset(order.by="freq", mainbar.y.label = "Number of species (intersection size)",
                      sets.x.label = "Number of species (set size)", ...)
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

    comp_db <- dplyr::bind_rows(list(gom = comp_gom,
                                     kozloff = comp_koz), .id = "region")
}

compare_taxonomy_database_overlap <- function(database_overlap) {
    database_overlap %>%
        dplyr::count(region, database, data_source, Phylum, Order)
}


plot_database_overlap <- function(comp_db) {

    phyla_to_keep <- c("mollusca", "arthropoda", "annelida",
                       "cnidaria", "bryozoa", "porifera",
                       "echinodermata", "chordata")

    comp_db <- comp_db %>%
        dplyr::mutate(phylum = tolower(phylum)) %>%
        dplyr::filter(phylum %in% phyla_to_keep)

    ## to have 2 sided plot, we take the negative value of the count
    ## for the records that are NOT in the geographical range in the
    ## database.
    data_for_plot <- comp_db %>%
        dplyr::count(region, database, data_source, phylum) %>%
        dplyr::mutate(number = ifelse(data_source == "not_in_geo", -n, n)) %>%
        dplyr::select(-n) %>%
        tidyr::spread(data_source, number) %>%
        dplyr::mutate(phylum = capitalize(phylum))

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
        facet_wrap(~ region, labeller = as_labeller(c(gom = "Gulf of Mexico", kozloff = "Pacific NW"))) +
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

not_in_list_collected_recently <- function(database_overlap) {

    .f <- . %>% fetch_spp_from_idigbio() %>%
        idigbio_parse_year() %>%
        dplyr::mutate(
                   cleaned_scientificname = cleanup_species_names(scientificname),
                   is_binomial = is_binomial(scientificname)
               ) %>%
        add_worms() %>%
        group_by(phylum, worms_valid_name) %>%
        summarize(min_year = min(year))


    ## for idigbio gom
    do_idig_gom <- database_overlap %>%
        dplyr::filter(
                   region == "gom",
                   data_source == "not_in_list",
                   database == "idigbio"
               )

    spp_gom_in_idig <- do_idig_gom %>% .f

    ## for idigbio pnw
    do_idig_pnw <- database_overlap %>%
        dplyr::filter(
                   region == "kozloff",
                   data_source == "not_in_list",
                   database == "idigbio"
               )
    spp_pnw_in_idig <- do_idig_pnw %>% .f

    list(
        gom_in_idig = spp_gom_in_idig,
        pnw_in_idig = spp_pnw_in_idig
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
