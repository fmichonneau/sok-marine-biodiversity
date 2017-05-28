compare_with_geo <- function(spp_list, geo_list, verbose = FALSE) {
    get_classification <- function(x) {
        lapply(x, function(i) {
            if (verbose) message("getting classification for aphiaid ", i)
            res <- store_worms_classification()$get(as.character(i))
            res <- res[[as.character(i)]]
            res %>% mutate_if(is.factor, as.character)
        })
    }

    unfold_classification <- function(classif, rank) {
        empty_classif <- data_frame(Phylum = character(0),
                                    Class = character(0),
                                    Order = character(0),
                                    Family = character(0))
        cols_keep <- c("Phylum", "Class", "Order", "Family")
        to_keep <- intersect(classif$rank, cols_keep)
        dplyr::distinct(classif, rank, .keep_all = TRUE) %>%
            tidyr::spread(rank, name) %>%
            dplyr::select_(.dots = to_keep) %>%
            dplyr::bind_rows(empty_classif)
    }

    list(
        ## in spp_list but not in geo_list
        not_in_list =
            as_data_frame(geo_list) %>%
            dplyr::anti_join(spp_list, by = "worms_valid_name") %>%
            dplyr::filter(is_binomial == TRUE, is_marine == TRUE) %>%
            dplyr::distinct(worms_valid_name, .keep_all = TRUE) %>%
            dplyr::select(worms_id, worms_valid_name,
                          phylum = taxon_name) %>%
            dplyr::mutate(classification = get_classification(worms_id),
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
            dplyr::mutate(classification = get_classification(worms_id),
                          classification_df = map(classification, unfold_classification)) %>%
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
    phyla_to_keep <- comp_db %>%
        dplyr::distinct(region, phylum) %>%
        dplyr::count(phylum) %>%
        dplyr::filter(n == 2) %>%
        .[["phylum"]]

    comp_db <- comp_db %>%
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

    extrafont::loadfonts(quiet = TRUE)

    offset <- 250
    dodge_width <- .6
    gg <- ggplot(data_for_plot, aes(x = phylum, color = database)) +
        geom_linerange(#data = filter(data_for_plot, data_source == "not_in_geo"),
                       aes(x = phylum, ymin = -offset, ymax = -offset + not_in_geo),
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
                  size = 4, color = "#5D646F") +
   # geom_label(x = 1, y = offset + 300, label = "Not in database", color = "#3A3F4A", family = "Ubuntu Condensed",
   #            fill = "#EFF2F4", size = 5, label.size = 0) +
   # geom_label(x = 1, y = -offset - 300, label = "Not in list", color = "#3A3F4A", family = "Ubuntu Condensed",
   #            fill = "#EFF2f4", size = 5, label.size = 0) +
        coord_flip() +
        scale_y_continuous(breaks = c(seq(-1500, 0, by = 250) + -offset,
                                      seq(0, 1500, by = 250) + offset),
                           labels = c(abs(seq(-1500, 0, by = 250)), seq(0, 1500, by = 250))
                           ) +
        facet_wrap(~ region, labeller = as_labeller(c(gom = "Gulf of Mexico", kozloff = "Pacific NW"))) +
        theme_ipsum(base_family = "Ubuntu Condensed") +
        labs(
            title = "compare db",
            subtitle = "yo"
        ) +
            ylab("Number of species") +
         theme(text = element_text(color = "#3A3F4A"),
               panel.grid.major.y = element_blank(),
               panel.grid.minor = element_blank(),
               panel.grid.major.x = element_line(linetype = "dotted", size = 0.1, color = "#3A3F4A"),
               axis.title = element_blank(),
               plot.title = element_blank(), #element_text(face = "bold", size = 36, margin = margin(b = 10), hjust = 0.030),
               plot.subtitle = element_blank(), #element_text(size = 16, margin = margin(b = 20), hjust = 0.030),
               plot.caption = element_text(size = 8, margin = margin(b = 10, t = 50), color = "#5D646F"),
               axis.text.x = element_text(size = 10, color = "#5D646F"),
               axis.text.y = element_blank(),
               strip.text = element_text(color = "#5D646F", size = 18, face = "bold", hjust = 0.030),
               plot.background = element_rect(fill = "#EFF2F4"),
               #plot.margin = unit(c(2, 2, 2, 2), "cm"),
               #legend.position = "top",
               #legend.spacing = unit(0.01, "lines"),
               legend.text  = element_text(family = "Ubuntu Condensed", size = 10),
               ##       legend.text.align = 0) +
               panel.spacing.x = unit(5, "lines")
               ) +
            annotation_custom(
                grob = grid::textGrob(label = "Not in list", hjust = -1,
                                      gp = gpar(fontfamily = "Ubuntu Condensed", col = "#3A3F4A")),
                ymin = 100,
                ymax = 100,
                xmin = -.6,
                xmax = -.6) +
            annotation_custom(
                grob = grid::textGrob(label = "Not in database", hjust = 1,
                                      gp = gpar(fontfamily = "Ubuntu Condensed", col = "#3A3F4A")
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
