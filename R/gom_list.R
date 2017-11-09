get_gom_species <- function(pattern = "^biogomx-.+\\.csv$") {
    res <- list.files(path = "data-raw/GOM", pattern = pattern, full.name = TRUE) %>%
        purrr::map_df(read_csv,
                      col_type = list(
                          `Species number` = col_character(),
                          `Scientific name` = col_character(),
                          Kingdom = col_character(),
                          Phylum = col_character(),
                          Subphylum = col_character(),
                          Class = col_character(),
                          Subclass = col_character(),
                          Infraclass = col_character(),
                          Superorder = col_character(),
                          Order = col_character(),
                          Suborder = col_character(),
                          Infraorder = col_character(),
                          Section = col_character(),
                          Subsection = col_character(),
                          Superfamily = col_character(),
                          `Above family` = col_character(),
                          Family = col_character(),
                          Subfamily = col_character(),
                          Tribe = col_character(),
                          Supergenus = col_character(),
                          Genus = col_character(),
                          Subgenus = col_character(),
                          Species = col_character(),
                          Subspecies = col_character(),
                          Synonyms = col_character(),
                          `Scientific name author` = col_character(),
                          `Habitat-Biology` = col_character(),
                          `Overall geographic range` = col_character(),
                          `Min depth (m)` = col_double(),
                          `Max depth (m)` = col_double(),
                          Polygon = col_character(),
                          Source = col_character(),
                          References = col_character(),
                          Endnotes = col_character(),
                          Author = col_character(),
                          ## year is character as they include a, b, c...
                          Year = col_character(),
                          `Changes from the book` = col_character(),
                          URL = col_character()
                          )
                      ) %>%
        dplyr::filter(! Subphylum %in% "Vertebrata") %>%
        dplyr::rename(
                   species_number = `Species number`,
                   scientificname_verbatim = `Scientific name`,
                   above_family = `Above family`,
                   scientific_name_author = `Scientific name author`,
                   habitat_biology = `Habitat-Biology`,
                   overall_geographic_range = `Overall geographic range`,
                   min_depth_m = `Min depth (m)`,
                   max_depth_m = `Max depth (m)`,
                   changes_from_book = `Changes from the book`
               ) %>%
        dplyr::distinct(phylum, scientificname_verbatim, .keep_all = TRUE) %>%
        dplyr::mutate_if(is.character, tolower)

    ## Extract taxon names...
    ## First remove extra spaces (a few names have additional spaces)
    res$scientificname_verbatim  <- gsub("\\s{2, }", " ", res$scientificname_verbatim )
    ## ... and quotation marks
    res$scientificname_verbatim  <- gsub("\\\"|\\\'|“|”", "", res$scientificname_verbatim )
    res$cleaned_scientificname <- cleanup_species_names(res$scientificname_verbatim)
    res$is_binomial <- is_binomial(res$cleaned_scientificname)
    names(res) <- tolower(names(res))
    res
}

summarize_richness_per_db <- function(bold_db, idig_db, obis_db, gbif_db, region = c("gom", "pnw")) {

    region <- match.arg(region)

    col_name <- paste0("n_idigbio_in_", region)


    idig_ <- idig_db %>%
        dplyr::group_by(worms_phylum) %>%
        dplyr::summarize(
                   n_idigbio = sum(!is.na(n_idigbio)),
                   n_idigbio_in_us = sum(!is.na(n_idigbio_in_us)),
                   !!col_name := sum(is.na(!!sym(col_name)))
               )

    stopifnot(all(idig_[["n_idigbio"]] >= idig_[["n_idigbio_in_us"]]))

    obis_ <- obis_db %>%
        dplyr::group_by(worms_phylum) %>%
       dplyr::summarize_(
                   .dots = setNames(
                       list(
                           "sum(!is.na(n_obis))",
                           "sum(!is.na(n_obis_in_us))",
                           lazyeval::interp("sum(!is.na(n_obis_in_region))",
                                            n_obis_in_region = as.name(paste0("n_obis_in_", region)))
                       ),
                       c("n_obis", "n_obis_in_us",
                         paste0("n_obis_in_", region)
                         )
                   )
              )
    stopifnot(all(idig_[["n_obis"]] >= idig_[["n_obis_in_us"]]))
    stopifnot(all(idig_[["n_obis"]] >= idig_[[paste0("n_obis_in_", region)]]))

    gbif_ <- gbif_db %>%
        group_by(worms_phylum) %>%
        dplyr::summarize_(
                   .dots = setNames(
                       list(
                           "sum(!is.na(n_gbif))",
                           "sum(!is.na(n_gbif_in_us))",
                           lazyeval::interp("sum(!is.na(n_gbif_in_region))",
                                            n_gbif_in_region = as.name(paste0("n_gbif_in_", region)))
                       ),
                       c("n_gbif", "n_gbif_in_us",
                         paste0("n_gbif_in_", region)
                         )
                   )
               )
    stopifnot(all(idig_[["n_gbif"]] >= idig_[["n_gbif_in_us"]]))
    stopifnot(all(idig_[["n_gbif"]] >= idig_[[paste0("n_gbif_in_", region)]]))


    bold_db %>%
        dplyr::group_by(worms_phylum) %>%
        dplyr::summarize(
            n_barcoded = sum(n_bold_records > 0),
            n_total = n()
        ) %>%
        dplyr::left_join(idig_, by = "worms_phylum") %>%
        dplyr::left_join(obis_, by = "worms_phylum") %>%
        dplyr::left_join(gbif_, by = "worms_phylum") %>%
        dplyr::filter(worms_phylum %in% c("arthropoda", "mollusca", "annelida",
                                    "cnidaria", "echinodermata", "platyhelminthes",
                                    "porifera", "bryozoa", "nematoda", "chordata"
                                    )) %>%
        dplyr::mutate_(.dots = setNames(list(
                           lazyeval::interp("n_idigbio_in_region/n_total",
                                            n_idigbio_in_region = as.name(paste0("n_idigbio_in_", region))),
                           "n_idigbio_in_us/n_total",
                           "n_idigbio/n_total",
                           lazyeval::interp("n_obis_in_region/n_total",
                                            n_obis_in_region = as.name(paste0("n_obis_in_", region))),
                          "n_obis_in_us/n_total",
                           "n_obis/n_total",
                           lazyeval::interp("n_gbif_in_region/n_total",
                                            n_gbif_in_region = as.name(paste0("n_gbif_in_", region))),
                           "n_gbif_in_us/n_total",
                           "n_gbif/n_total"
                       ),
                       c(paste0("prop_idigbio_in_", region),
                         "prop_idigbio_in_us",
                         "prop_idigbio",
                         paste0("prop_obis_in_", region),
                         "prop_obis_in_us",
                         "prop_obis",
                         paste0("prop_gbif_in_", region),
                         "prop_gbif_in_us",
                         "prop_gbif"))
                       ) %>%
        rename(phylum = worms_phylum)

}

plot_richness_per_db <- function(smry_db, region = c("gom", "pnw")) {
    region <- match.arg(region)

    full_region <- c(gom = "GoM", pnw = "PNW")

    phyla_to_plot <- tolower(c("Arthropoda", "Mollusca", "Annelida", "Cnidaria", "Echinodermata",
                        "Platyhelminthes", "Porifera", "Bryozoa", "Chordata"))

    smry_db %>%
        dplyr::filter(phylum %in% phyla_to_plot) %>%
        dplyr::mutate_(.dots = setNames(list(
                           lazyeval::interp("n_idigbio - n_idigbio_in_region",
                                            n_idigbio_in_region = as.name(paste0("n_idigbio_in_", region))),
                           lazyeval::interp("n_obis - n_obis_in_region",
                                            n_obis_in_region = as.name(paste0("n_obis_in_", region))),
                           lazyeval::interp("n_gbif - n_gbif_in_region",
                                            n_gbif_in_region = as.name(paste0("n_gbif_in_", region)))
                           ), c(paste0("n_idigbio_", region, "_diff"),
                                paste0("n_obis_", region, "_diff"),
                                paste0("n_gbif_", region, "_diff")
                                ))
                       )  %>%
        dplyr::select(-n_idigbio, -n_obis, -n_gbif,
                      -n_idigbio_in_us, -n_obis_in_us, -n_gbif_in_us,
                      -starts_with("prop")
                      ) %>%
        tidyr::gather(source_n_spp, n_spp, -phylum) %>%
        dplyr::mutate(source_db = gsub("(n_[a-z]+)(_.+)?", "\\1", source_n_spp)) %>%
        dplyr::filter(source_db != "n_gbif") %>%
        ggplot(aes(x = factor(source_db,
                              levels = rev(c("n_total", "n_obis", "n_idigbio", "n_barcoded")),
                              labels = c("BOLD", "iDigBio", "OBIS", "List")
                              ),
                   y = n_spp,
                   fill = factor(source_n_spp,
                                 levels = rev(
                                     gsub("XXX", region,
                                          c("n_total",
                                            "n_obis_in_XXX", "n_obis_XXX_diff",
                                            #"n_gbif_in_XXX", "n_gbif_XXX_diff",
                                            "n_idigbio_in_XXX", "n_idigbio_XXX_diff",
                                            "n_barcoded"
                                            ))),
                                 labels = rev(
                                     c("Species in list",
                                       "OBIS in region","OBIS global",
                                       #"GBIF in region", "GBIF global",
                                       "iDigBio in region", "iDigBio global",
                                       "BOLD"
                                       ))
                                 )
                   )
               ) +
            geom_bar(stat = "identity", position = "stack") +
        facet_grid(factor(phylum,
                          levels = phyla_to_plot) ~ .) +
        xlab("") + ylab("Number of species") +
        labs(title = "") +
        scale_fill_viridis(name = "Data source", option = "viridis", discrete = TRUE,
                           guide = guide_legend(reverse=TRUE)) +
        theme_bw(base_family = "Ubuntu Condensed") +
        coord_flip()
}
