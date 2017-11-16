
all_idigbio_species_name_cleanup <- . %>%
    ## remove non-ascii characters
    dplyr::mutate(cleaned_scientificname = iconv(scientificname, "latin1", "ASCII", sub = "")) %>%
    ## remove subsp. foo
    dplyr::mutate(cleaned_scientificname = gsub("\\ssubsp\\.? .+$", "", cleaned_scientificname)) %>%
    dplyr::mutate(cleaned_scientificname = cleanup_species_names(cleaned_scientificname)) %>%
    ## remove quotes
    dplyr::mutate(cleaned_scientificname = gsub("\"|\'", "", cleaned_scientificname)) %>%
    ## remove ex. foo bar
    dplyr::mutate(cleaned_scientificname = gsub("\\sex\\.? .+$", "", cleaned_scientificname)) %>%
    ## remove author names in botanical names
    ## in the form l. or (l.) or (c. l.)
    dplyr::mutate(cleaned_scientificname = gsub("\\s\\(?([a-z]+\\.)+\\s?([a-z]+\\.)?\\)?", "", cleaned_scientificname)) %>%
    ## remove authors with & e.g. (bartram & smith)
    dplyr::mutate(cleaned_scientificname = gsub("\\s\\(?[a-z]+\\.?\\s&\\s[a-z]+\\.?\\)?", "", cleaned_scientificname)) %>%
    ## remove synonyms e.g. solariella (=margarita) infundibulum
    dplyr::mutate(cleaned_scientificname =  gsub("\\s\\(=[a-z]+\\)", "", cleaned_scientificname))  %>%
    ## remove names that end with variations of (something)
    dplyr::mutate(cleaned_scientificname = gsub("(\\s\\(\\s?[a-z]+$)|(\\s[a-z]+\\s?\\)$)|(\\s\\(\\s?[a-z]+\\s?\\)$)", "", cleaned_scientificname)) %>%
    ## remove blend, dronen and armstrong OR dronen and armtrong
    dplyr::mutate(cleaned_scientificname = gsub("([a-z]+,\\s)?[a-z]+\\sand\\s[a-z]+", "", cleaned_scientificname)) %>%
    ## remove blend, dronen
    dplyr::mutate(cleaned_scientificname = gsub("\\s[a-z]+,\\s[a-z]+", "", cleaned_scientificname))

add_sub_kingdom <- function(tbl) {
    dplyr::mutate(tbl, sub_kingdom = case_when(
                                     is.na(worms_id)  ~ NA_character_,
                                     worms_phylum == "chordata" &
                                     worms_class %in% c("appendicularia",
                                                        "ascidiacea",
                                                        "leptocardii",
                                                        "thaliacea") ~ "animalia - invertebrates",
                                     worms_phylum == "chordata" &
                                     worms_class %in% c("actinopterygii",
                                                        "aves",
                                                        "elasmobranchii",
                                                        "holocephali",
                                                        "mammalia",
                                                        "myxini",
                                                        "petromyzonti",
                                                        "reptilia") ~ "animalia - vertebrates",
                                     worms_kingdom == "animalia" ~ "animalia - invertebrates",
                                     worms_kingdom == "chromista" ~ "chromista",
                                     worms_kingdom == "plantae" ~ "plantae",
                                     TRUE ~ "others"
                                     ))
}

get_kingdom_worms_stats <- function(worms_stats) {

 ## retrieved from http://www.marinespecies.org/aphia.php?p=browser&id=1821#ct
 n_vertebrates <- 19741

  wrm <- worms_stats %>%
    ## we don't need it here, as we split vertebrates/invertebrates below
    dplyr::filter(phylum != "Chordata - inverts") %>%
    dplyr::mutate(sub_kingdom = case_when(
                    tolower(kingdom) %in% c("fungi", "protozoa", "bacteria",
                                            "archaea", "viruses", "biota incertae sedis") ~ "others",
                    TRUE ~ tolower(kingdom)
                  )) %>%
    dplyr::group_by(sub_kingdom) %>%
    dplyr::summarize(
             n_spp = sum(accepted_species_marine_non_fossil)
           ) %>%
    dplyr::bind_rows(
             data_frame(
               sub_kingdom =  "animalia - vertebrates",
               n_spp = n_vertebrates)
           )
  wrm[wrm$sub_kingdom == "animalia", "n_spp"] <- wrm[wrm$sub_kingdom == "animalia", "n_spp"] - n_vertebrates
  wrm[wrm$sub_kingdom == "animalia", "sub_kingdom"] <- "animalia - invertebrates"
  wrm
}

calc_kingdom_diversity <- function(worms_stats) {

    db <- sok_db()

    get_n_spp <- . %>%
        filter_by_geo("within_eez") %>%
        keep_marine_species_only() %>%
        dplyr::group_by(sub_kingdom) %>%
        dplyr::summarize(
                   n_samples = n(),
                   n_spp = n_distinct(worms_valid_name)
        ) %>%
        dplyr::collect()

    wrm <- get_kingdom_worms_stats(worms_stats)

    ## diversity comparison
    dplyr::bind_rows(
               idigbio = tbl(db, "us_idigbio_worms") %>%
                   add_sub_kingdom() %>%
                   get_n_spp(),
            obis =  tbl(db, "us_obis_worms") %>%
                add_sub_kingdom() %>%
                get_n_spp(),
            worms = wrm,
        .id = "database") %>%
        dplyr::group_by(database) %>%
        dplyr::mutate(prop_spp = n_spp/sum(n_spp),
                      prop_samp = n_samples/sum(n_samples)) %>%
        dplyr::mutate(sub_kingdom = capitalize(sub_kingdom))
}

plot_kingdom_diversity <- function(kng) {
    kng %>%
        ggplot(aes(x = reorder(sub_kingdom, prop_spp), y = prop_spp, fill = database)) +
        geom_col(position = "dodge") + coord_flip() +
        geom_text(aes(y = prop_spp + .01, label = n_spp), position = position_dodge(.9),
                      hjust = .1,  family = "Ubuntu Condensed") +
        scale_fill_hc(name = "Source", labels = c("iDigBio", "OBIS", "Global diversity (WoRMS)")) +
        ylim(c(0, .85)) +
        xlab(NULL) + ylab("Proportion of total diversity (per source)") +
        theme_ipsum(base_family = "Ubuntu Condensed")

}

plot_kingdom_samples <- function(kng) {
    kng %>%
        dplyr::mutate(prop_samp = replace(prop_samp, is.na(prop_samp), prop_spp)) %>%
    ggplot(aes(x = reorder(sub_kingdom, prop_samp), y = prop_samp, fill = database)) +
    geom_col(position = "dodge") + coord_flip() +
    scale_fill_hc(name = "Source", labels = c("iDigBio", "OBIS", "Global diversity (WoRMS)")) +
    xlab(NULL) + ylab("Proportion of total diversity (per source)")
}

calc_kingdom_stats <- function() {

    db <- sok_db()

    get_median <- . %>%
        group_by(sub_kingdom, worms_phylum, worms_valid_name) %>%
        summarize(
            n_samples = n()
        ) %>%
        collect() %>%
        group_by(sub_kingdom, worms_phylum) %>%
        summarize(
            median_sample = median(n_samples),
            mean_sample = mean(n_samples)
        )

    dplyr::bind_rows(
               idigbio =  tbl(db, "us_idigbio_worms") %>%
                   add_sub_kingdom() %>%
                   get_median(),
            obis =  tbl(db, "us_obis_clean") %>%
                add_sub_kingdom() %>%
                get_median(),
        .id = "database")
}



if (FALSE) {
    ## number of species and records per (sub)kingdom
    idigbio_kingdom_stats("us_idigbio_clean")  %>%
        dplyr::group_by(sub_kingdom, worms_phylum) %>%
        dplyr::summarize(
                   n_samples = n(),
                   n_spp = n_distinct(worms_valid_name)
               ) %>%
        collect() %>%
        ggplot(aes(x=n_samples, y=n_spp, colour=sub_kingdom)) +
        geom_point() +
        geom_label_repel(aes(label=worms_phylum)) +
        scale_x_log10() + scale_y_log10()

     ## comparison of number of samples per species across phyla
      idigbio_kingdom_stats("us_idigbio_clean") %>%
        dplyr::group_by(worms_kingdom, sub_kingdom, worms_phylum, worms_valid_name) %>%
        dplyr::summarize(
                   n_samples= n()
               ) %>%
        dplyr::group_by(worms_kingdom, sub_kingdom, worms_phylum) %>%
        dplyr::mutate(
                   rank = row_number(desc(n_samples))
               ) %>%
        collect() %>%
        dplyr::filter(worms_phylum %in% c("chordata", "mollusca", "arthropoda", "annelida",
                                          "cnidaria", "echinodermata")) %>%
        dplyr::mutate(n_samp_log = log10(n_samples)) %>%
        ggplot(aes(x = rank, height = n_samples, y= interaction(sub_kingdom, worms_phylum), fill = sub_kingdom)) +
        geom_density_ridges(stat = "identity") + xlim(c(0, 100)) #position = "dodge") #+ ylim(c(0, 750)) + #scale_y_log10() +


    ## violin plot
    idigbio_kingdom_stats("us_idigbio_clean") %>%
        dplyr::group_by(worms_kingdom, sub_kingdom, worms_phylum, worms_valid_name) %>%
        dplyr::summarize(
                   n_samples= n()
               ) %>%
        dplyr::group_by(worms_kingdom, sub_kingdom, worms_phylum) %>%
        dplyr::mutate(
                   rank = row_number(desc(n_samples))
               ) %>%
        collect() %>%
        dplyr::filter(worms_phylum %in% c("chordata", "mollusca", "arthropoda", "annelida",
                                          "cnidaria", "echinodermata")) %>%
        ggplot(aes(x = interaction(sub_kingdom, worms_phylum), fill = worms_phylum, y = n_samples)) +
        geom_point(position = "jitter", alpha = .2) +
        scale_y_log10() + coord_flip() #+ facet_wrap(~ worms_kingdom, scales = "free")

    ## median
     idigbio_kingdom_stats("us_idigbio_clean") %>%
        dplyr::group_by(worms_kingdom, sub_kingdom, worms_phylum, worms_valid_name) %>%
        dplyr::summarize(
                   n_samples= n()
               ) %>%
        dplyr::group_by(worms_kingdom, sub_kingdom, worms_phylum) %>%
        dplyr::mutate(
                   rank = row_number(desc(n_samples))
               ) %>%
        collect() %>%
        dplyr::group_by(worms_kingdom, sub_kingdom, worms_phylum) %>%
        dplyr::summarize(
                   median_n_samp = median(n_samples),
                   mean_n_samp = mean(n_samples)
               ) %>% write_csv("/tmp/summ_samples.csv")



}
