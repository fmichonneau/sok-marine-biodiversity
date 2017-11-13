
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

prepare_idig_stats_by_kingdom <- function(db_table) {

    db <- sok_db()

    ## clean up
    chordata_classes <- chordata_classes_to_rm()[-match("unknown", chordata_classes_to_rm())]
    chordata_classes <- glue("(", collapse(paste0("'", chordata_classes, "'"), sep=", "), ")")
    chordata_families <- glue("(", collapse(paste0("'", chordata_families_to_rm(), "'"), sep = ", "), ")")

    ## Drop table if it already exists
    if (db_has_table(db, glue::glue("{db_table}_clean")))
        db_drop_table(db, glue::glue("{db_table}_clean"))

    ## create tables to infer higher classification and whether species are marine
    dbExecute(db, glue::glue("CREATE TABLE {db_table}_clean AS ",
                             "SELECT DISTINCT ON (uuid) * FROM {db_table} ",
                             "WHERE within_eez IS TRUE;"))
    dbExecute(db, glue::glue("ALTER TABLE {db_table}_clean ",
                             "ADD PRIMARY KEY (uuid);"))
    dbExecute(db, glue::glue("CREATE INDEX ON {db_table}_clean (scientificname)"))
    dbExecute(db, glue::glue("UPDATE {db_table}_clean ",
                             "SET phylum = 'chordata' ",
                             "WHERE phylum IS NULL AND (",
                             "class IN {chordata_classes} OR ",
                             "family IN {chordata_families})"))

    ## get all species names, clean them up, and get worms info
    q <- dbSendQuery(db, glue::glue("SELECT DISTINCT scientificname FROM {db_table}_clean"))
    dbFetch(q) %>%
        all_idigbio_species_name_cleanup() %>%
        dplyr::distinct(cleaned_scientificname, .keep_all = TRUE) %>%
        dplyr::filter(grepl("\\s", cleaned_scientificname)) %>%
        dplyr::mutate(is_binomial = is_binomial(cleaned_scientificname)) %>%
        add_worms(remove_vertebrates = FALSE)  %>%
        dplyr::filter(rank == "Species" | rank == "Subspecies") %>%
        dplyr::filter(!is.na(rank)) %>%
        dplyr::mutate(worms_kingdom = add_kingdom(worms_id)) %>%
        dplyr::copy_to(db, ., name = glue::glue("{db_table}_species"), temporary = FALSE,
                       overwrite = TRUE, indexes = list("scientificname"))

    ## worms info to idigbio records
    purrr::map(list("worms_kingdom", "worms_phylum", "worms_class",
             "worms_order", "worms_family", "worms_valid_name", "worms_id", "rank"),
        function(x) dbExecute(db, glue::glue("ALTER TABLE {db_table}_clean ADD COLUMN {x} TEXT DEFAULT NULL;"))
        )
    dbExecute(db, glue::glue("ALTER TABLE {db_table}_clean ADD COLUMN is_marine BOOL DEFAULT NULL;"))
    dbExecute(db, glue::glue("UPDATE {db_table}_clean ",
                             "SET (worms_valid_name, worms_id, is_marine, rank, worms_kingdom, worms_phylum,",
                             "     worms_class, worms_order, worms_family) = ",
                             "(SELECT worms_valid_name, worms_id, is_marine, rank, worms_kingdom, ",
                             "        worms_phylum, worms_class, worms_order, worms_family ",
                             " FROM {db_table}_species ",
                             "  WHERE {db_table}_clean.scientificname = {db_table}_species.scientificname);"))
}

prepare_obis_stats_by_kingdom <- function(db_table) {
    db <- sok_db()

    if (db_has_table(db, glue::glue("{db_table}_clean")))
        db_drop_table(db, glue::glue("{db_table}_clean"))

    ## obis data is cleaner than iDigBio and already has worms_id (aphiaid) as a
    ## column. This aphiaID may not be the one for the accepted name though.
    v3(glue::glue("Create {db_table}_clean ... "),  appendLF = FALSE)
    dbExecute(db, glue::glue("CREATE TABLE {db_table}_clean AS ",
                             "SELECT DISTINCT ON (uuid) * FROM {db_table} ",
                             "WHERE within_eez IS TRUE;"))
    dbExecute(db, glue::glue("CREATE INDEX ON {db_table}_clean (aphiaid); "))
    v3("DONE.")

    ## get worms info from aphiaid
    v3(glue::glue("Create {db_table}_species ... "), appendLF = FALSE)
    dbSendQuery(db, glue::glue("SELECT DISTINCT aphiaid FROM {db_table}_clean;")) %>%
        dbFetch() %>%
        dplyr::filter(!is.na(aphiaid)) %>%
        add_worms_by_id(remove_vertebrates = FALSE) %>%
        dplyr::filter(rank == "Species" | rank == "Subspecies") %>%
        dplyr::filter(!is.na(rank)) %>%
        dplyr::mutate(worms_kingdom = add_kingdom(worms_id))  %>%
        dplyr::copy_to(db, ., name = glue::glue("{db_table}_species"), temporary = FALSE,
                       overwrite = TRUE, indexes = list("aphiaid"))
    v3("DONE.")

    ## worms info to idigbio records
    v3(glue::glue("Adding worms info to {db_table}_clean ..."), appendLF = FALSE)
    purrr::map(list("worms_kingdom", "worms_phylum", "worms_class",
             "worms_order", "worms_family", "worms_valid_name", "worms_id", "rank"),
        function(x) dbExecute(db, glue::glue("ALTER TABLE {db_table}_clean ADD COLUMN {x} TEXT DEFAULT NULL;"))
        )
    dbExecute(db, glue::glue("ALTER TABLE {db_table}_clean ADD COLUMN is_marine BOOL DEFAULT NULL;"))
    dbExecute(db, glue::glue("UPDATE {db_table}_clean ",
                             "SET (worms_valid_name, worms_id, is_marine, rank, worms_kingdom, worms_phylum,",
                             "     worms_class, worms_order, worms_family) = ",
                             "(SELECT worms_valid_name, worms_id, is_marine, rank, worms_kingdom, ",
                             "        worms_phylum, worms_class, worms_order, worms_family ",
                             " FROM {db_table}_species ",
                             "  WHERE {db_table}_clean.aphiaid = {db_table}_species.aphiaid);"))
    v3("DONE.")

}

add_sub_kingdom <- function(db_table) {
    db <- sok_db()

    tbl <- tbl(db, db_table)

    tbl %>%
        dplyr::filter(is_marine) %>%
        dplyr::filter(!is.na(worms_id)) %>%
        dplyr::mutate(sub_kingdom = case_when(
                          worms_phylum == "chordata" &
                          worms_class %in% c("appendicularia",
                                             "ascidiacea",
                                             "holocephali",
                                             "leptocardii",
                                             "thaliacea") ~ "animalia",
                          worms_phylum == "chordata" &
                          worms_class %in% c("actinopterygii",
                                             "aves",
                                             "elasmobranchii",
                                             "mammalia",
                                             "myxini",
                                             "petromyzonti",
                                             "reptilia") ~ "animalia - vertebrates",
                          worms_kingdom == "animalia" ~ "animalia",
                          worms_kingdom == "chromista" ~ "chromista",
                          worms_kingdom == "plantae" ~ "plantae",
                          TRUE ~ "others"
                      ))

}

calc_kingdom_diversity <- function(worms_stats) {

    ## retrieved from http://www.marinespecies.org/aphia.php?p=browser&id=1821#ct
    n_vertebrates <- 19741
    get_n_spp <- . %>%
        dplyr::group_by(sub_kingdom) %>%
        dplyr::summarize(
                   n_samples = n(),
                   n_spp = n_distinct(worms_valid_name)
        ) %>%
        dplyr::collect()

    wrm <- worms_stats %>%
        dplyr::mutate(sub_kingdom = case_when(
                          tolower(kingdom) %in% c("fungi", "protozoa", "bacteria",
                                                  "archaea", "viruses", "biota incertae sedis") ~ "others",
                          TRUE ~ tolower(kingdom)
                      )) %>%
        dplyr::group_by(sub_kingdom) %>%
        dplyr::summarize(
                   n_spp = sum(all_species_marine_non_fossil)
               ) %>%
        dplyr::bind_rows(
            data_frame(
                sub_kingdom =  "animalia - vertebrates",
                n_spp = n_vertebrates)
        )
    wrm[wrm$sub_kingdom == "animalia", "n_spp"] <- wrm[wrm$sub_kingdom == "animalia", "n_spp"] - n_vertebrates

    ## diversity comparison
    bind_rows(
        idigbio = add_sub_kingdom("us_idigbio_clean") %>%
            get_n_spp(),
        obis =  add_sub_kingdom("us_obis_clean") %>%
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
        idigbio = add_sub_kingdom("us_idigbio_clean") %>%
            get_median(),
        obis =  add_sub_kingdom("us_obis_clean") %>%
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
