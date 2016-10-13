get_plankton_identifications <- function() {
    res <- labmanager::summary_bold_store(cutoff = 0.95)
    res %>%
        distinct_("sequences", "ID", .keep_all = TRUE) %>%
        mutate(field_phylum = labmanager::get_phylum(sequences),
               esu = labmanager::get_esu_by_voucher_number(sequences))# %>%
}

all_esus <- function(whitney_only = TRUE, phylum = "all") {
    sample_esu <- get_lab("sample_esu")
    if (phylum != "all") {
        sample_esu <- sample_esu[sample_esu$phylum == phylum, ]
    }
    if (whitney_only) {
        sample_data <- get_lab("sample_data")
        sample_esu <- get_lab("station_data") %>%
            filter(latitude_start > 29.6, latitude_start < 29.8,
                   longitude_start > -81.3, longitude_start < -81.2) %>%
            select(station_number) %>%
            left_join(sample_data, by = "station_number") %>%
            select(voucher_number) %>%
            left_join(sample_esu, by = "voucher_number") %>%
            filter(!is.na(voucher_number), !is.na(phylum), !is.na(group_esu),
                   phylum != "Chordata") %>%
            distinct(voucher_number, .keep_all = TRUE)
    }
    unique(paste(sample_esu$phylum, sample_esu$group_esu, sep = "-"))
}

plankton_matches_binomial <- function(pk_id) {
    pk_id %>%
        filter(esu %in% all_esus()) %>%
        ## we focus on matches identified at the species level
        mutate(is_binomial = is_binomial(taxonomicidentification)) %>%
        filter(is_binomial == TRUE)
}

prop_plankton_with_match <- function(pk_id) {
    ## proportion of ESUs that have at least one match in
    ## BOLD/GenBank
    pk_id <- pk_id %>% filter(esu %in% all_esus())
    length(unique(pk_id$esu))/length(all_esus())
}

prop_plankton_with_species_match <- function(pk_id) {
    ## proportion of ESUs that have a species-level match
    pk <- plankton_matches_binomial(pk_id)
    length(unique(pk$esu))/length(all_esus())
}

prop_plankton_with_species_match_by_phylum <- function(pk_id) {
    ## proportion of ESUs that have species-level match by phylum
    ## first let's get the number of esu per phylum with matches
    pk <- plankton_matches_binomial(pk_id) %>%
        group_by(field_phylum) %>%
        summarize(n_esu_match = length(unique(esu)))
    ## then let's make a table of the total number of esu
    esu <- vapply(pk$field_phylum, function(x)
        length(all_esus(phylum = x)),
        numeric(1))
    names(esu) <- pk$field_phylum
    esu <- data.frame(phylum=names(esu), n_esu_sampled = esu,
                      stringsAsFactors = FALSE)
    left_join(pk, esu, by = c("field_phylum" = "phylum")) %>%
        mutate(p_esu_match = n_esu_match/n_esu_sampled)
}


prop_plankton_with_multi_species_match <- function(pk_id) {
    ## proportion of ESUs that match to more than one binomial name
    pk <- plankton_matches_binomial(pk_id) %>%
        group_by(esu) %>%
        summarize(
            n_spp = length(unique(taxonomicidentification)),
            spp = paste(unique(taxonomicidentification), collapse = ", ")
        )
    sum(pk$n_spp ==  1)/length(all_esus())
}
