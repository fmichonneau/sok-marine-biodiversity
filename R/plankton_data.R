get_plankton_identifications <- function() {
    res <- labmanager::summary_bold_store(cutoff = 0.95)
    res %>%
        distinct_("sequences", "ID", .keep_all = TRUE) %>%
        mutate(field_phylum = labmanager::get_phylum(sequences),
               esu = labmanager::get_esu_by_voucher_number(sequences),
               geo_dist = distance_from_whitney(specimen_lat, specimen_lon))# %>%
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
    sample_esu
}

all_esus <- function(whitney_only = TRUE, phylum = "all") {
    sample_esu <- all_voucher_numbers(whitney_only, phylum)
    unique(paste(sample_esu$phylum, sample_esu$group_esu, sep = "-"))
}

calc_plankton_matches_binomial <- function(pk_id) {
    pk_id %>%
        ## we focus on matches identified at the species level
        mutate(is_binomial = is_binomial(taxonomicidentification)) %>%
        filter(is_binomial == TRUE)
}

calc_prop_plankton_with_match <- function(pk_id) {
    ## proportion of ESUs that have at least one match in
    ## BOLD/GenBank
    length(unique(pk_id$esu))/length(all_esus())
}

calc_prop_plankton_with_species_match <- function(pk_id) {
    ## proportion of ESUs that have a species-level match
    pk <- calc_plankton_matches_binomial(pk_id)
    length(unique(pk$esu))/length(all_esus())
}

calc_prop_plankton_with_species_match_by_phylum <- function(pk_id) {
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


distance_from_whitney <- function(lat, lon) {
    mapply(function(.lat, .lon) {
        if (is.na(.lat) || is.na(.lon)) return(NA)
        gcd.hf(deg2rad(as.numeric(.lat)), deg2rad(as.numeric(.lon)),
               deg2rad(29.669), deg2rad(-81.216), warn = FALSE)
    }, lat, lon)
}

plankton_geo_distances <- function(pk_id) {
    pk_id <- pk_id %>%
        group_by(field_phylum, esu) %>%
        summarize(
            closest_match = min(geo_dist, na.rm = TRUE)
        )
    pk_id
}

calc_n_larvae <- function(...) {
    nrow(all_voucher_numbers(...))
}

calc_n_esus <- function(...) {
    length(all_esus(...))
}
