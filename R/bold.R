fetch_hook_bold_specimens_per_species <- function(key, namespace) {
    if (is.na(key)) return(NULL)
    is_lower_case(key)
    res <- try(bold_specimens(taxon = paste0("'", key, "'")), silent = TRUE)
    if (inherits(res, "try-error")) {
        message("No record for ", key, ". Trying to look for synonyms ...")
        wid <- store_worms_ids()$get(key)
        if (!inherits(wid, "data.frame")) {
            message("  no valid WoRMS ID.")
            return("not in worms/multi match")
        } else {
            syn <- store_synonyms()$get(as.character(wid$valid_AphiaID))
            if (inherits(syn, "data.frame")) {
                res <- try(bold_specimens(taxon = paste0("'", syn, "'", collapse = "|")),
                           silent = TRUE)
                if (inherits(res, "try-error")) {
                    message("  No record for any of the synonyms ",
                            paste("   - ", syn, collapse = " \n"))
                    return(NULL)
                }
                message(" found ", nrow(res), " records for synonyms")
            } else {
                message(" No synonyms")
                return(NULL)
            }
        }
    }
    message(nrow(res), " record(s) for ", key)
    res
}

store_bold_specimens_per_species <- function(store_path = "data/bold_specimens_per_species") {
    storr_external(driver_rds(store_path),
                   fetch_hook_bold_specimens_per_species)
}

## this function takes a data frame, for which the column named with
## the content of `col_nm` contains the species names to look up in
## BOLD. It returns a data frame.
internal_add_bold <- function(res, col_nm, show_progress = TRUE) {
    bold_rcrd <- bold_bin <- n_country <- n_coords <- numeric(nrow(res))

    if (show_progress) {
        to_find <- !store_bold_specimens_per_species()$exists(tolower(res[[col_nm]]))
        if (sum(to_find) > 0)
            pb <- progress::progress_bar$new(total = sum(to_find))
    }

    for (i in seq_len(nrow(res))) {
        if ((!store_bold_specimens_per_species()$exists(tolower(res[[col_nm]][i]))) &&
            show_progress) pb$tick()
        bold <- store_bold_specimens_per_species()$get(tolower(res[[col_nm]][i]))
        bold_rcrd[i] <- ifelse(is.null(bold) || inherits(bold, "character"),
                               0, nrow(bold))
        bold_bin[i] <- ifelse(is.null(bold) || inherits(bold, "character"),
                              0, length(unique(na.omit(bold$bin_uri))))
        n_country[i] <- ifelse(is.null(bold) || inherits(bold, "character"),
                               0, sum(!is.na(bold$country)))
        n_coords[i] <-  ifelse(is.null(bold) || inherits(bold, "character"),
                               0, sum((!is.na(bold$lat)) & (!is.na(bold$lon))))
    }
    res$n_bold_records <- bold_rcrd
    res$n_bins <- bold_bin
    res$n_country <- n_country
    res$n_coords <- n_coords

    res
}

preprocess_bold <- function(worms, use_worms) {
    if (use_worms) {
        res <- keep_marine_taxa(worms)
    } else {
        res <- worms %>%
            dplyr::filter(is_binomial == TRUE) %>%
            dplyr::disctint(cleaned_scientificname, .keep_all = TRUE)
    }
}

find_bold_records <- function(worms, use_worms = TRUE) {

    res <- preprocess_bold(worms, use_worms)

    if (use_worms)
        col_nm <- "worms_valid_name"
    else {
        col_nm <- "cleaned_scientificname"
    }

    internal_add_bold(res, col_nm) %>%
        dplyr::filter_(.dots = lazyeval::interp(~ !is.na(var.), var. = as.name(col_nm)))
}


make_stat_bold <- function(gom_bld, koz_bld, gom_wrm, koz_wrm) {
    res <- dplyr::bind_rows(gom_bld, koz_bld) %>%
        dplyr::distinct(worms_valid_name, .keep_all = TRUE)

    bin_data <- get_bold_bins(gom_wrm, koz_wrm)

    ## BINs that are found in more than 1 species
    n_shared_bins <- bin_data %>%
        dplyr::group_by(bins) %>%
        dplyr::summarize(
            n_shared_bins = n_distinct(worms_valid_name)
        ) %>%
        dplyr::filter(n_shared_bins > 1) %>%
        nrow()

    ## Species that have more than 1 BIN associated with their name
    ## -- we need to remove species that have only 1 record
    multi_bin <- bin_data %>%
        dplyr::group_by(worms_valid_name) %>%
        dplyr::summarize(
            n_spp_multi_bin = n_distinct(bins)
            ) %>%
        dplyr::left_join(dplyr::select(res, worms_valid_name, n_bold_records),
                         by = "worms_valid_name") %>%
        dplyr::filter(n_bold_records > 1)

    n_multi_bin <- multi_bin %>%
        dplyr::filter(n_spp_multi_bin > 1) %>%
        nrow()
    p_multi_bin <- n_multi_bin/nrow(multi_bin)

    list(
        p_no_country = 1 - (sum(res$n_country)/sum(res$n_bold_records)),
        p_no_coords = 1 - (sum(res$n_coords)/sum(res$n_bold_records)),
        n_total_records = sum(res$n_bold_records),
        n_shared_bins = n_shared_bins,
        n_spp_multi_bin = n_multi_bin,
        p_spp_multi_bin = p_multi_bin,
        n_spp_not_singleton = nrow(multi_bin),
        n_bins = length(unique(bin_data$bins)),
        n_spp = length(unique(bin_data$worms_valid_name))
    )
}

get_bold_bins <- function(gom, koz) {
    spp <- dplyr::bind_rows(gom, koz) %>%
        dplyr::filter(is_marine == TRUE, !is.na(worms_id)) %>%
        dplyr::distinct(worms_valid_name)

    res <- lapply(spp$worms_valid_name, function(x) {
        bd <- store_bold_specimens_per_species()$get(tolower(x))
        if ((!is.null(bd)) && (!inherits(bd, "character")) && nrow(bd) > 0) {
            bins <- unique(bd$bin_uri)
            data.frame(worms_valid_name = rep(x, length(bins)),
                       bins = bins, stringsAsFactors = FALSE)
        } else
            NULL
    })

    dplyr::bind_rows(res) %>%
        dplyr::filter(nzchar(gsub("\\s+", "", bins)))

}


bold_status <- function(idig) {
    idig %>%
        dplyr::group_by(phylum) %>%
        dplyr::summarize(
                   p_has_bold = mean(n_bold_records > 0),
                   n_has_bold = sum(n_bold_records > 0)
        )
}

bold_status_data <- function(gom_bold, koz_bold, idig_bold) {
    dplyr::bind_rows(
               gom = bold_status(gom_bold),
               koz = bold_status(koz_bold),
               all_idigbio = bold_status(idig_bold),
               .id = "data_source"
           ) %>%
    dplyr::filter(
               phylum %in% c("annelida", "arthropoda",
                             "bryozoa",
                             "chordata", "cnidaria",
                             "echinodermata", "mollusca",
                             "porifera"
                             )
           ) %>%
    dplyr::mutate(phylum = capitalize(phylum))
}

plot_bold_status <- function(bold_data) {
    bold_data %>%
        ggplot(aes(x = reorder(phylum, p_has_bold), y = p_has_bold,
                   fill = data_source)) +
        geom_col(position = "dodge") +
        geom_text(aes(y = p_has_bold +.01, label = n_has_bold), position = position_dodge(.9),
                  hjust = .1, family = "Ubuntu Condensed")  +
        xlab("") + ylab("Proportion of species with available DNA barcodes") +
        scale_fill_viridis(discrete = TRUE,
                           name = "",
                           labels = c("US EEZ", "Gulf of Mexico", "Pacific Northwest")) +
        theme_ipsum(base_family = "Ubuntu Condensed") +
        coord_flip()
}

make_stat_barcoding <- function(bold_data) {
    list(
        min_eez_phylum = bold_data %>%
            group_by(data_source) %>%
            filter(data_source == "all_idigbio" &
                   p_has_bold == min(p_has_bold)) %>%
            pull(phylum),
        min_eez_value = bold_data %>%
            group_by(data_source) %>%
            filter(data_source == "all_idigbio" &
                   p_has_bold == min(p_has_bold)) %>%
            pull(p_has_bold),
        max_eez_phylum = bold_data %>%
            group_by(data_source) %>%
            filter(data_source == "all_idigbio" &
                   p_has_bold == max(p_has_bold)) %>%
            pull(phylum),
        max_eez_value = bold_data %>%
            group_by(data_source) %>%
            filter(data_source == "all_idigbio" &
                   p_has_bold == max(p_has_bold)) %>%
            pull(p_has_bold)

    )
}


project_contribution <- function(db, use_worms, col_nm) {

    db <- preprocess_bold(db, use_worms)


    res <- as.list(unique(db[[col_nm]])) %>%
        map_df(function(x) {
            .r <- store_bold_specimens_per_species()$get(tolower(x))
            if (is.null(.r) || inherits(.r, "character"))
                tibble(species_name = character(0),
                       projects = character(0),
                       n =  integer(0))
            else {
                .r <- .r %>%
                    filter(!grepl("Mined from GenBank", institution_storing))

                ## we use the prefix of the processid in BOLD to
                ## figure out the contribution of each project to the
                ## total number of barcodes.
                tibble(
                        projects = gsub("([A-Z]{3,5}).+", "\\1", .r$processid)
                    ) %>%
                    dplyr::count(projects) %>%
                    dplyr::bind_cols(tibble(species_name = rep(x, nrow(.)))) %>%
                    dplyr::select(species_name, projects, n)

            }
        })
    res
}
