fetch_hook_bold_specimens_per_species <- function(key, namespace) {
    if (is.na(key)) return(NULL)
    is_lower_case(key)
    res <- try(bold_specimens(taxon = paste0("'", key, "'")), silent = TRUE)
    if (inherits(res, "try-error")) {
        message("No record for ", key, ". Trying to look for synonyms ...", appendLF = FALSE)
        wid <- store_worms_ids()$get(key)
        if (is.na(wid)) {
            message("Can't find a valid WoRMS ID")
            return("not in worms/multi match")
        } else {
            syn <- store_synonyms()$get(wid)
            res <- try(bold_specimens(taxon = paste0("'", syn, "'", collapse = "|")),
                       silent = TRUE)
            if (inherits(res, "try-error")) {
                message("No record for any of the synonyms ",
                        paste(syn, collapse = ", "))
                return(NULL)
            }
            message(" found ", nrow(res), " records for synonyms")
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
    bold_rcrd <- bold_bin <- numeric(nrow(res))

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
    }
    res$n_bold_records <- bold_rcrd
    res$n_bins <- bold_bin
    res
}

find_bold_records <- function(worms, use_worms = TRUE) {
    if (use_worms) {
        res <- keep_marine_taxa(worms)
        col_nm <- "worms_valid_name"
    } else {
        res <- worms %>%
            dplyr::filter(is_binomial == TRUE) %>%
            dplyr::disctint(cleaned_scientificname, .keep_all = TRUE)
        col_nm <- "cleaned_scientificname"
    }

    internal_add_bold(res, col_nm) %>%
        dplyr::filter_(.dots = lazyeval::interp(~ !is.na(var.), var. = as.name(col_nm)))
}
