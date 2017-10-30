read_worms_stats <- function(worms_csv) {
    wrms_stats <- readr::read_csv(worms_csv)
    ## use Appletans 2012 number (removing fishes and other vertebrates)
    wrms_stats$accepted_species_marine_non_fossil[wrms_stats$phylum == "Chordata"] <- 3053
    wrms_stats
}

## Storr for the WoRMS ids: given a taxon name, what is its WoRMS id? ----------
worm_fail <- function(key, reason) {
    assertthat::is.string(key)
    assertthat::is.string(reason)
    v1("failed to find a good candidate for: ", key)
    cat(format(Sys.time()), key, reason, "\n",
        file = "no_match_worms.tsv", sep = "\t", append = TRUE)
    setNames(NA_character_, reason)

}

fetch_hook_worms_ids <- function(key, namespace) {
    is_lower_case(key)
    internal_worms <- function(key, fuzzy = FALSE) {
        wid <- try(worrms::wm_records_name(key, fuzzy = FALSE),
                   silent = TRUE)
        ## if no content, we repeat using fuzzy matching
        if (inherits(wid, "try-error") &&
            grepl("no content", wid, ignore.case = TRUE) && !fuzzy) {
            wid <- internal_worms(key, fuzzy = TRUE)
        } else if (inherits(wid, "data.frame")) {
            ## When we get a data frame:
            ## - we check if there is accepted among the names and we choose that
            ## - otherwise
            if (nrow(wid) > 1L) {
                if (any(wid$status %in% c("accepted", "alternate representation"))) {
                    wid <- wid %>%
                        ## only keep accepted if it's there
                        dplyr::filter(status %in%  c("accepted", "alternate representation"))
                    if (nrow(wid) > 1L) {
                        wid <- wid %>%
                            ## do not include records not reviewed
                            ## (the citation info starts with WoRMS instead of the name of an editor)
                            dplyr::filter(!grepl("^WoRMS", citation))
                    }
                } else {
                    wid <- wid %>%
                        dplyr::filter(! status %in% c("deleted", "taxon inquirendum")) %>%
                        dplyr::distinct(valid_AphiaID, .keep_all = TRUE)
                }
            }
            if (nrow(wid) == 1L) {
                wid$fuzzy <- fuzzy
                if (wid$valid_AphiaID == 0 ||
                    is.na(wid$valid_AphiaID)) {
                    worm_fail(key, "AphiaID == 0")
                } else return(wid)
            } else {
                worm_fail(key, "multi-match")
            }
        } else worm_fail(key, "no-match")
    }
    internal_worms(key)
}

store_worms_ids <- function(store_path = "data/storr_worms_ids") {
    invisible(storr_external(driver_rds(store_path),
                             fetch_hook_worms_ids))
}

find_nas <- function() {
    all_keys <- store_worms_ids()$list()
    all_keys %>%
        map_if(function(x) is.na(store_worms_ids()$get(x)),
               function(x) x)
}

## Storr for WoRMS taxon info from WoRMS id
fetch_hook_worms_info <- function(key, namespace) {
    res <- try(worrms::wm_record(as.integer(key)),
               silent = TRUE)
    if (!inherits(res, "try-error") && length(res) > 0L) {
        res
    } else {
        warning("AphiaID ", sQuote(key), " failed.")
        NA
    }
}

store_worms_info <- function(store_path = "data/storr_worms_info") {
    invisible(storr::storr_external(storr::driver_rds(store_path),
                                    fetch_hook_worms_info))
}

## Storr for the WoRMS synonyms: given a WoRMS id, what are the synonyms? ------
fetch_hook_worms_synonyms <- function(key, namespace) {
    res <- try(worrms::wm_synonyms(as.integer(key))$scientificname,
               silent = TRUE)
    if (!inherits(res, "try-error") && length(res) > 0L) {
        res
    } else NA
}

store_synonyms <- function(store_path = "data/storr_worms_synonyms") {
    invisible(storr_external(driver_rds(store_path),
                             fetch_hook_worms_synonyms))
}

## Storr for the higher classification: given a WoRMS id, what is the
## higher classification
fetch_hook_worms_classification <- function(key, namespace) {
    worrms::wm_classification(as.integer(key))
}

store_worms_classification <- function(store_path = "data/storr_worms_classification") {
    invisible(storr_external(driver_rds(store_path),
                             fetch_hook_worms_classification))
}

worms_phylum_by_wid <- function(wid) {
    vapply(wid, function(x) {
        stopifnot(is.character(x), length(x) == 1L)
        if (is.na(x)) return("")
        tmp_res <- store_worms_classification()$get(x)
        v2(x, " --- ", appendLF = FALSE)
        res <- tmp_res[[1]]
        res <- res$name[res$rank == "Phylum"]
        v2(res)
        as.character(res)
    },  character(1))
}

worms_is_marine <- function(sp) {
    winfo <- store_worms_info()$get(sp)
    if (exists("isMarine", winfo)) {
        if ((is.null(winfo$isMarine) && is.null(winfo$isBrackish)) ||
            (is.na(winfo$isMarine) && is.na(winfo$isBrackish)))
            NA
        else
            identical(as.character(winfo$isMarine), "1") |
                identical(as.character(winfo$isBrackish), "1")
    } else NA
}

add_worms <- function(sp_list) {
    stopifnot(inherits(sp_list, "data.frame"))
    stopifnot(all(c("cleaned_scientificname") %in%
                  names(sp_list)))

    spp <- sp_list %>%
        dplyr::distinct(cleaned_scientificname) %>%
        dplyr::filter(nchar(cleaned_scientificname) > 3)

    wid <- valid_name <- rank <- is_fuzzy <- character(nrow(spp))
    marine <- logical(nrow(spp))

    for (i in seq_len(nrow(spp))) {
        w_info <- store_worms_ids()$get(tolower(spp[i, 1]))
        if (inherits(w_info, "data.frame")) {
            wid[i] <- w_info$valid_AphiaID
            valid_name[i] <- w_info$valid_name
            is_fuzzy[i] <- w_info$fuzzy
            rank[i] <- w_info$rank
            # use the valid name to infer marine or not, as it is not score
            marine[i] <- worms_is_marine(as.character(w_info$valid_AphiaID))
        } else {
            wid[i] <- marine[i] <- valid_name[i] <- is_fuzzy[i] <- rank[i] <- NA
        }
    }
    to_add <- data_frame(
        cleaned_scientificname = dplyr::pull(spp, "cleaned_scientificname"),
        worms_id = wid,
        is_marine = marine,
        worms_valid_name = valid_name,
        rank = rank,
        is_fuzzy = is_fuzzy
    )

    dplyr::left_join(sp_list, to_add, by = "cleaned_scientificname") %>%
        add_classification() %>%
        dplyr::filter(!worms_class %in% chordata_classes_to_rm()) %>%
        dplyr::filter(rank == "Species" | rank == "Subspecies") %>%
        dplyr::filter(is_marine) %>%
        dplyr::filter(!is.na(worms_id))
}
