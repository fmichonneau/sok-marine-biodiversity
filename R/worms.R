read_worms_stats <- function(worms_csv) {
  readr::read_csv(worms_csv) %>%
    ## from WoRMS website: http://www.marinespecies.org/aphia.php?p=browser&id[]=2&id[]=1821#focus
    dplyr::bind_rows(tibble::tibble(
      kingdom = "Animalia",
      phylum = "Chordata - inverts",
      all_taxa_marine_non_fossil = NA_integer_,
      all_taxa_species_marine_non_fossil = NA_integer_,
      accepted_species_marine_non_fossil = 3089,
      accepted_species_non_marine_non_fossil = NA_integer_,
      checked_taxa = NA_integer_
    )) %>%
    dplyr::arrange(kingdom, phylum)
}

## Storr for the WoRMS ids: given a taxon name, what is its WoRMS id? ----------
worm_fail <- function(key, reason) {
  assertthat::is.string(key)
  assertthat::is.string(reason)
  v1("failed to find a good candidate for: ", key)
  cat(format(Sys.time()), key, reason, "\n",
    file = "no_match_worms.tsv", sep = "\t", append = TRUE
  )
  setNames(NA_character_, reason)
}

fetch_hook_worms_ids <- function(key, namespace) {
  is_lower_case(key)
  ## if (namespace == "objects")
  ##    stop("namespace must be the phylum name")
  ## is_lower(namespace)
  internal_worms <- function(key, fuzzy = FALSE) {
    worms_one_row <- function(wid, fuzzy) {
      wid$fuzzy <- fuzzy
      if (wid$valid_AphiaID == 0 ||
        is.na(wid$valid_AphiaID)) {
        worm_fail(key, "AphiaID == 0")
      } else {
        return(wid)
      }
    }
    wid <- try(worrms::wm_records_name(key, fuzzy = FALSE),
      silent = TRUE
    )
    ## if no content, we repeat using fuzzy matching
    if (inherits(wid, "try-error") &&
      grepl("no content", wid, ignore.case = TRUE) && !fuzzy) {
      wid <- internal_worms(key, fuzzy = TRUE)
    } else if (inherits(wid, "data.frame")) {
      ## When we get a data frame:
      ## - we check if there is accepted among the names and we choose that
      ## - otherwise we do what we can
      if (nrow(wid) > 1L) {
        ## first try to limit search to only include same phylum as data
        ## wid_sub <- wid %>%
        ##     dplyr::mutate(phylum = tolower(phylum)) %>%
        ##     dplyr::filter(phylum == namespace)
        ## if (nrow(wid_sub) == 1L) {
        ##     return(worms_one_row(wid_sub, fuzzy))
        ## }

        if (any(wid$status %in% c("accepted", "alternate representation"))) {
          wid <- wid %>%
            ## only keep accepted if it's there
            dplyr::filter(status %in% c("accepted", "alternate representation"))
          if (nrow(wid) > 1L) {
            wid <- wid %>%
              ## do not include records not reviewed
              ## (the citation info starts with WoRMS instead of the name of an editor)
              dplyr::filter(!grepl("^WoRMS", citation))
          }
        } else {
          wid <- wid %>%
            dplyr::filter(!status %in% c("deleted", "taxon inquirendum")) %>%
            dplyr::distinct(valid_AphiaID, .keep_all = TRUE)
        }
      }
      if (nrow(wid) == 1L) {
        return(worms_one_row(wid, fuzzy))
      } else {
        worm_fail(key, "multi-match")
      }
    } else {
      worm_fail(key, "no-match")
    }
  }
  internal_worms(key)
}

store_worms_ids <- function(store_path = "data/storr_worms_ids") {
  invisible(storr_external(
    driver_rds(store_path),
    fetch_hook_worms_ids
  ))
}

find_nas <- function() {
  all_keys <- store_worms_ids()$list()
  all_keys %>%
    map_if(
      function(x) is.na(store_worms_ids()$get(x)),
      function(x) x
    )
}

## Storr for WoRMS taxon info from WoRMS id
fetch_hook_worms_info <- function(key, namespace) {
  res <- try(worrms::wm_record(as.integer(key)),
    silent = TRUE
  )
  if (!inherits(res, "try-error") && length(res) > 0L) {
    res
  } else {
    warning("AphiaID ", sQuote(key), " failed.")
    NA
  }
}

store_worms_info <- function(store_path = "data/storr_worms_info") {
  invisible(storr::storr_external(
    storr::driver_rds(store_path),
    fetch_hook_worms_info
  ))
}

## Storr for the WoRMS synonyms: given a WoRMS id, what are the synonyms? ------
fetch_hook_worms_synonyms <- function(key, namespace) {
  res <- try(worrms::wm_synonyms(as.integer(key))$scientificname,
    silent = TRUE
  )
  if (!inherits(res, "try-error") && length(res) > 0L) {
    res
  } else {
    NA
  }
}

store_synonyms <- function(store_path = "data/storr_worms_synonyms") {
  invisible(storr_external(
    driver_rds(store_path),
    fetch_hook_worms_synonyms
  ))
}

## Storr for the higher classification: given a WoRMS id, what is the
## higher classification
fetch_hook_worms_classification <- function(key, namespace) {
  .fetch_hook <- function(key) worrms::wm_classification(as.integer(key))
  res <- try(.fetch_hook(key), silent = TRUE)
  attempts <- 0
  pred <- inherits(res, "try-error")
  while (pred && attempts <= 3) {
    v3("sleeping ... attempt ", attempts)
    Sys.sleep(exp(runif(1) * attempts))
    res <- try(.fetch_hook(key), silent = TRUE)
    pred <- inherits(res, "try-error")
    attempts <- attempts + 1
  }
  if (pred) {
    stop(res)
  } else {
    return(res)
  }
}

store_worms_classification <- function(store_path = "data/storr_worms_classification") {
  invisible(storr_external(
    driver_rds(store_path),
    fetch_hook_worms_classification
  ))
}

worms_phylum_by_wid <- function(wid) {
  vapply(wid, function(x) {
    stopifnot(is.character(x), length(x) == 1L)
    if (is.na(x)) {
      return("")
    }
    tmp_res <- store_worms_classification()$get(x)
    v2(x, " --- ", appendLF = FALSE)
    res <- tmp_res[[1]]
    res <- res$name[res$rank == "Phylum"]
    v2(res)
    as.character(res)
  }, character(1))
}

worms_is_marine <- function(sp) {
  winfo <- store_worms_info()$get(as.character(sp))
  if (inherits(winfo, "logical")) {
    return(NA)
  }
  if (exists("isMarine", winfo)) {
    if (is.null(winfo$isMarine) || is.na(winfo$isMarine)) {
      is_marine <- "0"
    } else {
      is_marine <- winfo$isMarine
    }
    if (is.null(winfo$isBrackish) || is.na(winfo$isBrackish)) {
      is_brackish <- "0"
    } else {
      is_brackish <- winfo$isBrackish
    }

    identical(as.character(is_marine), "1") |
      identical(as.character(is_brackish), "1")
  } else {
    NA
  }
}

add_worms <- function(sp_list, remove_vertebrates = TRUE) {
  stopifnot(inherits(sp_list, "data.frame"))
  stopifnot(all(c("cleaned_scientificname") %in%
    names(sp_list)))

  wrm_names <- names(store_worms_ids()$get("holothuria", namespace = "echinodermata"))
  default_wrms <- set_names(rep(NA, length(wrm_names)), wrm_names)
  default_wrms <- tibble::tibble(!!!default_wrms)

  spp <- sp_list %>%
    dplyr::select(
      matches("^phyum$"),
      cleaned_scientificname
    ) %>%
    dplyr::distinct() %>%
    dplyr::filter(nchar(cleaned_scientificname) > 3)

  wrms <- store_worms_ids()$mget(tolower(spp$cleaned_scientificname)) %>%
    purrr::map_if(is.character, ~default_wrms) %>%
    dplyr::bind_rows() %>%
    dplyr::select(
      worms_id = AphiaID,
      worms_valid_name = valid_name,
      valid_worms_id = valid_AphiaID,
      is_fuzzy = fuzzy,
      rank = rank
    )

  wrms <- dplyr::bind_cols(spp, wrms) %>%
    dplyr::mutate(
      worms_id = as.character(worms_id),
      valid_worms_id = as.character(valid_worms_id)
    ) %>%
    dplyr::mutate(is_marine = map_lgl(valid_worms_id, worms_is_marine)) %>%
    dplyr::filter(!is.na(valid_worms_id)) %>%
    add_classification()

  res <- dplyr::left_join(sp_list, wrms, by = "cleaned_scientificname")

  if (remove_vertebrates) {
    res <- res %>%
      dplyr::filter(!worms_class %in% chordata_classes_to_rm())
  }

  res %>%
    dplyr::filter(!is.na(valid_worms_id))
}

keep_marine_species_only <- function(wrm_tbl) {
  arth_class_to_rm <- tibble::tibble(
    worms_phylum = "arthropoda",
    worms_class = arthropod_classes_to_rm()
  )

  if (inherits(wrm_tbl, "tbl_dbi")) {
    stopifnot(length(setdiff(
      c("rank", "is_marine", "worms_id"),
      colnames(wrm_tbl)
    )) == 0L)
    arth_class_to_rm <- copy_to(sok_db(), arth_class_to_rm,
      overwrite = TRUE,
      temporary = TRUE
    )
  } else {
    stopifnot(exists("rank", wrm_tbl) &&
      exists("is_marine", wrm_tbl) &&
      exists("worms_id", wrm_tbl))
  }

  wrm_tbl %>%
    dplyr::filter(rank == "Species" | rank == "Subspecies") %>%
    dplyr::filter(is_marine) %>%
    dplyr::filter(!is.na(worms_id)) %>%
    ## remove insects and other possibly ambiguous arthropods
    dplyr::anti_join(arth_class_to_rm, by = c("worms_phylum", "worms_class"))
}

add_worms_by_id <- function(tbl, colname = "aphiaid", remove_vertebrates = TRUE) {
  stopifnot(inherits(tbl, "data.frame"))
  stopifnot(exists(colname, tbl))

  colnm <- rlang::sym(colname)

  tbl %>%
    dplyr::mutate(wrm_info = purrr::map(!!colnm, function(.id) {
      message("aphia id: ", .id)
      .info <- store_worms_info()$get(as.character(.id))
      if (inherits(.info, "logical")) {
        tibble::tibble(
          worms_id = NA_character_,
          is_marine = NA_character_,
          worms_valid_name = NA_character_,
          valid_worms_id = NA_character_,
          rank = NA_character_,
          is_fuzzy = NA_character_
        )
      } else {
        tibble::tibble(
          worms_id = as.character(.info$valid_AphiaID) %||% NA,
          is_marine =
            if (is.null(.info$valid_AphiaID)) {
              NA
            } else {
              worms_is_marine(as.character(.info$valid_AphiaID))
            },
          worms_valid_name = .info$valid_name %||% NA,
          valid_worms_id = as.character(.info$valid_AphiaID) %||% NA,
          rank = .info$rank %||% NA,
          is_fuzzy = .info$match_type %||% NA
        )
      }
    })) %>%
    tidyr::unnest(wrm_info) %>%
    dplyr::filter(is_marine) %>%
    dplyr::filter(!is.na(worms_id)) %>%
    add_classification()
}
