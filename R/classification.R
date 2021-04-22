get_classification_from_wid <- function(wid) {
  wid <- as.character(wid)
  if (is.na(wid) || identical(wid, "0") || identical(wid, "1")) {
    return(
      tibble(
        worms_phylum = NA_character_,
        worms_class = NA_character_,
        worms_order = NA_character_,
        worms_family = NA_character_
      )
    )
  }
  v2("getting classification for aphiaid ", wid)
  classif <- store_worms_classification()$get(wid)

  empty_classif <- tibble(
    phylum = character(0),
    class = character(0),
    order = character(0),
    family = character(0)
  )
  names(empty_classif) <- paste0("worms_", names(empty_classif))

  classif <- classif %>%
    dplyr::mutate(
      rank = paste0("worms_", tolower(rank)),
      scientificname = tolower(scientificname)
    )

  to_keep <- intersect(classif$rank, names(empty_classif))

  classif %>%
    dplyr::distinct(rank, .keep_all = TRUE) %>%
    dplyr::select(-AphiaID) %>%
    tidyr::spread(rank, scientificname) %>%
    dplyr::select_(.dots = to_keep) %>%
    dplyr::bind_rows(empty_classif)
}


add_classification <- function(data) {
  stopifnot(exists("worms_id", data))

  wrms_tbl <- "worms_classification"
  wrms_types <- structure(c(
    "INT", "TEXT", "TEXT", "TEXT", "TEXT"
  ),
  .Names = c(
    "valid_worms_id", "worms_phylum", "worms_class",
    "worms_order", "worms_family"
  )
  )

  if (!DBI::dbExistsTable(sok_db(), wrms_tbl)) {
    DBI::dbCreateTable(
      sok_db(),
      wrms_tbl,
      fields = wrms_types,
      temporary = FALSE
    )
    dbExecute(
      sok_db(),
      paste("CREATE UNIQUE INDEX wrms_idx ON", wrms_tbl, "(valid_worms_id)")
    )
    DBI::dbCommit(sok_db())
  }

  wid_tbl <- tbl(sok_db(), wrms_tbl)

  wid_in_db <- wid_tbl %>%
    dplyr::distinct(valid_worms_id) %>%
    dplyr::collect(n = Inf)

  wid_to_match <- unique(na.omit(data$worms_id))
  wid_to_classify <- tibble(valid_worms_id = setdiff(wid_to_match, wid_in_db$valid_worms_id)) %>%
    filter(!is.na(valid_worms_id))

  if (nrow(wid_to_classify) > 0) {
    to_add_to_db <- wid_to_classify %>%
      dplyr::mutate(classif = purrr::map(
        valid_worms_id,
        get_classification_from_wid
      )) %>%
      tidyr::unnest(classif)
    to_add_to_db$valid_worms_id <- as.integer(to_add_to_db$valid_worms_id)
    dbWriteTable(sok_db(), wrms_tbl, to_add_to_db,
      append = TRUE,
      field.types = wrms_types, row.names = FALSE
    )
  }

  wid_tbl_mem <- tbl(sok_db(), wrms_tbl) %>%
    dplyr::filter(valid_worms_id %in% wid_to_match) %>%
    dplyr::collect(n = Inf) %>%
    dplyr::mutate(valid_worms_id = as.character(valid_worms_id))

  res <- data %>%
    dplyr::left_join(wid_tbl_mem, by = "valid_worms_id")

  ## for Kozloff's list, that's how we're getting the phylum info,
  ## so we need to add a `phylum` column that will be used
  ## downstream.
  if (!exists("phylum", res)) {
    res <- res %>%
      dplyr::mutate(phylum = worms_phylum)
  }
  res
}

add_kingdom <- function(wid) {
  wid <- as.character(wid)
  res <- store_worms_classification()$mget(wid)
  vapply(res, function(x) {
    ## should only be for "Biota" wid == 1
    if (!exists("scientificname", x)) {
      return(NA_character_)
    }
    dplyr::pull(x, scientificname)[1] %>% tolower()
  }, character(1))
}
