## key the OBIS occurrences from the AphiaID (worms_id)
fetch_hook_obis_occurrences <- function(key, namespace) {
  if (!is.na(key)) {
    res <- try(robis::occurrence(taxonid = key), silent = TRUE)
  } else {
    return(NULL)
  }
  if (nrow(res) == 0) {
    return(NULL)
  } else if (inherits(res, "try-error")) {
    stop("something's wrong")
  } else {
    v2(" ... ", appendLF = FALSE)
    res
  }
}

store_obis_occurrences <- function(store_path = "data/storr_obis_occurrences") {
  invisible(storr::storr_external(
    storr::driver_rds(store_path),
    fetch_hook_obis_occurrences
  ))
}

fetch_spp_from_obis <- function(wrm, feather_out) {
  stopifnot(inherits(wrm, "data.frame"))
  stopifnot("worms_id" %in% names(wrm))
  w_id <- dplyr::pull(wrm, worms_id)

  res <- vector("list", length(w_id))
  for (i in seq_along(w_id)) {
    v2("getting obis for ", w_id[i], appendLF = FALSE)
    r <- store_obis_occurrences()$get(as.character(w_id[i]))
    r$id <- as.character(r$id)
    r <- sok_as_character(r, c("individualCount", "coordinatePrecision"))
    res[[i]] <- r
    v2(" DONE.")
  }
  res <- dplyr::bind_rows(res)
  names(res) <- tolower(names(res))

  res <- dplyr::rename(res, uuid = id) %>%
    dplyr::distinct(uuid, .keep_all = TRUE) %>%
    dplyr::select(
      uuid, decimallatitude, decimallongitude, depth, phylum,
      family, genus, species, scientificname, year,
      depth, minimumdepthinmeters, maximumdepthinmeters
    )

  if (!is.null(feather_out)) {
    feather::write_feather(res, feather_out)
  } else {
    res
  }
}
