fetch_hook_gbif_occ <- function(key, namespace) {
    is_lower_case(key)
    fields <- gbif_fields()
    message("Fetching ", sQuote(key), appendLF = FALSE)
    occ <- rgbif::occ_search(scientificName = key, fields = fields, limit = 5000)
    message(" ... DONE (", nrow(occ$data), "/", occ$meta$count, ")")
    return(occ)
}

store_gbif_occ <- function(store_path = "data/gbif_occ_storr") {
    invisible(storr::storr_external(driver_rds(store_path),
                                    fetch_hook_gbif_occ))
}

gbif_fields <- function() {
    c("key", "decimalLatitude", "decimalLongitude", "issues",
      "protocol", "basisOfRecord", "scientificName", "species",
      "taxonRank", "year", "month", "day", "identifier",
      "institutionCode")
}


fetch_spp_from_gbif <- function(wrm, feather_path) {
    stopifnot(inherits(wrm, "data.frame"))
    stopifnot("worms_valid_name" %in% names(wrm))
    wrm_names <- wrm[["worms_valid_name"]]
    res <- lapply(wrm_names[!duplicated(wrm_names)],
                  function(x) {
        store_gbif_occ()$get(tolower(x))
    })

    res <- dplyr::bind_rows(lapply(res, function(x) {
                      ## remove issues
                      rr <- rgbif::occ_issues(x,
                                              -preneglat,    # presumed negated latitude
                                              -preneglon,    # presumed negated longitude
                                              -preswcd,      # presumed swapped coordinates
                                              -rdatunl)$data # unlikely date (future or way past)
                      if (is.null(rr)) return(NULL)
                      rr
                  }))
    names(res) <- tolower(names(res))
    res <- res %>%
        dplyr::rename(scientificname_authority = scientificname,
                      scientificname = species,
                      uuid = key) %>%
        filter(basisofrecord != "FOSSIL_OBSERVATION") %>%
        ## for some reason, there are duplicated records in GBIF data
        dplyr::distinct(uuid, .keep_all = TRUE)
    feather::write_feather(res, path = feather_path)
}
