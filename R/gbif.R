fetch_hook_gbif_occ <- function(key, namespace) {
    if (!identical(tolower(key), key))
        stop("key must be lowercase")
    limit <- 2000
    fields <- gbif_fields()
    message("Fetching ", sQuote(key), appendLF = FALSE)
    occ <- rgbif::occ_search(scientificName = key, fields = fields)
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
    stopifnot("worms_valid_name.y" %in% names(wrm) ||
              "worms_valid_name" %in% names(wrm))
    if (exists("worms_valid_name.y", wrm)) {
        colnm <- "worms_valid_name.y"
    } else
        colnm <- "worms_valid_name"
    res <- lapply(wrm[[colnm]], function(x) {
        store_gbif_occ()$get(tolower(x))
    })
    res <- dplyr::bind_rows(lapply(res, function(x) x$data))
    feather::write_feather(res, path = feather_path)
    feather_path
}
