fetch_hook_gbif_occ <- function(key, namespace) {
    if (!identical(tolower(key), key))
        stop("key must be lowercase")
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
    stopifnot("worms_valid_name.y" %in% names(wrm) ||
              "worms_valid_name" %in% names(wrm))
    if (exists("worms_valid_name.y", wrm)) {
        colnm <- "worms_valid_name.y"
    } else
        colnm <- "worms_valid_name"
    res <- lapply(wrm[[colnm]], function(x) {
        store_gbif_occ()$get(tolower(x))
    })
    res <- dplyr::bind_rows(lapply(res, function(x) {
                      ## remove issues
                      rgbif::occ_issues(x,
                                        -preneglat,    # presumed negated latitude
                                        -preneglon,    # presumed negated longitude
                                        -preswcd,      # presumed swapped coordinates
                                        -rdatunl)$data # unlikely date (future or way past)
                  }))
    feather::write_feather(res, path = feather_path)
}


filter_gbif <- function(feather_path) {
    gb <- feather::read_feather(feather_path)
    gb %>%
        filter(basisOfRecord != "FOSSIL_OBSERVATION",
               !is.na(decimalLatitude) | !is.na(decimalLongitude))
}

us_gbif <- function(gbif_filtered_data) {
    res_lawn <- gbif_filtered_data %>%
        dplyr::select(uuid = key,
               lat = decimalLatitude,
               long = decimalLongitude) %>%
        lawn_get_is_in_eez(cache = "data/gbif_is_in_eez.rds")
    gbif_filtered_data$is_in_eez <- gbif_filtered_data %in% res_lawn$features$properties$uuid
    gbif_filtered_data
}
