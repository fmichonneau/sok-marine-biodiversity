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
    res <- lapply(wrm[["worms_valid_name"]], function(x) {
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
    names(res) <- tolower(names(res))
    res <- res %>%
        dplyr::rename(scientificname_authority = scientificname,
                      scientificname = species)
    feather::write_feather(res, path = feather_path)
}


filter_gbif <- function(feather_path) {
    gb <- feather::read_feather(feather_path)
    gb %>%
        filter(basisOfRecord != "FOSSIL_OBSERVATION",
               !is.na(decimalLatitude) | !is.na(decimalLongitude))
}

us_gbif <- function(gbif_filtered_data, map_usa) {
    res_lawn <- gbif_filtered_data %>%
        dplyr::select(uuid = key,
               lat = decimalLatitude,
               long = decimalLongitude) %>%
        is_in_eez(map_usa)
    gbif_filtered_data$is_in_eez <- gbif_filtered_data$key %in% res_lawn$features$properties$uuid
    gbif_filtered_data
}
