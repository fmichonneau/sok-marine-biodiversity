## Given the URL of an ITIS page that lists species, creates a data frame that
## contains the name of the species and its TSN ID.
extract_itis_names <- function(url) {
    itis_pg <- url %>%
        xml2::read_html()

    all_links <- itis_pg %>%
        xml_find_all(".//td/table/tbody/tr//a")

    res <- purrr::map_df(all_links, function(x) {
        data_frame(name = xml_text(x),
                   tsn_id = xml_find_all(x, "@href") %>%
                       xml_text() %>%
                       gsub(".+=([0-9]+)$", "\\1", .)
                   )
    })
    res
}

crustacean_names <- function() {
    crust_url <- "https://www.itis.gov/servlet/SingleRpt/RefRpt?search_type=source&search_id=source_id&search_id_value=271"
    extract_itis_names(crust_url)
}

molluscs_names <- function() {
    moll_url <- "https://www.itis.gov/servlet/SingleRpt/RefRpt?search_type=publication&search_id=pub_id&search_id_value=844"
    extract_itis_names(moll_url)

}

### Crustaceans ----------------------------------------------------------------

## Use the ITIS API (from ritis) to extract classification and comments from TSN
## IDs.  For the crustaceans,the geographic locations and habitats are encoded
## into the comments.
crust_name_details <- function(tbl) {
    tbl %>%
        dplyr::mutate(classification =
                          purrr:::pmap(., function(tsn_id, ...) store_itis_classification()$get(tsn_id)),
                      comments =
                          purrr::pmap(., function(tsn_id, ...) store_itis_comments()$get(tsn_id))
                      )
}

#### codes used in book and encoded in ITIS to represent geographic location and habitat
### Geography
## A: atlantic ocean or atlantic drainage
## Ar: Arctic ocean
## P: Pacific ocean or Pacific drainage
## G: Gulf of Mexico or Gulf of Mexico drainage
## H: Hawaiian islands
## Cos: cosmopolitan
### Habitat
## T: terrestrial
## I: inland waters, usually freshwaters
## B: brackish
## M: marine
## C: commensal (used in Branchiura and Copepoda)
## F: free living (used in Copepoda)
## [E]: introduced, exotic species
## [X]: extinct or thought to be extinct
## [N]: endangered
## ?: questionnable

## Convert the string of codes, into a data frame
unpack_comment <- function(x, dict)  {
    ## Questionable records (with a ?) are ignored
    default <- set_names(as.list(rep(NA, length(dict))), dict)

    if (exists("commentDetail", x)) {
        x <- dplyr::filter(x, grepl("mclaughlin.+occurrence", commentDetail,
                                    ignore.case = TRUE))

        x <- dplyr::filter(x, commentTimeStamp == max(commentTimeStamp))
        if (nrow(x) > 1) {
            stop("problem with number of rows, need stricter filter")
        }
        if (nrow(x) < 1) return(as_tibble(default))
        occurrence_codes <- unlist(strsplit(x$commentDetail, ":"))[2]
        occurrence_codes <- unlist(strsplit(occurrence_codes, ";|,"))
        occurrence_codes <- gsub("\\s", "", occurrence_codes)
        in_codes <- intersect(occurrence_codes, names(dict))
        default[dict[in_codes]] <- TRUE
        as_tibble(default)
    } else {
        as_tibble(default)
    }
}

extract_geo_from_comment <- function(x) {
    geo_dict <- c(
        "A"   = "atlantic",
        "Ar"  = "arctic",
        "P"   = "pacific",
        "G"   = "gulf_mexico",
        "H"   = "hawaii",
        "Cos" = "cosmopolitan"
    )
    unpack_comment(x, geo_dict)

}

extract_habitat_from_comment <- function(x) {
    ## not all taxa are scored for this, so best to rely on WoRMS to filter out
    ## non-marine species
    habitat_dict <- c(
        "T" = "terrestrial",
        "I" = "inland",
        "B" = "brackish",
        "M" = "marine"
    )
    unpack_comment(x, habitat_dict)
}

is_itis_species <- function(id, x) {
    purrr::map2_lgl(id, x, function(.id, .x) {
               if (exists("rankname", .x)) {
                   .x[.x$tsn == .id, "rankname"] == "Species"
               } else FALSE
           })
}

crust_name_geo <- function(tbl) {

    tbl %>%
        dplyr::filter(is_itis_species(tsn_id, classification)) %>%
        dplyr::mutate(geo_codes = purrr::map(comments, extract_geo_from_comment),
                      habitat_codes = purrr::map(comments, extract_habitat_from_comment)
                      ) %>%
        dplyr::select(-classification, -comments) %>%
        tidyr::unnest() %>%
        dplyr::mutate(cleaned_scientificname = remove_subspecies(cleanup_species_names(name)),
                      is_binomial = is_binomial(cleaned_scientificname)) %>%
        add_worms()
}

add_dbs_info <- function(tbl, combined_species, map) {
    tbl %>%
        dplyr::left_join(dplyr::select(combined_species,
                                       worms_id, idigbio, obis, n_bold_records),
                         by = "worms_id") %>%
        dplyr::mutate(
                   info_idigbio = purrr::map(worms_valid_name,
                      function(wm_nm) {
                          v3("worms name: ", wm_nm)
                          .idig <- store_idigbio_species_occurrences()$get(tolower(wm_nm))
                          if (nrow(.idig) < 1)
                              return(as_tibble(list(
                                  n_idigbio_total = NA,
                                  n_idigbio_no_geo = NA,
                                  n_idigbio_geo_us = NA,
                                  n_idigbio_country_us = NA
                              )))
                          .idig <- .idig %>%
                              dplyr::rename(decimallatitude = geopoint.lat,
                                            decimallongitude = geopoint.lon) %>%
                              is_within_eez_records(map = map)
                          as_tibble(list(
                              n_idigbio_total = nrow(.idig),
                              n_idigbio_no_geo = sum(is.na(.idig$decimallatitude) |
                                                     is.na(.idig$decimallongitude)),
                              n_idigbio_geo_us = sum(is.na(.idig$is_in_eez)),
                              n_idigbio_country_us = sum(.idig$country == "united states")
                          ))
                      }),

                   info_obis = purrr::map(worms_id,
                      function(wid) {
                          v3("worms id: ", wid)
                          .obis <- store_obis_occurrences()$get(as.character(wid))
                          if (is.null(.obis))
                              return(
                                  as_tibble(list(
                                      n_obis_total = NA,
                                      n_obis_no_geo = NA,
                                      n_obis_geo_us = NA
                                  ))
                              )

                          .obis <- .obis %>%
                              dplyr::rename(decimallatitude = decimalLatitude,
                                            decimallongitude = decimalLongitude) %>%
                              is_within_eez_records(map = map)

                          ## OBIS doesn't store country info directly, sometimes
                          ## included in locality but difficult to parse.
                          as_tibble(list(
                              n_obis_total = nrow(.obis),
                              n_obis_no_geo = sum(is.na(.obis$decimallatitude) |
                                                  is.na(.obis$decimallongitude)),
                              n_obis_geo_us = sum(.obis$is_in_eez)
                          ))
                      })
               ) %>%
        tidyr::unnest()
}


crust_name_in_dbs <- function(tbl, combined_species, map) {

    ## keep only the continental marine species
    select_crust <- tbl %>%
        filter(is.na(arctic) & is.na(hawaii) &
               (!is.na(atlantic) | !is.na(pacific) | !is.na(gulf_mexico) | !is.na(cosmopolitan)) &
               is.na(terrestrial) & is.na(inland) &
               !is.na(worms_id) & is_marine)

    ## check species that are in the combined US list
    res <- select_crust %>%
        add_dbs_info(combined_species, map)
    res
}

moll_name_details <- function(tbl) {
    tbl %>%
        dplyr::mutate(classification =
                          purrr:::pmap(., function(tsn_id, ...) store_itis_classification()$get(tsn_id)),
                      geo =
                          purrr::pmap(., function(tsn_id, ...) store_itis_geo()$get(tsn_id))
                      )
}

moll_name_geo <- function(tbl) {

    geo_dict <- c(
        "North America" = "north_america",
        "Western Atlantic Ocean" = "us_atlantic",
        "East Pacific" = "us_pacific",
        "Europe & Northern Asia (excluding China)" = "eurasia",
        "Middle America" = "middle_america",
        "Indo-West Pacific" = "indowp",
        "Eastern Atlantic Ocean" = "europe_atlantic",
        "Australia" = "australia"
    )
    default <- set_names(as.list(rep(NA, length(geo_dict))),
                                   geo_dict)
    tbl %>%
        dplyr::filter(is_itis_species(tsn_id, classification)) %>%
        dplyr::mutate(geo = map(geo, function(x) {
                          if (!exists("geographicValue", x))
                              return(as_tibble(default))
                          geo_val <- dplyr::pull(x, geographicValue)
                          if (length(geo_val) < 1) browser()
                          default[geo_dict[geo_val]] <- TRUE
                          as_tibble(default)
                      })) %>%
        dplyr::select(-classification) %>%
        tidyr::unnest() %>%
        dplyr::mutate(cleaned_scientificname = remove_subspecies(cleanup_species_names(name)),
                      is_binomial = is_binomial(cleaned_scientificname)) %>%
        add_worms()
}

molluscs_name_in_dbs <- function(tbl, combined_species, map) {
    select_moll <- tbl %>%
        dplyr::filter((north_america | us_atlantic | us_pacific) &
                      !is.na(worms_id) & is_marine) %>%
        add_dbs_info(combined_species, map)
}


fetch_hook_itis_classif <- function(key, namespace) {
    v3("Getting classifcation for TSN: ", key,  appendLF = FALSE)
    res <- ritis::hierarchy_full(key)
    if (!(inherits(res, "data.frame") && nrow(res) > 0)) {
        warning("no result for: ", sQuote(key))
        NA
    }
    v3(" done.")
    res
}

store_itis_classification <- function(path = "data/storr_itis_classification") {
    invisible(storr::storr_external(storr::driver_rds(path),
                                    fetch_hook_itis_classif))
}

fetch_hook_itis_comments <- function(key, namespace) {
    v3("Getting comments for TSN: ", key, appendLF = FALSE)
    res <- ritis::comment_detail(key)
    if (!(inherits(res, "data.frame") && nrow(res) > 0)) {
        warning("no result for: ", sQuote(key))
        NA
    }
    v3(" done.")
    res
}

store_itis_comments <- function(path = "data/storr_itis_comments") {
    invisible(storr::storr_external(storr::driver_rds(path),
                                    fetch_hook_itis_comments))
}

fetch_hook_itis_geo <- function(key, namespace) {
    v3("Getting geo information from: ", key, appendLF = FALSE)
    res <- ritis::geographic_divisions(tsn = key)
    if(!(inherits(res, "data.frame") && nrow(res) > 0)) {
        warning("no result for: ", sQuote(key))
        NA
    }
    v3(" done.")
    res
}

store_itis_geo <- function(path = "data/storr_itis_geo") {
    invisible(storr::storr_external(storr::driver_rds(path),
                                    fetch_hook_itis_geo))
}
