## crust_page <-
## crustacean_html_cache <- saveRDS(crust_page, "data/itis_crustacean_list.rds")

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
    moll_url <- "https://www.itis.gov/servlet/SingleRpt/RefRpt?search_type=publication&search_id=pub_id&search_id_value=4158"
    extract_itis_names(moll_url)
}

crust_name_details <- function(tbl) {
    tbl %>%
        dplyr::mutate(classification =
                          purrr:::pmap(., function(tsn_id, ...) store_itis_classification()$get(tsn_id)),
                      comments =
                          purrr::pmap(., function(tsn_id, ...) store_itis_comments()$get(tsn_id))
                      )
}


moll_name_details <- function(tbl) {
    tbl %>%
        dplyr::mutate(classification =
                          purrr:::pmap(., function(tsn_id, ...) store_itis_classification()$get(tsn_id)),
                      geo =
                          purrr::pmap(., function(tsn_id, ...) store_itis_geo()$get(tsn_id))
                      )
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
