list_us_amphibians <- function() {
    xml_data <- httr::GET("http://amphibiaweb.org/cgi/amphib_ws_locality?where-isocc=us&rel-isocc=like") %>%
        httr::content(encoding = "UTF-8", as = "text") %>%
        xml2::read_xml()

    spp <- xml2::xml_find_all(xml_data, ".//amphibian/scientificname") %>%
        xml2::xml_text()

    families <- xml2::xml_find_all(xml_data, ".//amphibian/family") %>%
        xml2::xml_text()

    orders <- xml2::xml_find_all(xml_data, ".//amphibian/order") %>%
        xml2::xml_text()

    tibble(
        order = orders,
        family = families,
        species_name = spp
    )
}

fetch_amphibian_idigbio <- function(amph) {
    ## We can't use the same logic as for mammals or other
    ## invertebrates, as some species have more than 100000, so until
    ## iDigBio fixes the bug, there is no easy way to get the full
    ## records. Getting the counts per species, is probably enough for
    ## now though.
    purrr::map_df(amph$species_name, function(x) {
        qry <- list(scientificname = as.list(x),
                    basisofrecord = "PreservedSpecimen",
                    country = "united states")
        tibble(species_name = x, count = ridigbio::idig_count_records(rq = qry))
    }) %>%
        dplyr::right_join(amph, by = "species_name") %>%
        dplyr::select(order, family, species_name, count)
}

fetch_amphibian_bold <- function(amph) {
    internal_add_bold(amph, "species_name")
}
