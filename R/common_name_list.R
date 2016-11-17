get_common_names <- function() {
    url <- "https://www.itis.gov/servlet/SingleRpt/RefRpt?search_type=publication&search_id=pub_id&search_id_value=844"
    cont <- xml2::read_html(url)
    res <- xml2::xml_find_all(cont, ".//table/tbody/tr/td/a/text()")
    xml2::xml_text(res)
}

binomial_common_names <- function(nm) {
    data.frame(cleaned_scientificname = cleanup_species_names(nm),
               is_binomial = is_binomial(nm),
               stringsAsFactors = FALSE)
}

add_worms_info_common_names <- function(nm) {
    nm <- nm %>%
        dplyr::filter(is_binomial == TRUE) %>%
        dplyr::select(cleaned_scientificname) %>%
        unique
    res <- add_worms_info(nm)
    res$rank <- rep("Phylum", nrow(res))
    res$taxon_name <- rep("Mollusca", nrow(res))
    res
}
