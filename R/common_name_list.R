get_common_names <- function() {
    url <- "https://www.itis.gov/servlet/SingleRpt/RefRpt?search_type=publication&search_id=pub_id&search_id_value=844"
    cont <- xml2::read_html(url)
    res <- xml2::xml_find_all(cont, ".//table/tbody/tr/td/a/text()")
    xml2::xml_text(res)
}

binomial_common_names <- function(nm) {
    nm[is_binomial(nm)]
}

add_worms_info_common_names <- function(nm) {
    add_worms_info(nm)
}
