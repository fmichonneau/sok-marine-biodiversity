get_idigibio_records <- function(phylum = "Nemertea") {
    res <- idig_search_records(rq = list(phylum = phylum, basisofrecord = "PreservedSpecimen",
                                         geopoint = list(type = "geo_bounding_box",
                                                         top_left = list(lat = 51, lon = -133),
                                                         bottom_right = list(lat = 23, lon = -51))))
}

species_list_from_idigbio <- function(idig) {
    res <- na.omit(unique(idig$scientificname))
    res <- res[!grepl("sp\\.|[0-9]+", res)]
    res <- gsub("cf\\.|aff\\.", "", res)
    res <- gsub("\\([^)]*\\)", "", res)
    res <- gsub("\\s+$", "", res)
    res <- gsub("\\s{2,}", " ", res)
    res <- res[grepl(" ", res)]
    is_binom <- is_binomial(res)
    if (!all(is_binom)) {
        message("Not binomial: ",
                paste(res[!is_binom], collapse = ", "))
    }
    res
}

get_bold_record_per_species <- function(sp) {
NULL
}
