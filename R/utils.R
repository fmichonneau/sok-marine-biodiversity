cleanup_species_names <- function(nm) {
    ## remove words that contain numbers
    nm <- gsub("\\S*\\d\\S*", "", nm)
    ## remove extra spaces
    nm <- gsub("\\s{2, }", " ", nm)
    ## remove sp(p).
    nm <- gsub("\\sspp?\\.", "", nm)
    ## remove words fully capitalized
    nm <- gsub("\\s[A-Z]+", "", nm)
    ## remove trailing spaces
    nm <- gsub("\\s+$", "", nm)
    nm
}


is_binomial <- function(nm) {
    nm <- cleanup_species_names(nm)
    vapply(strsplit(nm, " "), function(x) length(x) == 2, logical(1))
}
