cleanup_species_names <- function(nm, rm_subgenus = FALSE) {
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
    ## remove cf., aff. and ?
    nm <- gsub("\\s(cf\\.|aff\\.|\\?|(ex\\.? gr\\.))\\s", " ", nm)
    ## remove subgenus
    if (rm_subgenus)
        nm <- gsub("\\s?\\([^)]*\\)\\s?", " ", nm)
    nm
}


is_binomial <- function(nm) {
    nm <- cleanup_species_names(nm)
    vapply(strsplit(nm, " "), function(x) length(x) == 2 &&
                                          vapply(x[[1]],
                                                 function(y) all(nchar(gsub("[a-zA-Z]", "", x)) == 0),
                                                 logical(1)),
           logical(1))
}


keep_marine_taxa <- function(worms) {
    res <- worms %>%
        dplyr::filter(!is.na(is_marine),
                      is_marine == TRUE,
                      worms_valid_name != "not in worms") %>%
        dplyr::distinct(worms_valid_name, .keep_all = TRUE) %>%
        dplyr::select(rank, taxon_name, cleaned_scientificname, worms_valid_name, worms_id)
    res
}

capitalize <- function(s, strict = FALSE) {
    cap <- function(s) paste(toupper(substring(s, 1, 1)),
                       {s <- substring(s, 2); if(strict) tolower(s) else s},
                       sep = "", collapse = " " )
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}


####
### By Scott Chamberlain http://r.789695.n4.nabble.com/Geographic-distance-between-lat-long-points-in-R-td3442338.html
## Convert degrees to radians
deg2rad <- function(deg) return(deg*pi/180)

## Calculates the geodesic distance between two points specified by
## radian latitude/longitude using the Haversine formula
gcd.hf <- function(long1, lat1, long2, lat2, warn = TRUE) {
    if (warn)
        warning("Did you convert the latitudes in radian first?!")
    R <- 6371 # Earth mean radius [km]
    delta.long <- (long2 - long1)
    delta.lat <- (lat2 - lat1)
    a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
    c <- 2 * asin(min(1,sqrt(a)))
    d = R * c
    return(d) # Distance in km
}

format_output <- function(x) {
    sprintf("%.1f", x)
}

render <- function(file, ...) {
    rmarkdown::render(file, ...)
}

## US coordinates
## pretty generous, but easier
us_coords <- function() {
    list(
        lon = c(-128, -60),
        lat = c(22, 51)
    )
}

split_by_n <- function(x, n, ...) {
    split(x, ceiling(seq_along(x)/n), ...)
}

convert_to_feather <- function(file, feather_out) {
    res <- readr::read_csv(file)
    feather::write_feather(res, path = feather_out)
}

is_lower_case <- function(x) {
    if(!identical(tolower(x), x))
       stop(x, " is not lowercase.")
}


filter_raw_records <- function(db) {

    if (inherits(db, "character")) {
        db <- feather::read_feather(db)
        names(db) <- tolower(names(db))
    }

    if (any(duplicated(db$uuid)))
        stop("duplicated UUID values!")

    db %>%
        filter(!is.na(decimallatitude) | !is.na(decimallongitude))
}

first_5 <- function(x) {
    n <- ifelse(length(x) > 5, 5, length(x))
    paste(x[seq_len(n)], collapse = ", ")
}
