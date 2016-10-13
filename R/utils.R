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

