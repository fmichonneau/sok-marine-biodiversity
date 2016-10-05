is_binomial <- function(nm) {
    vapply(strsplit(nm, " "), function(x) length(x) == 2, logical(1))
}
