common_phyla <- function(lower_case = FALSE) {
    res <- c("Annelida", "Arthropoda",
              "Echinodermata", "Mollusca",  "Chordata",
             "Nematoda", "Nemertea", "Porifera",
             "Bryozoa", "Cnidaria")
    if (lower_case) return(tolower(res))
    res
}
