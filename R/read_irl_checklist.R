irl_phylum_list <- function() {
    c("Mollusca", "Arthropoda", "Annelida", "Cnidaria", "Ectoprocta",
      "Echinodermata", "Nemertea", "Sipuncula", "Porifera",
      "Hemichordata", "Platyhelminthes", "Tardigrada", "Kinorhyncha",
      "Chaetognatha", "Brachiopoda", "Entoprocta", "Ctenophora",
      "Phoronida", "Echiura")
}


read_irl_checklist_raw <- function(file) {

    res <- lapply(3:25, function(i)
        readxl::read_excel(path = file, sheet = i)
        )
    res <- dplyr::bind_rows(res)
    ## remove trailing space in PHYLUM column
    res <- dplyr::mutate(res, PHYLUM = gsub("[^a-z]+$", "", res$PHYLUM))
    res <- dplyr::mutate(res, PHYLUM = gsub("Playhelminthes", "Platyhelminthes", res$PHYLUM))
    ## remove last column that is empty (and causes dplyr bug #1576)
    res <- res[, -ncol(res)]
    res
}


read_irl_checklist <- function(file) {
    res <- read_irl_checklist_raw(file)
    res <- dplyr::filter(res, PHYLUM %in% irl_phylum_list())
    if (! all(res$PHYLUM %in% irl_phylum_list()) &&
        all(irl_phylum_list() %in% res$PHYLUM))
        stop("It looks like there is a typo in the spreadsheet or in ",
             " the `phylum_list()`.")
    res %>%
        dplyr::select(cleaned_scientificname = `SCIENTIFIC NAME`,
                      taxon_name = `PHYLUM`) %>%
        dplyr::mutate(is_binomial = is_binomial(cleaned_scientificname),
                      rank = rep("phylum", nrow(res)))
}


phylum_plot <- function(irl_checklist) {
    irl_checklist %>%
        group_by(PHYLUM) %>%
        tally() %>%
        ggplot() +
        geom_bar(aes(x = reorder(PHYLUM, n), y = n), stat = "identity") +
        coord_flip()
}
