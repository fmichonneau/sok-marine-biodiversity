bold_specimens_raw <- function(taxon) {
    res <- try(bold_specimens(taxon = taxon), silent = TRUE)
    if (inherits(res, "try-error")) {
        message("No results for ", taxon)
        return(NULL)
    } else
        return(res)

}

n_binomial_species <- function(bold_raw_nm) {
    nm <- unique(bold_raw_nm)
    nm <- nm[nchar(nm) > 1]
    nm <- nm[!grepl("[0-9]|(sp\\.|aff\\.|cf\\.)", nm)]

    ## is it a binomial name?
    binom <- is_binomial(nm)

    res <- length(nm)
    attr(res, "species_names") <- nm
    attr(res, "binom") <- binom
    res
}

assemble_bold_data <- function(...) {
    bold <- list(...)
    bold <- bold[!vapply(bold, is.null, logical(1))]
    bold <- lapply(bold, function(x) select(x, -image_ids))
    bind_rows(bold)
}

get_stats_bold <- function(bold) {
    bold %>%
        group_by(phylum_name, class_name) %>%
        summarize(n_binom =  n_binomial_species(species_name),
                  n_spcm = n())  %>%
        filter(! nchar(phylum_name) < 2,
               ! nchar(class_name) < 2) %>%
        mutate(taxon = ifelse(phylum_name == "Chordata", class_name,  phylum_name)) %>%
        group_by(taxon) %>%
        summarize(
            n_binom = sum(n_binom),
            n_spcm = sum(n_spcm)
        )
}


plot_proportion_barcoded_by_phylum <- function(bold, worms, others) {
    worms <- read.csv(file = worms, stringsAsFactors = FALSE)
    other <- read.csv(file = others, stringsAsFactors = FALSE)

    oth <- worms %>%
        dplyr::select_("taxon", "accepted_species_marine") %>%
        dplyr::rename_("accepted_species" = "accepted_species_marine") %>%
        dplyr::bind_rows(other)

    left_join(bold, oth) %>%
        mutate(p_barcoded = n_binom/accepted_species) %>%
        ggplot(.) +
        geom_bar(aes(x = reorder(taxon, p_barcoded), y = p_barcoded), stat = "identity") +
        coord_flip() +
        xlab("Taxa") + ylab("Proportion of number of species for which DNA barcodes are available")

}

## takes the output of n_binomial_species() to inspect taxon names
## that are not binomial (hybrids, include sub-genus, etc.)
show_non_binomial <- function(binom) {
    attr(binom, "species_names")[!attr(binom, "binom")]
}

get_level_identification <- function(bold) {
    ## for this to be accurate, it needs to be aggregated by BIN
    bold %>%
        mutate(taxon = ifelse(phylum_name == "Chordata", class_name, phylum_name)) %>%
        group_by(taxon) %>%
        summarize(
            prop_class = sum(nchar(class_name) < 3 & nchar(order_name) < 3 & nchar(family_name) < 3 & nchar(genus_name) < 3 & nchar(species_name) < 3)/n(),
            prop_order = sum(nchar(order_name) < 3 & nchar(family_name) < 3 & nchar(genus_name) < 3 & nchar(species_name) < 3)/n(),
            prop_family = sum(nchar(family_name) < 3 & nchar(genus_name) < 3 & nchar(species_name) < 3)/n(),
            prop_genus = sum(nchar(genus_name) < 3 & nchar(species_name) < 3)/n()
        ) %>%
        filter(nchar(taxon) > 1)
}

plot_level_identification <- function(lvl_id) {
    lvl_id <- gather(lvl_id, level, proportion, -taxon)
    lvl_id %>%
        ggplot(.) +
        geom_bar(aes(x = reorder(taxon, proportion), y = proportion, fill = level), stat = "identity") +
        coord_flip()
}
