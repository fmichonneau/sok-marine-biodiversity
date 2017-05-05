## the file ./data-raw/vertebrates/NACC_list_species_birds.csv
## was downloaded on April 24th, 2017 from http://checklist.aou.org/taxa/
list_us_birds <- function(file) {
    readr::read_csv(file) %>%
        dplyr::filter(is.na(status_accidental),
                      is.na(status_hawaiian),
                      is.na(status_misplaced)) %>%
        dplyr::distinct(species, .keep_all = TRUE) %>%
        dplyr::rename(species_name = species)
}
