get_phyla_from_db <- function(db_table) {
  sok_db() %>%
    tbl(db_table) %>%
    dplyr::distinct(phylum) %>%
    dplyr::arrange(phylum) %>%
    dplyr::collect()
}


get_phyla <- function(csv) {
  readr::read_csv(csv, col_types = "ccc")
}
