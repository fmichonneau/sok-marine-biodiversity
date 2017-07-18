## families that are in not Kozloff but are in GOM
get_not_in_kozloff <- function(koz, gom)  {
    koz %>%
        dplyr::select(family) %>%
        dplyr::distinct(family) %>%
        dplyr::anti_join(gom, by = "family") %>%
        dplyr::mutate(worms_id = vapply(family, function(x) {
                          wid <- store_worms_ids()$get(x)
                          if (inherits(wid, "data.frame")) wid$valid_AphiaID
                          else NA_integer_
                      }, integer(1))) %>%
        dplyr::mutate_if(is.integer, as.character) %>%
        add_classification() %>%
        dplyr::arrange(phylum, order, class, family) %>%
        readr::write_csv("/tmp/test.csv")
}
