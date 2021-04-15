add_depth <- function(tbl, map) {
  uniq_depths <- tbl %>%
    dplyr::distinct(decimallongitude, decimallatitude) %>%
    dplyr::select(decimallongitude, decimallatitude)

  bathys <- marmap::get.depth(bathy_from_map(map),
    uniq_depths,
    locator = FALSE
  ) %>%
    dplyr::rename(marmap_depth = depth)

  tbl %>%
    dplyr::left_join(bathys, by = c(
      "decimallongitude" = "lon",
      "decimallatitude" = "lat"
    ))
}
