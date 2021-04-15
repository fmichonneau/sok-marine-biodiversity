parse_year <- function(recs) {
  recs %>%
    dplyr::mutate(parsed_date = parse_date_time(datecollected, c("Y", "ymd", "ym", "%Y-%m-%d%H:%M:%S%z"))) %>%
    dplyr::mutate(
      year = year(parsed_date),
      year = replace(year, year > 2017 | year < 1850, NA),
      year = as.integer(year)
    ) %>%
    dplyr::select(-parsed_date)
}

year_as_integer <- function(d) {
  stopifnot(is.list(d))
  lapply(d, function(x) {
    if (exists("year", x)) {
      x$year <- as.integer(x$year)
    }
    x
  })
}
