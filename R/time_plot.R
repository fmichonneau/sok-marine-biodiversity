plot_knowledge_through_time <- function(idig_data) {
    res <- idig_data %>%
        mutate(parsed_date = parse_date_time(datecollected, c("Y", "ymd", "ym", "%Y-%m-%d%H:%M:%S%z"))) %>%
        mutate(year = year(parsed_date)) %>%
        mutate(year = replace(year, year > 2017 | year < 1800, NA)) %>%
        filter(!is.na(year))

    ## plot of the number of records per phylum through time
    ## res %>%
    ##     group_by(phylum, year) %>%
    ##     arrange(year) %>%
    ##     summarize(n = n()) %>%
    ##     mutate(cum_n = cumsum(n)) %>%
    ##     ggplot(aes(x = year, y = cum_n, colour = phylum)) +
    ##     geom_line()

    ## plot of the number of "new" species by phylum through time
    res %>%
        group_by(phylum, scientificname, year) %>%
        arrange(year) %>%
        tally() %>%
        mutate(min_year = min(year)) %>%
        select(-n) %>%
        group_by(phylum, min_year) %>%
        arrange(min_year) %>%
        tally() %>%
        mutate(cum_n = cumsum(n))  %>%
        ggplot(aes(x = min_year, y = cum_n, colour = phylum)) +
        geom_line() #+ scale_y_log10()


}
