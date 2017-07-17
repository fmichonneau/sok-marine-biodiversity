prop_identified_by_phylum <- function(db) {

    db %>%
        idigbio_add_year() %>%
        dplyr::filter(!is.na(year)) %>%
        dplyr::mutate(year_5 = cut(year, (max(year)-min(year))/5)) %>%
        dplyr::group_by(phylum, year_5) %>%
        dplyr::summarize(
                   n_records = n(),
                   n_unidentified = sum(is.na(worms_id)),
                   p_unidentified = n_unidentified/n_records,
               )
}



if (FALSE) {

    ## a few examples of visualization:

    ## proportions with number of samples on top,  not great
    remake::fetch("idigbio_records") %>%
        prop_identified_by_phylum() %>%
        filter(phylum %in% common_phyla(TRUE)) %>%
        ggplot(aes(x = year_5, fill = phylum, y = p_unidentified)) +
        geom_col(position = "dodge") +
        geom_text(aes(x = year_5, y = .9, label = n_records), angle = 90) +
        facet_wrap(~ phylum)

    ## proportions again, but using transparency on log scale to
    ## represent number of samples, and only keeping most samples
    ## phyla
    remake::fetch("idigbio_records") %>%
        prop_identified_by_phylum() %>%
        filter(phylum %in% common_phyla(TRUE)[1:5]) %>%
        ggplot(aes(x = year_5, fill = phylum, y = p_unidentified)) +
        geom_col(aes(alpha = log(n_records)), position = "dodge") +
        geom_text(aes(x = year_5, y = .9, label = n_records), angle = 90) +
        facet_wrap(~ phylum) +
        scale_fill_ipsum() +
        theme_ipsum_rc() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))



    ## absolute numbers, not great etiher
      remake::fetch("idigbio_records") %>%
          prop_identified_by_phylum() %>%
          filter(phylum %in% common_phyla(TRUE)) %>%
          tidyr::gather(identified, n, -phylum, -year_5) %>%
              dplyr::filter(identified != "n_no_coords",
                            identified != "p_no_coords") %>%
              ggplot(aes(x = year_5, y = n, fill = identified)) +
              geom_col() +
              facet_wrap(~ phylum, scales = "free_y")


}
