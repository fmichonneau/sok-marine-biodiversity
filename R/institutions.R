calc_institutions <- function(idig_records) {

    res_idig <- idig_records %>%
        dplyr::count(institutioncode, sort = TRUE) %>%
        dplyr::filter(n >= 500) %>%
        dplyr::mutate(Institution = case_when(
                          institutioncode == "usnm" ~ "Smithsonian Institution National Museum of Natural History",
                          institutioncode == "uf" ~ "Florida Museum of Natural History, University of Florida",
                          institutioncode == "ypm" ~ "Yale Peabody Museum of Natural History",
                          institutioncode == "cas" ~ "California Academy of Sciences",
                          institutioncode == "mcz" ~ "Museum of Comparative Zoology, Harvard University",
                          institutioncode == "tcwc" ~ "The Texas A&M Biodiversity Research and Teaching Collections",
                          institutioncode == "fmnh" ~ "The Field Museum of Natural History",
                          institutioncode == "am" ~ "The American Museum of Natural History",
                          institutioncode == "ncsm" ~ "The North Carolina Museum of Natural Sciences",
                          institutioncode == "magnt" ~ "Northern Territory Museum and Art Gallery",
                          TRUE ~ "problem"
                      )) %>%
        purrr::pwalk(function(Institution, ...) {
                   if(any(grepl("problem", Institution))) stop("unknown collection")
               }) %>%
        dplyr::select(Institution,
                      "Number of records" = n)

    res_obis <- obis_records %>%
        dplyr::count(institutioncode, )


}
