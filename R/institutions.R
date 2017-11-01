calc_institutions <- function(idig_records, obis_records) {

    res_idig <- idig_records %>%
        dplyr::count(institutioncode, sort = TRUE) %>%
        dplyr::top_n(10, n) %>%
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
        dplyr::mutate(n = format(n, big.mark = ",")) %>%
        dplyr::select(Institution,
                      "Number of records" = n)

    res_obis <- obis_records %>%
        dplyr::mutate(institutioncode_simple = case_when(
                          grepl("noaa|nmfs|dsc_rtp", institutioncode) ~ "noaa",
                          TRUE ~ institutioncode
                      )) %>%
        dplyr::count(institutioncode_simple, sort = TRUE) %>%
        dplyr::filter(!is.na(institutioncode_simple)) %>%
        ## regroup all noaa records
        dplyr::top_n(10, n) %>%
        dplyr::mutate(Institution = case_when(
                          institutioncode_simple == "noaa" ~ "National Oceanographic and Atmostpheric Association",
                          institutioncode_simple == "usnm" ~  "Smithsonian Institution National Museum of Natural History",
                          institutioncode_simple == "tpwd" ~ "",
                          institutioncode_simple == "boemre" ~ "Bureau of Ocean Energy Management, Regulation, and Enforcement",
                          institutioncode_simple == "emap_nca" ~ "U.S. Environmental Protection Agency through its Environmental Monitoring and Assessment Program (EMAP)",
                          institutioncode_simple == "usgs" ~ "U.S. Geological Services",
                          institutioncode_simple == "mbari" ~ "Monterey Bay Aquarium Research Institute",
                          institutioncode_simple == "seamap" ~ " Southeast Area Monitoring and Assessment Program",
                          institutioncode_simple == "hri" ~ "Harte Research Institute for Gulf of Mexico Studies",
                          institutioncode_simple == "uf" ~ "Florida Museum of Natural History, University of Florida",
                          TRUE ~ "problem"
                      ))%>%
        purrr::pwalk(function(Institution, ...) {
                   if(any(grepl("problem", Institution))) stop("unknown collection")
               }) %>%
        dplyr::mutate(n = format(n, big.mark = ",")) %>%
        dplyr::select(Institution,
                      "Number of records" = n)




    list_tbl <- list(
               res_idig,
               res_obis
    )
    attr(list_tbl, "subheadings") <- c("A. iDigBio", "B. OBIS")
    x_list_tbl <- xtable::xtableList(list_tbl, align = c("lp{12cm}R{3cm}"),
          caption = "Top 10 institutions that contribute to the records used in this study to iDigBio (A) and OBIS (B).")
    x_list_tbl
}
