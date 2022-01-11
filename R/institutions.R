calc_institutions <- function(idig_records, obis_records) {
  res_idig <- idig_records %>%
    dplyr::group_by(institutioncode) %>%
    dplyr::summarize(
      n = n(),
      n_spp = n_distinct(worms_valid_name)
    ) %>%
    dplyr::arrange(desc(n_spp)) %>%
    dplyr::top_n(10, n_spp) %>%
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
      institutioncode == "nrm" ~ "Swedish Museum of Natural History",
      institutioncode == "inhs" ~ "Illinois Natural History Survey",
      institutioncode == "ansp" ~ "The Academy of Natural Sciences of Drexel University",
      institutioncode == "cmn" ~ "Canadian Museum of Nature",
      institutioncode ==  "sbmnh" ~ "Santa Barbara Museum of Natural History",
      institutioncode == "sio" ~ "Scripps Oceanographic Collections",
      institutioncode == "lacm" ~ "Natural History Museum Los Angeles County",
      TRUE ~ "problem"
    ))

  issues_idig <- res_idig %>%
    dplyr::filter(Institution == "problem") %>%
    dplyr::distinct() %>%
    dplyr::pull(institutioncode)

  if (length(issues_idig) > 0) {
    stop("unknown collection in idigbio: ", paste(issues_idig, collapse = ", "))
  }
  
  res_idig <- res_idig %>%
    dplyr::mutate(
      n = format(n, big.mark = ","),
      n_spp = format(n_spp, big.mark = ",")
    ) %>%
   dplyr::select(Institution,
     "Number of records" = n,
     "Number of species" = n_spp
   )

  res_obis <- obis_records %>%
    ## regroup all noaa records
    dplyr::mutate(institutioncode_simple = case_when(
      grepl("noaa|nmfs|dsc_rtp", institutioncode) ~ "noaa",
      grepl("smithsonian", institutioncode) ~ "usnm",
      TRUE ~ institutioncode
    )) %>%
    dplyr::group_by(institutioncode_simple) %>%
    dplyr::filter(!is.na(institutioncode_simple)) %>%
    dplyr::summarize(
      n = n(),
      n_spp = n_distinct(worms_valid_name)
    ) %>%
    dplyr::arrange(desc(n_spp)) %>%
    dplyr::top_n(10, n_spp) %>%
    dplyr::mutate(Institution = case_when(
      institutioncode_simple == "noaa" ~ "National Oceanographic and Atmostpheric Association²",
      institutioncode_simple == "usnm" ~ "Smithsonian Institution National Museum of Natural History¹",
      institutioncode_simple == "tpwd" ~ "Texas Parks and Wildlife Department",
      institutioncode_simple == "boemre" ~ "Bureau of Ocean Energy Management, Regulation, and Enforcement",
      institutioncode_simple == "emap_nca" ~ "U.S. Environmental Protection Agency through its Environmental Monitoring and Assessment Program (EMAP)",
      institutioncode_simple == "usgs" ~ "U.S. Geological Services",
      institutioncode_simple == "mbari" ~ "Monterey Bay Aquarium Research Institute",
      institutioncode_simple == "seamap" ~ " Southeast Area Monitoring and Assessment Program",
      institutioncode_simple == "hri" ~ "Harte Research Institute for Gulf of Mexico Studies",
      institutioncode_simple == "sahfos" ~ "Sir Alister Hardy Foundation for Ocean Science",
      institutioncode_simple == "uf" ~ "Florida Museum of Natural History, University of Florida",
      institutioncode_simple == "tamu" ~ "The Texas A&M Biodiversity Research and Teaching Collections",
      institutioncode_simple == "worms editorial board" ~ "WoRMS editorial board",
      institutioncode_simple == "marine resources research institute, scdnr" ~ "Marine Resource Research Institute, South Carolina Department of Natural Resources",
      institutioncode_simple == "woodsholebiosurvey" ~ "Woods Hole Biological Survey",
      institutioncode_simple == "hex" ~ "Hexacorallians of the World, Kansas University Natural History Museum",
      institutioncode_simple == "mcz" ~ "Museum of Comparative Zoology, Harvard University",
      institutioncode_simple == "cas" ~ "California Academy of Sciences",
      institutioncode_simple == "tcwc" ~ "The Texas A&M Biodiversity Research and Teaching Collections",
      TRUE ~ "problem"
    ))

  issues_obis <- res_obis %>%
    dplyr::filter(Institution == "problem") %>%
    dplyr::distinct() %>%
    dplyr::pull(institutioncode_simple)

  if (length(issues_obis) > 0) {
    stop("unknown collection in obis: ", paste(issues_obis, collapse = ", "))
  }

  res_obis <- res_obis %>%
    dplyr::mutate(
      n = format(n, big.mark = ","),
      n_spp = format(n_spp, big.mark = ",")
    ) %>%
    dplyr::select(Institution,
      "Number of records" = n,
      "Number of species" = n_spp
    )

  list_tbl <- list(
    res_idig,
    res_obis
  )
  attr(list_tbl, "subheadings") <- c("A. iDigBio", "B. OBIS")
  attr(list_tbl, "message") <- strwrap(glue::glue_collapse(
    c(
      "¹also includes data from the Smithsonian ",
      "Environmental Research Center. ",
      "²NOAA records include data from the Hollings Marine Laboratory, ",
      "the Northeast Fisheries Science Center, ",
      "the Southwest Fisheries Science Center, ",
      "the National Centers for Coastal Ocean Science, ",
      "the Center for Coastal Environmental Health and Biomolecular Research, ",
      "the National Marine Fisheries Service, and ",
      "the Deep Sea Coral Research and Technology Program."
    )
  ), width = 100)
  capt <- as.character(
    glue::glue(
      "Top 10 institutions that contribute the most species to the ",
      "records used in this study to iDigBio (A) and OBIS (B). ",
      "Federal and States agencies are important contributors to ",
      "marine biodiversity data. Differences in numbers for the ",
      "same data sources across databases are due to differences in data quality filters."
    )
  )
  x_list_tbl <- xtable::xtableList(
    list_tbl,
    align = c("l", "p{11.5cm}", "R{2cm}", "R{2cm}"),
    caption = capt, label = "tab:records-table"
  )
  x_list_tbl
}
