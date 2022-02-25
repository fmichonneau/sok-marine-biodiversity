fetch_hook_bold_specimens_per_species <- function(key, namespace) {
  if (is.na(key)) {
    return(NULL)
  }
  is_lower_case(key)
  res <- try(bold_specimens(taxon = paste0("'", key, "'")), silent = TRUE)
  if (inherits(res, "try-error")) {
    v2("No record for ", key, ". Trying to look for synonyms ...")
    wid <- store_worms_ids()$get(key)
    if (!inherits(wid, "data.frame")) {
      v2("  no valid WoRMS ID.")
      return("not in worms/multi match")
    } else {
      syn <- store_synonyms()$get(as.character(wid$valid_AphiaID))
      if (inherits(syn, "character")) {
        res <- try(bold_specimens(taxon = paste0("'", syn, "'", collapse = "|")),
          silent = TRUE
        )
        if (inherits(res, "try-error")) {
          v2(
            "  No record for any of the synonyms: \n",
            paste("   - ", syn, collapse = " \n")
          )
          return(NULL)
        }
        v2(" found ", nrow(res), " records for synonyms")
      } else {
        v2(" No synonyms")
        return(NULL)
      }
    }
  }
  v2(nrow(res), " record(s) for ", key)
  res
}

store_bold_specimens_per_species <- function(store_path = "data/storr_bold_specimens_per_species") {
  storr_external(
    driver_rds(store_path),
    fetch_hook_bold_specimens_per_species
  )
}

## this function takes a data frame, for which the column named with
## the content of `col_nm` contains the species names to look up in
## BOLD. It returns a data frame.
find_bold_records <- function(res, col_nm, show_progress = TRUE) {
  if (!exists(col_nm, res)) {
    stop("invalid column name")
  }

  bold_rcrd <- bold_bin <- n_country <- n_coords <- n_within_eez <- numeric(nrow(res))
  within_north_america <- logical(nrow(res))

  if (show_progress) {
    to_find <- !store_bold_specimens_per_species()$exists(tolower(res[[col_nm]]))
    if (sum(to_find) > 0) {
      pb <- progress::progress_bar$new(total = sum(to_find))
    }
  }

  for (i in seq_len(nrow(res))) {
    if ((!store_bold_specimens_per_species()$exists(tolower(res[[col_nm]][i]))) &&
      show_progress) {
      pb$tick()
    }

    bold <- store_bold_specimens_per_species()$get(tolower(res[[col_nm]][i]))

    if (inherits(bold, "data.frame") && nrow(bold) > 0) {
      coord_data <- tibble(
        decimallatitude = bold$lat,
        decimallongitude = bold$lon
      ) %>%
        dplyr::filter(!is.na(decimallatitude) |
          !is.na(decimallongitude))
      if (nrow(coord_data) > 0) {
        any_within_eez <- any(is_within_eez_records(
          coord_data
        )$within_eez)
      } else {
        any_within_eez <- NA
      }
    } else {
      any_within_eez <- NA
    }

    bold_rcrd[i] <- ifelse(is.null(bold) || inherits(bold, "character"),
      0, nrow(bold)
    )
    bold_bin[i] <- ifelse(is.null(bold) || inherits(bold, "character"),
      0, length(unique(na.omit(bold$bin_uri)))
    )
    n_country[i] <- ifelse(is.null(bold) || inherits(bold, "character"),
      0, sum(!is.na(bold$country))
    )
    n_coords[i] <- ifelse(is.null(bold) || inherits(bold, "character"),
      0, sum((!is.na(bold$lat)) & (!is.na(bold$lon)))
    )
    within_north_america[i] <- ifelse(is.null(bold) || inherits(bold, "character"),
      NA, any(north_america_countries() %in% tolower(bold$country))
    )
    n_within_eez[i] <- any_within_eez
  }
  res$n_bold_records <- bold_rcrd
  res$n_bold_bins <- bold_bin
  res$n_bold_country <- n_country
  res$n_bold_coords <- n_coords
  res$bold_within_north_america <- within_north_america
  res$n_bold_within_eez <- n_within_eez

  res
}


make_stat_bold <- function(gom_bld, koz_bld, gom_wrm, koz_wrm) {
  res <- dplyr::bind_rows(gom_bld, koz_bld) %>%
    dplyr::distinct(worms_valid_name, .keep_all = TRUE)

  bin_data <- get_bold_bins(gom_wrm, koz_wrm)

  ## BINs that are found in more than 1 species
  n_shared_bins <- bin_data %>%
    dplyr::group_by(bins) %>%
    dplyr::summarize(
      n_shared_bins = n_distinct(worms_valid_name)
    ) %>%
    dplyr::filter(n_shared_bins > 1) %>%
    nrow()

  ## Species that have more than 1 BIN associated with their name
  ## -- we need to remove species that have only 1 record
  multi_bin <- bin_data %>%
    dplyr::group_by(worms_valid_name) %>%
    dplyr::summarize(
      n_spp_multi_bin = n_distinct(bins)
    ) %>%
    dplyr::left_join(dplyr::select(res, worms_valid_name, n_bold_records),
      by = "worms_valid_name"
    ) %>%
    dplyr::filter(n_bold_records > 1)

  n_multi_bin <- multi_bin %>%
    dplyr::filter(n_spp_multi_bin > 1) %>%
    nrow()
  p_multi_bin <- n_multi_bin / nrow(multi_bin)

  list(
    p_no_country = 1 - (sum(res$n_bold_country) / sum(res$n_bold_records)),
    p_no_coords = 1 - (sum(res$n_bold_coords) / sum(res$n_bold_records)),
    n_total_records = sum(res$n_bold_records),
    n_shared_bins = n_shared_bins,
    n_spp_multi_bin = n_multi_bin,
    p_spp_multi_bin = p_multi_bin,
    n_spp_not_singleton = nrow(multi_bin),
    n_bins = length(unique(bin_data$bins)),
    n_spp = length(unique(bin_data$worms_valid_name))
  )
}

get_bold_bins <- function(gom, koz) {
  spp <- dplyr::bind_rows(gom, koz) %>%
    dplyr::distinct(worms_valid_name)

  res <- lapply(spp$worms_valid_name, function(x) {
    bd <- store_bold_specimens_per_species()$get(tolower(x))
    if ((!is.null(bd)) && (!inherits(bd, "character")) && nrow(bd) > 0) {
      bins <- unique(bd$bin_uri)
      data.frame(
        worms_valid_name = rep(x, length(bins)),
        bins = bins, stringsAsFactors = FALSE
      )
    } else {
      NULL
    }
  })

  dplyr::bind_rows(res) %>%
    dplyr::filter(nzchar(gsub("\\s+", "", bins)))
}


bold_status <- function(idig) {
  idig %>%
    dplyr::group_by(worms_phylum) %>%
    dplyr::summarize(
      p_has_bold = mean(n_bold_records > 0),
      n_has_bold = sum(n_bold_records > 0)
    )
}

bold_status_data <- function(gom_bold, koz_bold, idig_bold) {
  dplyr::bind_rows(
    gom = bold_status(gom_bold),
    koz = bold_status(koz_bold),
    all_idigbio = bold_status(idig_bold),
    .id = "data_source"
  ) %>%
    dplyr::filter(
      worms_phylum %in% c(
        "annelida", "arthropoda",
        "bryozoa",
        "chordata", "cnidaria",
        "echinodermata", "mollusca",
        "porifera"
      )
    ) %>%
    dplyr::mutate(phylum = capitalize(worms_phylum))
}

plot_bold_status <- function(bold_data) {
  plot_order <- bold_data %>%
    dplyr::filter(data_source == "all_idigbio") %>%
    dplyr::arrange(p_has_bold) %>%
    pull(phylum)

  bold_data %>%
    ggplot(aes(
      x = factor(phylum, levels = plot_order), y = p_has_bold,
      fill = forcats::fct_rev(data_source)
    )) +
    geom_col(position = "dodge") +
    geom_text(aes(y = p_has_bold + .01, label = n_has_bold),
      position = position_dodge(.9),
      ## family = "Ubuntu Condensed",
      hjust = .1
    ) +
    xlab("") +
    ylab("Proportion of species with available DNA barcodes") +
    scale_fill_viridis(
      discrete = TRUE,
      name = "",
      breaks = c("all_idigbio", "gom", "koz"),
      labels = c("US EEZ", "Gulf of Mexico", "Pacific Northwest")
    ) +
    theme_minimal() +
    ## theme_ipsum(
    ##   ##  base_family = "Ubuntu Condensed"
    ## ) +
    coord_flip()
}

calc_prop_spp_barcoded <- function(recs) {
  res <- recs$n_bold_records > 0
  list(
    p_barcoded = mean(res),
    n_barcoded = sum(res)
  )
}

make_stat_barcoding <- function(bold_data) {
  list(
    min_eez_phylum = bold_data %>%
      group_by(data_source) %>%
      filter(data_source == "all_idigbio" &
        p_has_bold == min(p_has_bold)) %>%
      pull(phylum),
    min_eez_value = bold_data %>%
      group_by(data_source) %>%
      filter(data_source == "all_idigbio" &
        p_has_bold == min(p_has_bold)) %>%
      pull(p_has_bold),
    max_eez_phylum = bold_data %>%
      group_by(data_source) %>%
      filter(data_source == "all_idigbio" &
        p_has_bold == max(p_has_bold)) %>%
      pull(phylum),
    max_eez_value = bold_data %>%
      group_by(data_source) %>%
      filter(data_source == "all_idigbio" &
        p_has_bold == max(p_has_bold)) %>%
      pull(p_has_bold)
  )
}


project_contribution <- function(db, use_worms, col_nm) {
  res <- as.list(unique(db[[col_nm]])) %>%
    map_df(function(x) {
      .r <- store_bold_specimens_per_species()$get(tolower(x))
      if (is.null(.r) || inherits(.r, "character")) {
        tibble(
          species_name = character(0),
          projects = character(0),
          n = integer(0)
        )
      } else {
        .r <- .r %>%
          filter(!grepl("Mined from GenBank", institution_storing))

        ## we use the prefix of the processid in BOLD to
        ## figure out the contribution of each project to the
        ## total number of barcodes.
        tibble(
          projects = gsub("([A-Z]{3,5}).+", "\\1", .r$processid)
        ) %>%
          dplyr::count(projects) %>%
          dplyr::bind_cols(tibble(species_name = rep(x, nrow(.)))) %>%
          dplyr::select(species_name, projects, n)
      }
    })
  res
}


get_bold_global_coverage_raw <- function() {

  ## This is an OK approximation of the higher taxa that are marine
  ## for each phylum. Some are missing (acanthocephala, leeches, oligochaetes,
  ## Chaetonotida gastrotrichs (but 0 records in BOLD on 2017-08-10), nematods)
  ##
  ## but also includes too much (all molluscs, all tardigrades, for instance...)
  tribble(
    ~phylum, ~taxa,
    "Annelida", "Polychaeta",
    "Arthropoda", "Harpacticoida",
    "Arthropoda", "Malacostraca",
    "Arthropoda", "Maxillopoda",
    "Arthropoda", "Ostracoda",
    "Arthropoda", "Pycnogonida",
    "Brachiopoda", "Brachiopoda",
    "Bryozoa", "Bryozoa",
    "Cephalorhyncha", "Priapulida",
    "Chaetognatha", "Chaetognatha",
    "Chordata", "Appendicularia",
    "Chordata", "Ascidiacea",
    "Chordata", "Thaliacea",
    "Cnidaria", "Cnidaria",
    "Ctenophora", "Ctenophora",
    "Dicyemida", "Dicyemida",
    "Echinodermata", "Echinodermata",
    "Entoprocta", "Entoprocta",
    "Gastrotricha", "Macrodasyida",
    "Gnathostomulida", "Gnathostomulida",
    "Hemichordata", "Hemichordata",
    "Mollusca", "Mollusca",
    "Nemertea", "Nemertea",
    "Orthonectida", "Orthonectida",
    "Phoronida", "Phoronida",
    "Platyhelminthes", "Turbellaria",
    "Porifera", "Porifera",
    "Rotifera", "Rotifera",
    "Sipuncula", "Sipuncula",
    "Tardigrada", "Tardigrada",
    "Xenacoelomorpha", "Xenacoelomorpha"
  ) %>%
    dplyr::mutate(bold_stats = map(taxa, bold::bold_stats))
}


get_bold_global_coverage <- function(bold_stats_raw, wrms_stats) {
  wrms_stats <- wrms_stats %>%
    dplyr::filter(kingdom == "Animalia") %>%
    dplyr::filter(phylum != "Chordata") %>%
    dplyr::mutate(phylum = replace(phylum, phylum == "Chordata - inverts", "Chordata"))

  bold_stats_raw %>%
    dplyr::mutate(
      n_records = purrr::map_int(bold_stats, "total_records"),
      n_bins = purrr::map_int(bold_stats, function(x) x$bins$count),
      n_spp = purrr::map_int(bold_stats, function(x) x$species$count)
    ) %>%
    dplyr::group_by(phylum) %>%
    dplyr::summarize(
      n_records = sum(n_records),
      n_bins = sum(n_bins),
      n_spp = sum(n_spp)
    ) %>%
    dplyr::left_join(wrms_stats, by = "phylum") %>%
    dplyr::mutate(prop_barcoded = n_spp / accepted_species_marine_non_fossil) ## %>%
  ## dplyr::filter(prop_barcoded < 1) %>%
  ## ggplot(aes(x = phylum, y = prop_barcoded, fill = phylum)) +
  ## geom_col() +
  ## coord_flip()
}


### trying to figure out where the barcodes samples are coming from North America
north_america_countries <- function() {
  c(
    "cuba", "bahamas", "turks and caicos", "bermuda",
    "canada", "mexico", "united states"
  )
}

## calculate stats from BOLD geographic data
bold_geo_stats <- function(bold_data) {
  list(
    n_spp_with_records = sum(bold_data$n_bold_records > 0),
    ## proportion of species with no geographic info in BOLD
    p_no_geo_info = mean(
      (bold_data$n_bold_records > 0 &
        ## no coordinates
        bold_data$n_bold_coords == 0L)
    ),
    ## proportion of species that have at least 1 record within the EEZ
    ## boundaries
    p_within_eez = mean(bold_data$n_bold_within_eez > 0, na.rm = TRUE),
    ## proportion of species that have at least one north american country
    ## listed in BOLD
    p_in_north_america = mean(bold_data$bold_within_north_america, na.rm = TRUE)
  )
}
