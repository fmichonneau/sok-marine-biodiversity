fetch_hook_idigbio_records <- function(key, namespace) {
    key_parts <- unlist(strsplit(key, "-"))
    taxon_level <- key_parts[1]
    taxon_name <- key_parts[2]
    res <- get_idigbio_records(taxon_name, taxon_level)
    res
}

store_idigbio_records <- function(store_path = "data/idigbio_records") {
    storr_external(driver_rds(store_path),
                   fetch_hook_idigbio_records)
}

assemble_idigbio_records <- function(taxon_name, taxon_level) {
    invisible(store_idigbio_records()$get(paste(taxon_level, taxon_name, sep = "-")))
}

idigbio_fields <- function() {
    c('uuid',
      'catalognumber',
      'datecollected',
      'institutioncode',
      'phylum',
      'data.dwc:phylum',
      'data.dwc:class',
      'data.dwc:order',
      'data.dwc:family',
      'data.dwc:genus',
      'scientificname',
      'datecollected',
      'country',
      'geopoint')
}

get_idigbio_records <- function(taxon_name, taxon_level) {
    message("Looking for ", taxon_name, " ... ")
    taxon_level <- match.arg(taxon_level, c("phylum", "class", "order", "family", "genus"))
    fields <- idigbio_fields()
    regions <- list(
        gulf_of_mex = list(type = "geo_bounding_box",
                           top_left = list(lat = 30.6, lon = -98.3),
                           bottom_right = list(lat = 23, lon = -80.6)),
        south_east = list(type = "geo_bounding_box",
                          top_left = list(lat = 39.5, lon = -82),
                          bottom_right = list(lat = 23, lon = -61.4)),
        north_east = list(type = "geo_bounding_box",
                          top_left = list(lat = 49.5, lon = -74.6),
                          bottom_right = list(lat = 39.5, lon = -61.4)),
        west_coast = list(type = "geo_bounding_box",
                          top_left = list(lat = 49.5, lon = -127.1),
                          bottom_right = list(lat = 31.7, lon = -116.9))
    )

    RES <- lapply(seq_along(regions), function(r) {
        message("  in ", names(regions)[r], ": ",   appendLF = FALSE)
        qry <- list(basisofrecord = "PreservedSpecimen",
                    scientificname = list(type = "exists"),
                    geopoint = regions[[r]])
        ## apparently, FLMNH doesn't provide the dwc.data:phylum fields so
        ## their records are not included. Using `phylum` to do the query
        ## on all iDigBio brings in too much crap, so we need two queries
        flmnh_qry <- c(setNames(taxon_name, taxon_level),
                       setNames("flmnh", "institutioncode"),
                       qry)
        taxon_level <- paste0("data.dwc:", taxon_level)
        qry <- c(setNames(taxon_name, taxon_level), qry)
        res <- idig_search_records(rq = qry, limit = 100000,
                                   fields = fields)
        flmnh_res <- idig_search_records(rq = flmnh_qry, limit = 100000,
                                         fields = fields)
        message("there are ", nrow(res), " records in iDigbio, and ",
                nrow(flmnh_res), " for FLMNH.")
        bind_rows(res, flmnh_res)

    })
    RES <- bind_rows(RES)
    top_phylum <- names(which.max(table(RES$`phylum`)))
        message("  using ", sQuote(top_phylum), " for missing values.")
    if (nrow(RES) > 0) {
        RES %>%
            distinct(uuid, .keep_all = TRUE) %>%
            mutate(`data.dwc:phylum` = tolower(`data.dwc:phylum`)) %>%
            mutate(`data.dwc:phylum` = replace(`data.dwc:phylum`,
                                               is.na(`data.dwc:phylum`),
                                               top_phylum))
    } else
        RES
}

capwords <- function(s, strict = FALSE) {
    cap <- function(s) paste(toupper(substring(s, 1, 1)),
                       {s <- substring(s, 2); if(strict) tolower(s) else s},
                       sep = "", collapse = " " )
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

assemble_idigbio_species_list_dates <- function(idig_worms) {
    res <- lapply(store_idigbio_records()$list(), function(x)
        store_idigbio_records()$get(x))
    res <- bind_rows(res)
    is_marine <- idig_worms %>%
        dplyr::filter(is_marine == TRUE) %>%
        dplyr::select(verbatim_scientificname) %>%
        .[[1]]
    res <- filter(res, scientificname %in% is_marine) %>%
        mutate(parsed_date = parse_date_time(datecollected, c("Y", "ymd", "ym", "%Y-%m-%d%H:%M:%S%z"))) %>%
        mutate(year = year(parsed_date)) %>%
        mutate(year = replace(year, year > 2016 | year < 1800, NA)) %>%
        filter(!is.na(year)) %>% #, !is.na(`data.dwc:phylum`)) %>%
        mutate(`data.dwc:phylum` = capwords(`data.dwc:phylum`, strict = TRUE))

    res

}

assemble_idigbio_species_list <- function(geo_filter = c("global", "east_coast",
                                                         "west_coast", "florida"),
                                          idig_store = store_idigbio_records()) {
    geo_filter <- match.arg(geo_filter)
    groups <- idig_store$list()
    res <- lapply(groups, function(x) {
        idig <- idig_store$get(x)
        idig <- switch(geo_filter,
                       global = idig,
                       east_coast = filter_idigbio_east_coast(idig),
                       west_coast = filter_idigbio_west_coast(idig),
                       florida = filter_idigbio_florida(idig))
        species_list_from_idigbio(idig)
    })
    names(res) <- groups
    res <- bind_rows(res, .id = "taxon")
    tidyr::extract_(res, "taxon", c("rank", "taxon_name"), "([[:alnum:]]+)-([[:alnum:]]+)")
}

filter_idigbio_east_coast <- function(idig) {
    filter(idig, geopoint.lon > -99)
}

filter_idigbio_west_coast <- function(idig) {
    filter(idig, geopoint.lon < -112)
}

filter_idigbio_florida <- function(idig) {
    filter(idig, geopoint.lon > -88, geopoint.lon < -78,
           geopoint.lat > 24,  geopoint.lat < 30)
}

species_list_from_idigbio <- function(idig) {
    res <- idig$scientificname
    ## Criteria that make the name invalid
    ## remove sp., spp., / and numbered species
    res[grepl("spp?\\.|[0-9]+|\\/", res)] <- NA
    ## remove species with a single letter word
    res[vapply(res, function(x) any(nchar(unlist(strsplit(x, " "))) < 2), logical(1))] <- NA
    ## Name modifications
    ## remove cf., aff. and ? from the species names
    res <- gsub("cf\\.|aff\\.|\\?", "", res)
    ## remove subgenus
    res <- gsub("\\([^)]*\\)", "", res)
    ## remove everything that comes after varieties
    res <- gsub("var\\. .+$", "", res)
    ## remove extra spaces at the end or in the middle
    res <- gsub("\\s+$", "", res)
    res <- gsub("\\s{2,}", " ", res)

    data.frame(verbatim_scientificname = idig$scientificname,
               cleaned_scientificname = res,
               is_binomial = is_binomial(res),
               stringsAsFactors = FALSE)
}



fetch_hook_bold_specimens_per_species <- function(key, namespace) {
    if (is.na(key)) return(NULL)
    res <- try(bold_specimens(taxon = paste0("'", key, "'")), silent = TRUE)
    if (inherits(res, "try-error")) {
        message("No record for ", key, ". Trying to look for synonyms ...", appendLF = FALSE)
        wid <- store_worms_ids()$get(key)
        if (is.na(wid)) {
            message("Can't find a valid WoRMS ID")
            return("not in worms/multi match")
        } else {
            syn <- store_synonyms()$get(wid)
            res <- try(bold_specimens(taxon = paste0("'", syn, "'", collapse = "|")),
                       silent = TRUE)
            if (inherits(res, "try-error")) {
                message("No record for any of the synonyms ",
                        paste(syn, collapse = ", "))
                return(NULL)
            }
            message(" found ", nrow(res), " records for synonyms")
        }
    }
    message(nrow(res), " record(s) for ", key)
    res
}

store_bold_specimens_per_species <- function(store_path = "data/bold_specimens_per_species") {
    storr_external(driver_rds(store_path),
                   fetch_hook_bold_specimens_per_species)
}

fill_bold_specimens_per_species <- function(sp_list, phylum, store = store_bold_specimens_per_species()) {
    res <- lapply(sp_list, function(x) store$get(x, namespace = phylum))
    invisible(res)
}

assemble_bold_specimens_per_species <- function(sp_lists) {
    lapply(names(sp_lists), function(x) {
        fill_bold_specimens_per_species(sp_lists[[x]], x)
    })
}

make_table_bold_records_per_idigbio_species <- function(...) {
    idig <- list(...)
    names(idig) <- names(list(...))
    idig_bold <- bind_rows(idig, .id = "geo_scope")
    idig_bold %>%
        group_by(geo_scope, taxon_name) %>%
        summarize(
            n_spp = n(),
            p_seq = sum(n_bold_records > 0)/n(),
            mean_seq_spcm = mean(n_bold_records)
        )
}

make_plot_bold_records_per_idigbio_species <- function(idig_table) {
    idig_table %>%
        filter(p_seq > 0) %>%
        ggplot(.) +
        geom_bar(aes(x = reorder(taxon_name, p_seq), y = p_seq), stat = "identity") +
        coord_flip() +
        xlab("Taxa") + ylab("Proportion of species sequenced")
}


make_map_idigibio_records <- function(idig_worms) {
    states <- map_data("state")

    idig <- store_idigbio_records()$list()
    res <- lapply(idig, function(x) store_idigbio_records()$get(x))
    res <- bind_rows(res)
    tmp_db <- src_sqlite("data/tmp_idigdb.sqlite", create = TRUE)
    ##idig_sql <- copy_to(tmp_db, res, name = "idigbio_records", temporary = FALSE, indexes = list("scientificname", "phylum"))
    ##wrms_sql <- copy_to(tmp_db, idig_worms, name = "worms", temporary = FALSE, indexes = list("taxon_name", "verbatim_scientificname"))

    pdf(file = "figures/map_by_phyla.pdf", paper = "USr")
    on.exit(dev.off())
    lapply(unique(res$`data.dwc:phylum`), function(x) {
        message("making map for ", x)
        db <- tbl(tmp_db, sql(paste0("SELECT DISTINCT idigbio_records.uuid, idigbio_records.`data.dwc:order`, idigbio_records.`data.dwc:phylum`, idigbio_records.`data.dwc:family`, idigbio_records.`scientificname`, idigbio_records.`geopoint.lat`, idigbio_records.`geopoint.lon`, worms.worms_id, worms.worms_valid_name  FROM idigbio_records JOIN worms ON idigbio_records.scientificname = worms.verbatim_scientificname WHERE worms.is_marine = 1 AND idigbio_records.`data.dwc:phylum` is '", x, "'")))
        message("Starting the collect")
        db_res <- collect(db, n = Inf)
        if (nrow(db_res) < 2) return(NULL)
        p <- ggplot(db_res) +
            annotation_map(states, fill = "gray40") +
            geom_point(aes(x = geopoint.lon, y = geopoint.lat), position = "jitter", colour = "red", alpha = .15) +
            coord_map(projection = "mercator") +
            xlab("longitude") + ylab("latitude") +
            ggtitle(unique(tolower(x)))
        print(p)
    })
}

make_plot_idigbio_records_per_date <- function(idig_dates, to_keep = c("Echinodermata", "Annelida", "Arthropoda", "Mollusca", "Porifera")) {
    idig_dates %>%
        filter(year >=  1900,
               `data.dwc:phylum` %in% to_keep) %>%
        group_by(year, institutioncode, `data.dwc:phylum`) %>%
        tally %>%
        ggplot(aes(x = year, y = n)) +
        geom_bar(aes(fill=institutioncode), stat = "identity") +
        geom_smooth(method = "lm", formula = y ~ splines::bs(x, 6), se = FALSE) + #geom_smooth() +
        scale_y_log10() +
        facet_wrap(~ `data.dwc:phylum`) +
        scale_fill_viridis(discrete = TRUE)
}

make_proportion_barcoded_east_coast <- function(idigbio_)

make_proportion_barcoded_summary <- function(bold_stats, idigbio_stats) {
    res <- left_join(bold_stats, idigbio_stats, by = c("taxon" = "taxon_name")) %>%
        select(taxon, global_barcoded = p_barcoded, us_barcoded = p_seq ) %>%
        gather(scale, prop_seq, -taxon)

    ggplot(res) +
        geom_bar(aes(x = reorder(taxon, prop_seq), y = prop_seq, fill = scale), stat = "identity", position = "dodge") +
        coord_flip()

}

calc_average_prop_species_seq <- function(idig) {
    res <- idig %>%
        group_by(geo_scope) %>%
        summarize(
            avg_prop_seq = mean(p_seq, na.rm = TRUE)
        ) %>%
        filter(geo_scope == "global") %>%
        select(avg_prop_seq) %>%
        .[[1]]
    format_output(res * 100)
}
