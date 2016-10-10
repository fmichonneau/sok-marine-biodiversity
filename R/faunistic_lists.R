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

get_idigbio_records <- function(taxon_name, taxon_level) {
    message("Looking for ", taxon_name, " ... ", appendLF = FALSE)
    taxon_level <- match.arg(taxon_level, c("phylum", "class", "order", "family", "genus"))
    fields <- c('uuid',
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
    qry <- list(basisofrecord = "PreservedSpecimen",
                scientificname = list(type = "exists"),
                geopoint = list(type = "geo_bounding_box",
                                top_left = list(lat = 51, lon = -133),
                                bottom_right = list(lat = 23, lon = -51))
                )
    ## apparently, we don't provide the dwc.data:phylum fields so our records
    ## are not included. Using `phylum` to do the query brings in too much crap.
    flmnh_qry <- c(setNames(taxon_name, taxon_level),
                   setNames("flmnh", "institutioncode"),
                   qry)
    taxon_level <- paste0("data.dwc:", taxon_level)
    qry <- c(setNames(taxon_name, taxon_level), qry)
    res <- idig_search_records(rq = qry, limit = 100000,
                               fields = fields)
    flmnh_res <- idig_search_records(rq = flmnh_qry, limit = 100000,
                                     fields = fields)
    message("found ", nrow(res), " records in iDigbio, and ",
            nrow(flmnh_res), " for FLMNH.")
    top_phylum <- names(which.max(table(res$`phylum`)))
    message("  using ", sQuote(top_phylum), " for missing values.")
    if (nrow(res) >  0 && nrow(flmnh_res) > 0)  {
        bind_rows(res, flmnh_res) %>%
            mutate(`data.dwc:phylum` = tolower(`data.dwc:phylum`)) %>%
            mutate(`data.dwc:phylum` = replace(`data.dwc:phylum`,
                                               is.na(`data.dwc:phylum`),
                                               top_phylum))
    } else {
        bind_rows(res, flmnh_res)
    }
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
        filter(is_marine == TRUE) %>%
        select(verbatim_scientificname) %>%
        .[[1]]
    res <- filter(res, scientificname %in% is_marine) %>%
        mutate(parsed_date = parse_date_time(datecollected, c("Y", "ymd", "ym", "%Y-%m-%d%H:%M:%S%z"))) %>%
        mutate(year = year(parsed_date)) %>%
        mutate(year = replace(year, year > 2016 | year < 1800, NA)) %>%
        filter(!is.na(year)) %>% #, !is.na(`data.dwc:phylum`)) %>%
        mutate(`data.dwc:phylum` = capwords(`data.dwc:phylum`, strict = TRUE))

    res

}

assemble_idigbio_species_list <- function(idig_store = store_idigbio_records()) {
    groups <- idig_store$list()
    res <- lapply(groups, function(x) species_list_from_idigbio(idig_store$get(x)))
    names(res) <- groups
    res <- bind_rows(res, .id = "taxon")
    tidyr::extract_(res, "taxon", c("rank", "taxon_name"), "([[:alnum:]]+)-([[:alnum:]]+)")
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
    is_binom <- is_binomial(res)

    data.frame(verbatim_scientificname = idig$scientificname,
               cleaned_scientificname = res,
               is_binomial = is_binom,
               stringsAsFactors = FALSE)
}

add_worms_info <- function(sp_list_idig) {
    res <- sp_list_idig %>%
        filter(is_binomial == TRUE)  %>%
        select(cleaned_scientificname) %>%
        unique
    wid <- valid_name <- vector("character", nrow(res))
    marine <- vector("logical", nrow(res))
    for (i in seq_len(nrow(res))) {
        wid[i] <- store_worms_ids()$get(res[i, 1])
        if (!is.na(wid[i]) && !identical(wid[i], "0")) {
            w_info <- store_worms_info()$get(wid[i])
            if (nrow(w_info) > 1) browser()
            marine[i] <- (identical(w_info$isMarine, "1") | identical(w_info$isBrackish, "1"))
            valid_name[i] <- w_info$valid_name
        } else {
            marine[i] <- NA
            valid_name[i] <- NA
        }
    }
    to_add <- data.frame(
        cleaned_scientificname = res,
        worms_id = wid,
        is_marine = marine,
        worms_valid_name = valid_name,
        stringsAsFactors = FALSE
    )
    left_join(sp_list_idig, to_add, by = "cleaned_scientificname")
}

add_bold_info <- function(worms_idig) {
    res <- worms_idig %>%
        filter(!is.na(is_marine), is_marine == TRUE,
               worms_valid_name != "not in worms") %>%
        select(worms_valid_name) %>%
        unique

    bold_rcrd <- numeric(nrow(res))

    for (i in seq_len(nrow(res))) {
        bold <- store_bold_specimens_per_species()$get(res$worms_valid_name[i])
        bold_rcrd[i] <- ifelse(is.null(bold) || inherits(bold, "character"),
                               0, nrow(bold))
    }
    res <- data.frame(worms_valid_name = res,
                      n_bold_records = bold_rcrd,
                      stringsAsFactors = FALSE)
    wrm <- select(worms_idig, rank, taxon_name, worms_valid_name) %>%
        unique %>%
        filter(!is.na(worms_valid_name))
    left_join(res, wrm)
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

make_table_bold_records_per_idigbio_species <- function(idig_bold) {
    idig_bold %>%
        group_by(taxon_name) %>%
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


make_map_idigibio_records <- function(idig_records) {
    states <- map_data("state")
    idig_records %>%
        filter(scientificname %in% sp_list) %>%
        ggplot(.) +
        annotation_map(states, fill = "gray40") +
        geom_point(aes(x = geopoint.lon, y = geopoint.lat), position = "jitter", colour = "red", alpha = .2) +
        coord_map(projection = "mercator") +
        xlab("longitude") + ylab("latitude") + ggtitle(unique(tolower(idig_records$`data.dwc:phylum`)))

}

make_plot_idigbio_records_per_date <- function(idig_dates) {
    idig_dates %>%
        filter(year >=  1850) %>%
    ggplot(aes(x=year, fill=institutioncode)) +
        geom_bar() + scale_y_log10() +
        facet_wrap(~ `data.dwc:phylum`) +
        scale_fill_viridis(discrete = TRUE)
}
