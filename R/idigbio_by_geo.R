## build the list of iDigBio queries based on the data frame that
## contains the coordinates of each element of the geographic grid.
coords_to_query <- function(coords) {
    qry <- lapply(seq_len(nrow(coords)), function(i) {
        list(basisofrecord = "PreservedSpecimen",    # only extant specimens (no fossils)
             scientificname = list(type = "exists"), # only records that contain something in the species name
             geopoint = list(
                 type = "geo_bounding_box",
                 top_left = list(lon = coords$xmin[i], lat = coords$ymax[i]),
                 bottom_right = list(lon = coords$xmax[i], lat = coords$ymin[i])
             ))
    })
    names(qry) <- coords$key
    qry
}


## fetch hook for the idigbio by geography storr. We use a closure to
## enforce the remake dependency on the data frame that contains the
## coordinates for each element of the grid.
internal_idigbio_by_geo_fetch <- function(coords_qry, key) {
    ridigbio::idig_search_records(rq = coords_qry[[key]], fields = idigbio_fields())
}

make_hook_idigbio_by_geo <- function(coords_qry) {
    force(coords_qry)
    function(key, namespace) {
        message("... ", appendLF = FALSE)
        res <- try(internal_idigbio_by_geo_fetch(coords_qry, key),
                   silent = TRUE)
        attempts <- 0
        pred <- (inherits(res, "try-error") && !grepl("return more than", res))
        while (pred && attempts <= 3) {
            message("sleeping ... ")
            Sys.sleep(exp(runif(1) * attempts))
            res <-  try(internal_idigbio_by_geo_fetch(coords_qry, key),
                        silent = TRUE)
            pred <- (inherits(res, "try-error") && !grepl("return more than", res))
            attempts <- attempts + 1
            message("attempt ... ", attempts)
        }
        if (pred) {
            stop(res)
        } else {
            return(res)
        }
    }
}

## define the storr that contains the iDigBio record for each element
## of the grid.
store_idigbio_by_geo <- function(coords, store_path = "data/idigbio_by_geo") {
    fetch_hook_idigbio_by_geo <- make_hook_idigbio_by_geo(coords)
    storr::storr_external(storr::driver_rds(store_path),
                          fetch_hook_idigbio_by_geo)
}


## Work around the 1e5 records idigbio api limit. We try to get the
## results for the coordinates, if we fail because of the limit, we
## split the area into 4 squares, and look for records within
## them. Its recursive nature should make it work.
get_idigbio_by_geo <- function(coords, q) {
    res <- try(store_idigbio_by_geo(coords)$get(q), silent = TRUE)
    if (inherits(res, "try-error")) {
        if (grepl("return more than", res)) {
            crds <- unlist(strsplit(q, "\\|"))
            mid_lon <- mean(as.numeric(c(crds[1], crds[3])))
            mid_lat <- mean(as.numeric(c(crds[2], crds[4])))
            .r <- list(
                top_left     = c(crds[1], crds[2], mid_lon, mid_lat),
                top_right    = c(mid_lon, crds[2], crds[3], mid_lat),
                bottom_left  = c(crds[1], mid_lat, mid_lon, crds[4]),
                bottom_right = c(mid_lon, mid_lat, crds[3], crds[4])
            ) %>%
                purrr::map(function(x) set_names(x, c("xmin", "ymax", "xmax", "ymin"))) %>%
                purrr::map(as.list) %>%
                purrr::map_df(as_tibble) %>%
                dplyr::bind_cols(data_frame(key = rep(q, 4))) %>%
                coords_to_query()
            return(lapply(names(.r), function(x)
                get_idigbio_by_geo(.r, x)))
        } else stop("Error with iDigBio query")
    } else {
        return(res)
    }
}


get_coords_idigbio_query <- function(map_usa, cellsize = .5) {
    bb_eez <- generate_bounding_boxes(map_usa, cellsize = cellsize)
    coords_to_query(bb_eez)
}

## for all the coordinates of the bounding boxes, find the iDigBio
## records they contain coords: output of get_coords_idigbio_query
## map_usa: map in GeoJSON db_table: name of table in the postgres
## database that will store the results use_cache: if TRUE, this uses
## the results from the iDigBio storr; if false, the entire storr is
## destroyed before
create_idigbio_db <- function(coords, db_table, gom_phyla) {
    idig_types <- structure(c("TEXT", "TEXT", "TEXT", "TEXT", "TEXT",
                              "TEXT", "TEXT", "TEXT", "TEXT", "TEXT",
                              "TEXT", "REAL", "REAL"),
                            .Names = c("uuid", "catalognumber",
                                  "datecollected", "institutioncode",
                                  "phylum", "class", "order", "family", "genus",
                                  "scientificname", "country",
                                  "decimallatitude", "decimallongitude"
                                  ))

    db <- connect_sok_db()
    con <- db$con
    db_begin(con)
    on.exit(db_rollback(con, db_table))

    if (db_has_table(con, db_table))
        db_drop_table(con, db_table)

    db_create_table(con, db_table, types = idig_types, temporary = FALSE)

    lapply(names(coords), function(q) {
        message("Getting iDigBio records for ", q, appendLF = FALSE)
        r <- get_idigbio_by_geo(coords, q) %>%
            dplyr::rename(phylum = `data.dwc:phylum`,
                          class = `data.dwc:class`,
                          order = `data.dwc:order`,
                          family = `data.dwc:family`,
                          genus = `data.dwc:genus`,
                          decimallatitude  = geopoint.lon,
                          decimallongitude = geopoint.lat) %>%
            dplyr::mutate_if(is.character, tolower)
        message(" DONE.")
        db_insert_into(con, db_table, r)
    })

    db_create_indexes(con, db_table, indexes = list(c("phylum", "class", "family", "scientificname"),
                                                    c("country")),
                      unique = FALSE)
    db_analyze(con, db_table)
    db_commit(con)
    on.exit(NULL)
}


extract_inverts_from_db <- function(db_table, gom_phyla) {

    ## We only want:
    ## - unique records
    ## - invertebrates
    ## - marine

    data_db <- connect_sok_db()

    db <- data_db %>%
        dplyr::tbl(db_table)

    arth_class_to_rm <- arthropod_classes_to_rm()
    chr_class_to_rm <- chordata_classes_to_rm()
    chr_fam_to_rm <- chordata_families_to_rm()

    arth_family_to_rm <- db %>%
        dplyr::filter(phylum == "arthropoda" & class %in% arth_class_to_rm) %>%
        dplyr::select(phylum, family) %>%
        dplyr::distinct(phylum, family) %>%
        dplyr::filter(!is.na(family))

    chordata_family_to_rm <- db %>%
        dplyr::filter(phylum == "chordata" &
                      (class %in% chr_class_to_rm |
                       family %in% chr_fam_to_rm)) %>%
        dplyr::select(phylum, class, family) %>%
        dplyr::distinct(phylum, class, family)

    ## check phyla that get filtered out
    db %>%
        dplyr::distinct(phylum) %>%
        dplyr::collect(n = Inf) %>%
        dplyr::anti_join(data_frame(phylum = gom_phyla), by = "phylum") %>%
        readr::write_csv(paste0("data-validation/check_", db_table, "_phyla.csv"))

    db %>%
        ## First let's get the phylum names from the Gulf of Mexico list,
        ## that will take care of plants, fungi, and records with no
        ## specified phylum
        dplyr::filter(phylum %in% gom_phyla) %>%
        ## only the obviously non-marine arthropods and the vertebrates
        dplyr::anti_join(arth_family_to_rm, by = c("phylum", "family")) %>%
        dplyr::anti_join(chordata_family_to_rm, by = c("phylum", "family")) %>%
        dplyr::anti_join(chordata_family_to_rm, by = c("phylum", "class")) %>%
        dplyr::collect(n = Inf) %>%
        dplyr::distinct(uuid, .keep_all = TRUE) %>%
        dplyr::mutate(cleaned_scientificname = cleanup_species_names(scientificname),
                      is_binomial = is_binomial(cleaned_scientificname))
}

filter_idigbio_records <- function(idig, map_usa) {
    idig %>%
        is_within_eez_records(map_usa) %>%
        dplyr::filter(is_in_eez == TRUE) %>%
        add_worms()
}


n_spp_from_idigbio <- function(idigbio_records) {
    idigbio_records %>%
        dplyr::filter(!is.na(is_marine),
                      is_marine == TRUE,
                      worms_valid_name != "not in worms") %>%
        mutate(phylum = tolower(`data.dwc:phylum`)) %>%
        group_by(phylum) %>%
        summarize(
            n_spp = n_distinct(worms_valid_name)
        )
}

plot_idigbio_invert_summary <- function(idigbio_records, idigbio_bold) {
    n_spp <- n_spp_from_idigbio(idigbio_records)

    n_bold <- idigbio_bold %>%
        group_by(phylum) %>%
        summarize(
            n_spp_bold = sum(n_bold_records > 0)
        )

    res <- left_join(n_spp, n_bold, by = "phylum") %>%
        mutate(p_bold = n_spp_bold/n_spp)

    ggplot(res, aes(x = reorder(phylum, p_bold), y = p_bold)) +
        geom_col() +
        coord_flip()

}

us_raster <- function()
    raster(vals = NA, xmn = -127, ymn = 23, xmx = -61, ymx = 50, res = .2)

make_data_map_sampling_effort <- function(idig) {
    us_raster <- us_raster()
    pts <- SpatialPoints(data.frame(lon = idig$decimallongitude,
                                    lat = idig$decimallatitude))
    r <- rasterize(pts, us_raster, fun = "count")
    gg_r <- as.data.frame(as(r, "SpatialPixelsDataFrame"))
    colnames(gg_r) <- c("value", "x", "y")
    gg_r
}


make_data_map_diversity <- function(idig) {
    us_raster <- us_raster()
    raster_cell <- mapply(function(x, y) cellFromXY(us_raster, c(x, y)),
                          idig$decimallongitude, idig$decimallatitude)

    idig_r <- data.frame(idig, rastercell = raster_cell) %>%
        group_by(rastercell) %>%
        summarize(
            n_spp = length(unique(scientificname))
        )
    us_raster[na.omit(idig_r$rastercell)] <- idig_r$n_spp[!is.na(idig_r$rastercell)]
    gg_r <- as.data.frame(as(us_raster, "SpatialPixelsDataFrame"))
    colnames(gg_r) <- c("value", "x", "y")
    gg_r
}

make_data_map_standardized_diversity <- function(sampling, diversity) {
    sampling <- sampling %>%
        rename(n_specimen = value)
    diversity <- diversity %>%
        rename(n_species = value)

    res <- bind_cols(sampling, dplyr::select(diversity, n_species)) %>%
        dplyr::select(x, y, n_specimen, n_species) %>%
        mutate(value = n_species*n_species/n_specimen)

    res
}


make_heatmap_sampling <- function(gg_r, title, limits = NULL) {
    state <- maps::map("world", fill = TRUE, plot = FALSE)
    ## convert the 'map' to something we can work with via geom_map
    IDs <- sapply(strsplit(state$names, ":"), function(x) x[1])
    state <- map2SpatialPolygons(state, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))

    us_bathy <- suppressMessages(getNOAA.bathy(lon1 = -128, lon2 = -60, lat1 = 22, lat2 = 51, keep = TRUE)) %>%
        fortify %>%
        filter(z < 0 & z > -1500)

    ## this does the magic for geom_map
    state_map <- fortify(state)

    if (!is.null(limits)) {
        limits <- c(1, limits)
        mid_point <-  log(quantile(seq(min(gg_r$value),
                                       max(limits),
                                       by = 1), .02))
    } else {
        mid_point <-  log(quantile(seq(min(gg_r$value),
                                       max(gg_r$value),
                                       by = 1), .02))
    }

    ggplot() +
        geom_raster(data = gg_r, aes(x = x, y = y, fill = value)) +
        scale_fill_gradient2(low = "#5E98AE", mid = "#E3C94A", high = "#D5331E",
                             midpoint = mid_point,
                             breaks = c(1, 10, 100, 1000, 5000), trans = "log",
                             limits = limits) +
        geom_map(data=state_map, map=state_map,
                 aes(x=long, y=lat, map_id=id),
                 fill="gray20", colour = "gray20", size = .05) +
        geom_contour(data = us_bathy, aes(x = x, y = y, z = z),
                     colour = "gray80", binwidth = 500, size = .1) +
        coord_quickmap(xlim = c(-128, -60), ylim = c(22, 51)) +
        #scale_fill_viridis(trans = "log", breaks = c(1, 10, 100, 1000, 10000)) +
        theme_bw(base_family = "Ubuntu Condensed") +
        theme(legend.title = element_blank()) +
        ggtitle(title) +
        xlab("Longitude") + ylab("Latitude")
}

make_heatmap_by_phylum <- function(idig, file = "figures/map_diversity_per_phylum.pdf") {
    uniq_phyla <- unique(idig$phylum)

    res <- parallel::mclapply(uniq_phyla, function(p) {
                         idig_sub <- idig[idig$phylumrg == p, ]
                         if (nrow(idig_sub) < 10) return(NULL)
                         ggr <- make_data_map_diversity(idig_sub)
                         ggr
                     }, mc.cores = 8L)
    has_data <- !vapply(res, is.null, logical(1))
    res <- res[has_data]
    max_limit <- dplyr::bind_rows(res) %>%
        max(.$value)
    names(res) <- uniq_phyla[has_data]
    pmaps <- parallel::mclapply(seq_along(res),
                       function(gg) {
                           make_heatmap_sampling(res[[gg]], names(res)[gg],
                                                 limits = max_limit)
                  }, mc.cores = 8L)
    pdf(file = file)
    on.exit(dev.off())
    for (i in seq_along(pmaps)) {
       print( pmaps[[i]])
    }
}

idigbio_parse_year <- function(idig) {
    idig %>%
        dplyr::mutate(parsed_date = parse_date_time(datecollected, c("Y", "ymd", "ym", "%Y-%m-%d%H:%M:%S%z"))) %>%
        dplyr::mutate(year = year(parsed_date)) %>%
        dplyr::mutate(year = replace(year, year > 2016 | year < 1800, NA)) %>%
        dplyr::filter(!is.na(year))
}

make_plot_idigbio_records_per_date <- function(idig, to_keep = c("Echinodermata", "Annelida", "Arthropoda", "Mollusca", "Porifera")) {
    idig %>%
        filter(is_marine == TRUE) %>%
        idigbio_parse_year() %>%
        mutate(`data.dwc:phylum` = capitalize(`data.dwc:phylum`, strict = TRUE)) %>%
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
