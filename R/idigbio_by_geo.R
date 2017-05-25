## generate the geographic grid from a SpatialPolygons object
get_bounding_box <- function(map, cellsize = .5) {
    box <- sp::bbox(map)
    gt <- sp::GridTopology(c(box[1,1], box[2,1]),
                           cellsize = rep(cellsize, 2), rep(100, 2))
    gr <- as(as(sp::SpatialGrid(gt, CRS(proj4string(map))), "SpatialPixels"),
             "SpatialPolygons")
    it <- rgeos::gIntersects(map, gr, byid = TRUE)
    gr[it]
}

## convert the SpatialGrid object into a data frame that contains the
## coordinates of the corners of square of the geographic grid
bb_to_df <- function(bb) {
    res <- lapply(seq_along(bb), function(i) {
        data.frame(
            xmin = xmin(bb[i]), ymax = ymax(bb[i]),
            xmax = xmax(bb[i]), ymin = ymin(bb[i])
        )
    })
    res <- dplyr::bind_rows(res)
    res <- mutate_all(res, round, digits = 3)
    res <- mutate(res, key = paste(xmin, ymax, xmax, ymin, sep = "|"))
    res
}

## from the map of the EEZ from the USA, create a data frame that
## contains the coordinates for a grid that spans the area. This grid
## will be used to do an iDigBio search by geography
generate_bounding_boxes <- function(map_usa, cellsize = .5) {

    map_usa_sp_df <- geojson_sp(map_usa)

    map_usa_fort <- ggplot2::fortify(map_usa_sp_df)
    res <- lapply(levels(map_usa_fort$piece), function(i) {
        SpatialPolygons(
            list(
                Polygons(
                    list(Polygon(subset(map_usa_fort, piece == i)[, c("long", "lat")])),
                    ID = 1)
            ), proj4string = CRS(proj4string(map_usa_sp_df)))
    }) %>%
        lapply(get_bounding_box, cellsize = cellsize) %>%
        lapply(bb_to_df)
    names(res) <- levels(map_usa_fort$piece)

    dplyr::bind_rows(res, .id = "piece")
}


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
make_hook_idigbio_by_geo <- function(coords_qry) {
    force(coords_qry)
    function(key, namespace) {
        message("... ", appendLF = FALSE)
        ridigbio::idig_search_records(rq = coords_qry[[key]], fields = idigbio_fields())
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
                map(function(x) set_names(x, c("xmin", "ymax", "xmax", "ymin"))) %>%
                map(as.list) %>%
                map_df(as_tibble) %>%
                bind_cols(data_frame(key = rep(q, 4))) %>%
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
## records they contain
fill_store_idigbio_by_geo <- function(coords, map_usa, use_cache) {

    idig_types <- structure(c("TEXT", "TEXT", "TEXT", "TEXT", "TEXT",
                              "TEXT", "TEXT", "TEXT", "TEXT", "TEXT",
                              "TEXT", "TEXT", "REAL", "REAL", "TEXT",
                              "TEXT", "TEXT"),
                            .Names = c("uuid", "catalognumber",
                                  "datecollected", "institutioncode",
                                  "phylum", "data.dwc:phylum",
                                  "data.dwc:class", "data.dwc:order",
                                  "data.dwc:family", "data.dwc:genus",
                                  "scientificname", "country",
                                  "geopoint.lon", "geopoint.lat",
                                  "clean_phylum", "clean_class", "clean_family"))

    idig_data_db <- dplyr::src_postgres("idigbio", host = "localhost",
                                        user = "marinediversity",
                                        password = "password")#dplyr::src_sqlite(db, create = TRUE)
    con <- idig_data_db$con
    db_begin(con)
    on.exit(db_rollback(con))
    if (db_has_table(con, "idigbio_data"))
        db_drop_table(con, "idigbio_data")

    db_create_table(con, "idigbio_data", types = idig_types, temporary = FALSE)

    ## if use_cache=FALSE, destroy the storr before fetching the
    ## results from iDigBio, otherwise, we'll use the cached results
    if (! use_cache)
        store_idigbio_by_geo(coords)$destroy()

    lapply(names(coords), function(q) {
        message("Getting iDigBio records for ", q, appendLF = FALSE)
        r <- get_idigbio_by_geo(coords, q) %>%
            mutate_(.dots = setNames(list("tolower(`data.dwc:phylum`)",
                                          "tolower(`data.dwc:class`)",
                                          "tolower(`data.dwc:family`)"),
                                     c("clean_phylum", "clean_class", "clean_family"))) %>%
            ## First let's get the phylum names from the Gulf of Mexico list,
            ## that will take care of plants, fungi, and records with no
            ## specified phylum
            filter(clean_phylum %in% gom_phyla())
        message(" DONE")
        db_insert_into(con, "idigbio_data", r)
    })
    db_create_indexes(con, "idigbio_data", indexes = list(c("clean_phylum", "clean_class", "clean_family"),
                                                          c("country")))
    db_analyze(con, "idigbio_data")
    db_commit(con)
    on.exit(NULL)

    ## We only want:
    ## - unique records
    ## - invertebrates
    ## - marine
    ## - within the EEZ boundaries

    db <- idig_data_db %>%
        dplyr::tbl("idigbio_data")

    arth_class_to_rm <- arthropod_classes_to_rm()
    chr_class_to_rm <- chordata_classes_to_rm()
    chr_fam_to_rm <- chordata_families_to_rm()

    arth_family_to_rm <- db %>%
        filter(clean_phylum == "arthropoda" & clean_class %in% arth_class_to_rm) %>%
        select(clean_phylum, clean_family) %>%
        distinct(clean_phylum, clean_family) %>%
        filter(!is.na(clean_family))

    chordata_family_to_rm <- db %>%
        filter(clean_phylum == "chordata" & (clean_class %in% chr_class_to_rm |
                                             clean_family %in% chr_fam_to_rm)) %>%
        select(clean_phylum, clean_class, clean_family) %>%
        distinct(clean_phylum, clean_class, clean_family)

    res <- db %>%
        ## only the phyla found in the gulf of mexico list
        filter(country != "canada") %>%
        ## only the obviously non-marine arthropods and the vertebrates
        anti_join(arth_family_to_rm, by = c("clean_phylum", "clean_family")) %>%
        anti_join(chordata_family_to_rm, by = c("clean_phylum", "clean_family")) %>%
        anti_join(chordata_family_to_rm, by = c("clean_phylum", "clean_class")) %>%
        collect(n = Inf) %>%
        distinct(uuid, .keep_all = TRUE) %>%
        mutate(cleaned_scientificname = cleanup_species_names(scientificname),
               is_binomial = is_binomial(cleaned_scientificname),
               rank = rep("phylum", n()),
               taxon_name = clean_phylum) %>%
        add_worms()  %>%
        dplyr::rename(decimallatitude = geopoint.lat,
                      decimallongitude = geopoint.lon) %>%
        is_in_eez_records(map_usa) %>%
        dplyr::filter(is_in_eez == TRUE)

    res
}


## select only the the records from the Gulf of Mexico from all the
## iDigBio records. It might be more efficient to do it earlier in the
## pipeline, but doing it there ensures that we are working with
## cleaned/pre-processed data.
select_gom_from_idigbio <- function(idig) {
    idig[is_in_gulf_of_mexico(idig$decimallongitude, idig$decimallatitude), ]
}


select_koz_from_idigbio <- function(idig) {
    idig[is_in_pnw(idig$decimallongitude, idig$decimallatitude), ]
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
        mutate(phylum = tolower(taxon_name)) %>%
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
        theme_bw() +
        theme(legend.title = element_blank()) +
        ggtitle(title) +
        xlab("Longitude") + ylab("Latitude")
}

make_heatmap_by_phylum <- function(idig, file = "figures/map_diversity_per_phylum.pdf") {
    uniq_phyla <- unique(idig$clean_phylum)

    res <- parallel::mclapply(uniq_phyla, function(p) {
                         idig_sub <- idig[idig$clean_phylum == p, ]
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
