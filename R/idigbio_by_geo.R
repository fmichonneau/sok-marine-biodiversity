## generate the geographic grid from a SpatialPolygons object
get_bounding_box <- function(map) {
    box <- sp::bbox(map)
    gt <- sp::GridTopology(c(box[1,1], box[2,1]), rep(.5, 2), rep(100, 2))
    gr <- as(as(sp::SpatialGrid(gt), "SpatialPixels"), "SpatialPolygons")
    proj4string(gr) <- CRS(proj4string(map))
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
generate_bounding_boxes <- function(map_usa) {

    map_usa_sp_df <- geojson_sp(map_usa)

    map_usa_fort <- ggplot2::fortify(map_usa_sp_df)

    map_usa_east_sp <-
        SpatialPolygons(
            list(
                Polygons(
                    list(Polygon(subset(map_usa_fort, piece == 1)[, c("long", "lat")])),
                    ID = 1)
            ), proj4string = CRS(proj4string(map_usa_sp_df)))

    map_usa_west_sp <-
        SpatialPolygons(
            list(
                Polygons(
                    list(Polygon(subset(map_usa_fort, piece == 2)[, c("long", "lat")])),
                    ID = 1)
            ), proj4string = CRS(proj4string(map_usa_sp_df)))

    east_bb <- get_bounding_box(map_usa_east_sp)
    west_bb <- get_bounding_box(map_usa_west_sp)
    east_df <- bb_to_df(east_bb)
    west_df <- bb_to_df(west_bb)

    dplyr::bind_rows(list(east = east_df, west = west_df), .id = "coast")
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


## fetch hook for the idigbio by geography storr. The use a closure to
## enforce the remake dependency on the data frame that contains the
## coordinates for each element of the grid.
make_hook_idigbio_by_geo <- function(coords_qry) {
    force(coords_qry)
    function(key, namespace) {
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

## for all the coordinates of the bounding boxes, find the iDigBio
## records they contains
fill_store_idigbio_by_geo <- function(map_usa, use_cache) {
    bb_eez <- generate_bounding_boxes(map_usa)
    coords <- coords_to_query(bb_eez)

    ## if use_cache=FALSE, destroy the storr before fetching the
    ## results from iDigBio, otherwise, we'll use the cached results
    if (! use_cache)
        store_idigbio_by_geo(coords)$destroy()

    lapply(names(coords), function(q)
        store_idigbio_by_geo(coords)$get(q))
}

cleanup_idigbio_raw <- function(idig) {

    ## We only want:
    ## - unique records
    ## - invertebrates
    ## - marine

    ## First let's get the phylum names from the Gulf of Mexico list,
    ## that will take care of plants, fungi, and records with no
    ## specified phylum
    taxa <- names(gom_taxa_to_keep())
    taxa <- strsplit(taxa, "-")
    taxa <- vapply(taxa, function(x) x[2], character(1))
    taxa <- tolower(unique(taxa))

    res <- idig %>%
        dplyr::bind_rows()


    res_ <- res %>%
        mutate_(.dots = setNames(list("tolower(`data.dwc:phylum`)",
                                      "tolower(`data.dwc:class`)",
                                      "tolower(`data.dwc:family`)"),
                                 c("clean_phylum", "clean_class", "clean_family")))

    arth_classes_to_rm <- c("arachnida", "myriapoda", "protura", "symphyla", "chilopoda",
                            "diplopoda", "hexapoda", "insecta", "trilobita", "unknown")

    arth_family_to_rm <- res_ %>%
        filter(clean_phylum == "arthropoda" & clean_class %in% arth_classes_to_rm) %>%
        select(clean_family) %>%
        distinct() %>%
        .[[1]] %>%
        na.omit()

    chordata_classes_to_rm <- c("actinopteri", "actinopterygii",
                                "cephalaspidomorphi", "agnatha",
                                "amphibia", "aves", "chondrichthyes",
                                "chondrichthys", "elasmobranchii",
                                "holocephali", "mammalia", "myxini",
                                "osteichthyes", "osteichthyes",
                                "petromyzonti", "pisces", "reptilia", "unknown")


    chordata_family_to_rm <- res_ %>%
        filter(clean_phylum == "chordata" & clean_class %in% chordata_classes_to_rm) %>%
        select(clean_family) %>%
        distinct() %>%
        .[[1]] %>%
        na.omit()

    chordata_family_to_rm <- c(chordata_family_to_rm, "anarhichantidae", "anthiidae",
                               "belontiidae", "branchiostegidae", "denticipitidae",
                               "doliolunidae", "eleotrididae", "fritillariidae",
                               "gobioididae", "grammistidae", "icelidae", "idiacanthidae",
                               "macrorhamphosidae", "zaniolepidae", "zaniolepididae")

    res. <- res_ %>%
        ## only the phyla found in the gulf of mexico list
        filter(clean_phylum %in%  taxa) %>%
        ## only the obviously non-marine arthropods and the vertebrates
        filter(! (clean_phylum == "arthropoda" &
                  (clean_class %in% arth_classes_to_rm | clean_family %in% arth_family_to_rm)),
               ! (clean_phylum == "chordata" &
                  (clean_class %in% chordata_classes_to_rm | clean_family %in% chordata_family_to_rm))
               ) %>%
        filter(country != "canada")

    res <- res. %>%
        mutate(cleaned_scientificname = cleanup_species_names(scientificname),
               is_binomial = is_binomial(cleaned_scientificname),
               rank = rep("phylum", nrow(.)),
               taxon_name = tolower(`data.dwc:phylum`)) %>%
        distinct(uuid, .keep_all = TRUE) %>%
        add_worms()

    res
}


## select only the the records from the Gulf of Mexico from all the
## iDigBio records. It might be more efficient to do it earlier in the
## pipeline, but doing it there ensures that we are working with
## cleaned/pre-processed data.
select_gom_from_idigbio <- function(idig) {
    ## coordinates used by Felder et al, but in our case, it will
    ## cover a smaller area as the iDigBio query currently only
    ## applies to EEZ waters
    res <- idig[(idig$geopoint.lon > -100 & idig$geopoint.lon < -80.5) &
                (idig$geopoint.lat > 17 & idig$geopoint.lat < 31), ]
    ## remove Jacksonville area points
    res[!(res$geopoint.lon > -82 & res$geopoint.lat > 27), ]
}


select_koz_from_idigbio <- function(idig) {
    idig[(idig$geopoint.lon > -125 & idig$geopoint.lon < -122) &
        (idig$geopoint.lat > 47 & idig$geopoint.lat < 49), ]
}


## Generate a data frame for species that occur in iDigBio but are not
## listed in the Gulf of Mexico checklist or the Kozloff
diff_list_idigbio <- function(idig, check_list) {
    idig %>%
        dplyr::anti_join(check_list, by = "worms_valid_name") %>%
        dplyr::filter(is_binomial == TRUE, is_marine == TRUE)##  %>%
        ## dplyr::distinct(worms_valid_name, .keep_all = TRUE) %>%
        ## dplyr::count(clean_phylum)
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
    pts <- SpatialPoints(data.frame(lon = idig$geopoint.lon,
                                    lat = idig$geopoint.lat))
    r <- rasterize(pts, us_raster, fun = "count")
    gg_r <- as.data.frame(as(r, "SpatialPixelsDataFrame"))
    colnames(gg_r) <- c("value", "x", "y")
    gg_r
}


make_data_map_diversity <- function(idig) {
    us_raster <- us_raster()
    raster_cell <- mapply(function(x, y) cellFromXY(us_raster, c(x, y)),
                          idig$geopoint.lon, idig$geopoint.lat)

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



compare_records <- function(idig_gom, gom_wrm,
                            idig_koz, koz_wrm) {
    diff_gom <- diff_list_idigbio(idig_gom, gom_wrm)
    diff_koz <- diff_list_idigbio(idig_koz, koz_wrm)

    gom_wrm <- dplyr::mutate(gom_wrm, `data.dwc:phylum` = tolower(taxon_name))
    koz_wrm <- dplyr::mutate(koz_wrm, `data.dwc:phylum` = tolower(taxon_name))

    res <- purrr::map_df(
                      list(diff_gom = diff_gom,
                           list_gom = gom_wrm,
                           idig_gom = idig_gom,
                           diff_koz = diff_koz,
                           list_koz = koz_wrm,
                           idig_koz = idig_koz),
                      n_spp_from_idigbio, .id = "source") %>%
        tidyr::spread(source, n_spp) %>%
        dplyr::mutate(
                   ymin_list_gom = -diff_gom,
                   ymax_list_gom = list_gom - diff_gom,
                   ymin_list_koz = -diff_koz,
                   ymax_list_koz = list_koz - diff_koz
               ) %>%
        dplyr::select(phylum, starts_with("y"), starts_with("idig")) %>%
        dplyr::mutate(ymin_idig_gom = rep(0, nrow(.)),
                      ymin_idig_koz = rep(0, nrow(.))) %>%
        dplyr::rename(ymax_idig_gom = idig_gom,
                      ymax_idig_koz = idig_koz) %>%
        tidyr::gather(source, n_spp, -phylum) %>%
        tidyr::extract(source, into = c("coord", "data_source", "location"),
                       regex = "([a-z]+)_([a-z]+)_([a-z]+)") %>%
        tidyr::spread(coord, n_spp)

    res
}

plot_compare_records <- function(rec) {

    rec <- rec[(!(is.na(rec$ymin) & is.na(rec$ymax))), ]

    to_keep <- rec$phylum[rec$ymax > 100]
    to_keep <- unique(to_keep)
    rec <- rec[complete.cases(rec), ]
    rec <- rec[rec$phylum %in% to_keep, ]
    rec$phylum <- factor(rec$phylum)
    ggplot(rec, aes(x = phylum, ymin = ymin, ymax = ymax, color = data_source)) +
        geom_linerange(size = 5, position = position_dodge(width = .35)) +
        coord_flip() +
        facet_grid(. ~ location)

}


