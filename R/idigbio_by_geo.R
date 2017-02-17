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
               taxon_name = `data.dwc:phylum`) %>%
        distinct(uuid, .keep_all = TRUE) %>%
        add_worms()

    res
}
