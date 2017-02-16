

get_bounding_box <- function(map) {
    box <- sp::bbox(map)
    gt <- sp::GridTopology(c(box[1,1], box[2,1]), rep(.5, 2), rep(100, 2))
    gr <- as(as(sp::SpatialGrid(gt), "SpatialPixels"), "SpatialPolygons")
    proj4string(gr) <- CRS(proj4string(map))
    it <- rgeos::gIntersects(map, gr, byid = TRUE)
    gr[it]
}

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

coords_to_query <- function(coords) {
    qry <- lapply(seq_len(nrow(coords)), function(i) {
        list(basisofrecord = "PreservedSpecimen",
             scientificname = list(type = "exists"),
             geopoint = list(
                 type = "geo_bounding_box",
                 top_left = list(lon = coords$xmin[i], lat = coords$ymax[i]),
                 bottom_right = list(lon = coords$xmax[i], lat = coords$ymin[i])
             ))
    })
    names(qry) <- coords$key
    qry
}


make_hook_idigbio_by_geo <- function(coords_qry) {
    force(coords_qry)
    function(key, namespace) {
        ridigbio::idig_search_records(rq = coords_qry[[key]], fields = idigbio_fields())
    }
}


store_idigbio_by_geo <- function(coords, store_path = "data/idigbio_by_geo") {
    fetch_hook_idigbio_by_geo <- make_hook_idigbio_by_geo(coords)
    storr::storr_external(storr::driver_rds(store_path),
                          fetch_hook_idigbio_by_geo)
}

fill_store_idigbio_by_geo <- function(map_usa) {
    bb_eez <- generate_bounding_boxes(map_usa)
    coords <- coords_to_query(bb_eez)
    lapply(names(coords), function(q)
        store_idigbio_by_geo(coords)$get(q))
}
