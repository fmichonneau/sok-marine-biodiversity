

get_bounding_box <- function(map) {
    box <- sp::bbox(map_usa_west_sp)
    gt <- sp::GridTopology(c(box[1,1], box[2,1]), rep(1, 2), rep(20, 2))
    gr <- as(as(sp::SpatialGrid(gt), "SpatialPixels"), "SpatialPolygons")
    proj4string(gr) <- CRS(proj4string(map))
    it <- rgeos::gIntersects(map_usa_west_sp, gr, byid = TRUE)
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

    map_usa_sp_df <- geojson_sp(tt)

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
