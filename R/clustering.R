make_species_site_matrix <- function(recs, raster) {
    recs %>%
        dplyr::filter(rank == "Species" | rank == "Subspecies") %>%
        add_rastercell(raster) %>%
        dplyr::select(worms_valid_name, rastercell) %>%
        dplyr::distinct(worms_valid_name, rastercell, .keep_all = TRUE) %>%
        dplyr::mutate(present = 1) %>%
        tidyr::spread(worms_valid_name, present, fill = 0L) %>%
        dplyr::mutate_at(vars(matches("[0-9]+")), as.integer)
}


make_distance_species_site_matrix <- function(mat) {
    ## keep only sites with more than 10 species
    ## keep only species observed at 5 sites
    ## first column is site names
    sub_mat <- mat[, c(FALSE, colSums(mat[, -1]) >=  5)]
    to_keep <- rowSums(sub_mat) >= 10
    sub_mat <- sub_mat[to_keep, ]
    sites <- mat[to_keep, 1]

    ## calculate jaccard matrix
    jac_mat <- vegan::vegdist(sub_mat, method="jaccard")
    attr(jac_mat, "sites") <- sites
    jac_mat
}

get_dunn_indices <- function(dmat) {

    methods <-  c("ward.D", "ward.D2","single", "complete", "average",
                  "mcquitty", "median", "centroid")
    res <- vector("list", length(methods))
    for (i in seq_along(methods)) {
        ## build hierarchical cluster
        hc <-  hclust(dmat, method = methods[i])
        dunn_i <- numeric(100)

        for (k in seq(from = 5, by = 1, length.out = 100)) {
            tc <- cutree(hc, k = k)
            dunn_i[k - 4] <- clValid::dunn(distance = dmat,
                                            clusters = tc)
        }
        res[[i]] <- dunn_i
    }
    names(res) <- methods
    res
}

## From this:
## di <- get_dunn_indices(distance_site_matrix)
## as_tibble(di) %>% gather(method,  distance) %>%
##     group_by(method) %>%
##     mutate(k = seq(5, by=1, length.out=100)) %>%
##     ggplot(aes(x=k, y=distance, colour=method)) +
##     geom_line()
##
## it seems that single method with few k is better however, this is largely
## driven by a few outliers that force a cluster with most observations. Using
## the complete method, with a relatively high number of cluster produces more
## biologically realistic results.


assign_clusters_to_cells <- function(dmat, k, raster) {

    hc <- hclust(dmat, method = "complete")
    sites <- attr(dmat, "sites")

    tc <- cutree(hc, k = k)
    res <- dplyr::bind_cols(sites, cluster = tc) %>%
        ## dplyr::group_by(cluster) %>%
        ## dplyr::mutate(n_cells_in_cluster = n()) %>%
        ## dplyr::filter(n_cells_in_cluster > 3) %>%
        dplyr::mutate(coords = map(rastercell, function(x) {
        .r <- raster::xyFromCell(raster, x)
        tibble::tibble(x=.r[[1]], y=.r[[2]])
    })) %>%
        tidyr::unnest(coords)
}


plot_map_clusters <- function(res) {
        state <- maps::map("world", fill = TRUE, plot = FALSE)

        ## convert the 'map' to something we can work with via geom_map
        IDs <- sapply(strsplit(state$names, ":"), function(x) x[1])
        state <- map2SpatialPolygons(state, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))

        us_bathy <- suppressMessages(getNOAA.bathy(lon1 = -128, lon2 = -60,
                                               lat1 = 22, lat2 = 51,
                                               keep = TRUE)) %>%
        fortify() %>%
        filter(z < 0 & z > -1500)

        state_map <- fortify(state)

        ggplot(res) +
            geom_raster(aes(x=x, y=y, fill=as.factor(cluster))) +
            scale_fill_viridis(guide = FALSE, option="viridis", discrete=TRUE)  +
            geom_map(data = state_map, map = state_map,
                     aes(map_id = id), fill = "gray20", colour = "gray20",
                     size = .05, inherit.aes = FALSE) +
            geom_contour(data = us_bathy, aes(x = x, y = y, z = z),
                         colour = "gray30", binwidth = 500, size = .1,
                         inherit.aes = FALSE) +
            coord_quickmap(xlim = c(-128, -60), ylim = c(22, 51)) +
            theme_bw(base_family = "Ubuntu Condensed") +
            theme(legend.title = element_blank()) +
            xlab("Longitude") + ylab("Latitude")

}
