add_environmental_data <- function(d, raster,
                                   env_data_file = "data-raw/environmental-layers/hcaf_v6.csv") {

    env_data <- readr::read_tsv(env_data_file, col_types = env_data_col_types())

    ## add raster cells from 0.5 USA raster grid
    ## and filter out records that are not part of it
    env_data_grid <- env_data %>%
        dplyr::rename(decimallatitude = CenterLat,
                      decimallongitude = CenterLong) %>%
        add_rastercell(raster) %>%
        dplyr::filter(!is.na(rastercell)) %>%
        ## select:
        ## - areas at depth
        ## - elevation data (evalation above sea level)
        ## - depth data
        ## - SST and Bottom temperature
        ## - salinity
        ## - primary productivity
        ## - distance from coast
        dplyr::select(rastercell,
                      starts_with("Area"), starts_with("Elevation"),
                      starts_with("Depth"), starts_with("SSTAn"),
                      starts_with("SSTMn"),
                      starts_with("SBTAn"), starts_with("SalinityM"),
                      starts_with("SalinityBM"),
                      starts_with("PrimProdM"), LandDist, WaveHeight,
                      TidalRange, Shelf, Slope, Abyssal, Coral, Estuary,
                      Seamount)

    dplyr::left_join(d, env_data_grid, by = "rastercell")

}

assemble_keras_data <- function(recs, raster) {

    res <- recs %>%
        dplyr::filter(rank == "Species" | rank == "Subspecies") %>%
        add_rastercell(raster) %>%
        dplyr::group_by(rastercell) %>%l
        dplyr::summarize(
            n_spp = n_distinct(worms_valid_name),
            n_samp = n()
            ) %>%
        add_environmental_data(raster = raster) %>%
        dplyr::mutate(coords = map(rastercell, function(x) {
            .r <- raster::xyFromCell(raster, x)
            tibble::tibble(x=.r[[1]], y=.r[[2]])
        })) %>%
        tidyr::unnest()
    ## only a few cells have missing data, let's remove them for now
    res[complete.cases(res), ]
}

kd_normalize <- function(x) {
    if (any(is.na(x))) stop("missing values detected")
    (x - min(x))/(max(x) - min(x))
}

kd_denormalize <- function(x, orig_min_x, orig_max_x) {
    x * (orig_max_x - orig_min_x) + orig_min_x
}

prepare_keras_data <- function(kd) {
    kd <- kd %>%
        dplyr::select(-Abyssal)

    undersampled_criterion <- kd$n_samp/kd$n_spp < 4

    kd_undersampled <- kd[undersampled_criterion, ]

    ## normalize all values
    kd_norm <- kd %>%
        filter(!undersampled_criterion) %>%
        dplyr::select(-rastercell, -n_samp) %>%
        as.matrix()

    ## split in training and testing
    set.seed(10101)
    tort <- sample(2, nrow(kd_norm), replace = TRUE, prob = c(.67, .33))

    ## number of species in first column
    kd_norm_train <- kd_norm[tort == 1L, -1]
    kd_norm_test <- kd_norm[tort == 2L, -1]

    kd_norm_train_lbls <- kd_norm[tort == 1L, 1]
    kd_norm_test_lbls <- kd_norm[tort == 2L, 1]

    list(
        kd_undersampled = kd_undersampled,
        kd_norm_train = kd_norm_train,
        kd_norm_test = kd_norm_test,
        kd_norm_train_lbls = kd_norm_train_lbls,
        kd_norm_test_lbls = kd_norm_test_lbls
    )
}

plot_under_sampled <- function(res) {
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
            geom_raster(aes(x=x, y=y, fill=ratio)) +
            scale_fill_viridis(guide = FALSE, option="plasma")  +
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


if (FALSE) {

    library(keras)
    model <- keras::keras_model_sequential()

    model %>%
        layer_dense(units = 50, activation = "relu", input_shape = 35) %>%
        layer_dense(units = 1, kernel_initializer="normal")

    ## compilation
     model %>%
        compile(
            loss = "mean_squared_error",
            optimizer = "adam",
            metrics= "accuracy"
        )

    ## fit
    history <- model %>%
        fit(
            scale(kd$kd_norm_train),
            kd$kd_norm_train_lbls,
            epochs = 300
        )

    ## prediction
    kd_preds <- model %>%
        predict(scale(kd$kd_norm_test))

    plot(kd$kd_norm_test_lbls, kd_preds, xlab = "Number of species (test set)",
         ylab = "Predicted number of species")
    abline(b = 1, a = 0)

    ## model undersampled areas
    kd_under <- kd$kd_undersampled %>%
        dplyr::select(-rastercell, -n_samp)

    kd_pred_under <- model %>%
        predict(as.matrix(scale(kd_under[, -1])))

    keras_under_preds <- bind_cols(
        pred_n_spp = kd_pred_under,
        kd$kd_undersampled
    ) %>%
        dplyr::rename(obs_n_spp = n_spp) %>%
        dplyr::mutate(ratio = pred_n_spp/obs_n_spp,
                      coords = map(rastercell, function(x) {
                          .r <- raster::xyFromCell(eez_raster_05, x)
                          tibble::tibble(x=.r[[1]], y=.r[[2]])
                      })) %>%
        tidyr::unnest(coords)

    plot_under_sampled(keras_under_preds)
    ggsave(file = "figures/keras_predictions.pdf", width = 11, height = 8)

    ## With using GLM ----------------------------------------------------------

    mdl_train_data <- bind_cols(n_spp = kd$kd_norm_train_lbls,
                                atl_pac = ifelse(kd$kd_norm_train[, "x"] < -100, "pac", "atl"),
                                as_tibble(scale(kd$kd_norm_train)))
    mdl_test_data <- bind_cols(n_spp = kd$kd_norm_test_lbls,
                                atl_pac = ifelse(kd$kd_norm_test[, "x"] < -100, "pac", "atl"),
                               as_tibble(scale(kd$kd_norm_test)))

    mdl_glm <- glm(n_spp ~ . -SSTMnRange
                   - ElevationMin -ElevationMax -ElevationMean -ElevationSD
                   -Area0_20 -Area20_40 -Area40_60 -Area60_80 -Area80_100
                   -AreaBelow100 -WaveHeight -TidalRange
                   -SalinityMin -SalinityMax -SalinityMean -SalinityBMean
                   -DepthMin -DepthMax -DepthMean -DepthSD -Estuary
                   -SSTAnSD -Seamount -x
                 , data = mdl_train_data, family = "quasipoisson")

    pred_mdl <- predict(mdl_glm, mdl_test_data, type = "response")

    plot(kd$kd_norm_test_lbls, pred_mdl, xlab = "Number of species (test set)",
         ylab = "Predicted number of species")
    abline(b = 1, a = 0)

    under_smpld <- bind_cols(n_spp = kd$kd_undersampled$n_spp,
                             atl_pac = ifelse(kd$kd_undersampled$x < -100, "pac", "atl"),
                             as_tibble(scale(kd$kd_undersampled[, -c(1, 2)])),
                             rastercell = kd$kd_undersampled$rastercell)
    pred_under <- predict(mdl_glm, under_smpld, type = "response")


    glm_under_preds <- tibble(rastercell = under_smpld$rastercell,
                            obs_n_spp = under_smpld$n_spp,
                            pred_n_spp = pred_under) %>%
        dplyr::mutate(ratio = pred_n_spp/obs_n_spp,
                      coords = map(rastercell, function(x) {
                          .r <- raster::xyFromCell(eezx_raster_05, x)
                          tibble::tibble(x=.r[[1]], y=.r[[2]])
                      })) %>%
        tidyr::unnest(coords)

    plot_under_sampled(glm_under_preds)
    ggsave(file = "figures/glm_predictions.pdf", width = 11, height = 8)


}


