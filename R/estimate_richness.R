
raster_from_map_res <- function(map, res) {
  bbox <- round(lawn::lawn_bbox(map), 0)
  ## let's make the bounding box a little larger to accommodate rounding:
  raster::raster(
    vals = NA,
    xmn = bbox[1] - 1, ymn = bbox[2] - 1,
    xmx = bbox[3] + 1, ymx = bbox[4] + 1,
    res = res
  )
}

## seq_res: sequence of resolution. Default for paper is 0.2x0.2°
list_rasters <- function(map, seq_res = seq(0.2, 2, by = .2)) {
  setNames(
    lapply(seq_res, function(r) raster_from_map_res(map, r)),
    paste0("res_", seq_res)
  )
}

##
add_cell_id <- function(recs, lr) {
  res <- map_df(lr, .f = function(r) {
    mapply(
      function(x, y) cellFromXY(r, c(x, y)),
      recs$decimallongitude, recs$decimallatitude
    )
  })

  dplyr::bind_cols(recs, res)
}


species_per_cell <- function(recs_cell) {
  all_resolutions <- dplyr::starts_with("res_", vars = names(recs_cell))

  lapply(all_resolutions, function(i) {
    sub_recs <- dplyr::select(recs_cell, phylum, worms_valid_name, i)
    grp <- names(recs_cell)[i]
    sub_recs %>%
      dplyr::group_by(!!!rlang::syms(grp)) %>%
      dplyr::summarize(n_species = dplyr::n_distinct(phylum, worms_valid_name))
  })
}

sample_richness_in_cells <- function(cells, lr, n_samples = 200) {
  area_from_resolution <- function(r) {
    vapply(
      r, function(.r) mean(raster::area(lr[[paste0("res_", .r)]])@data@values),
      double(1)
    )
  }

  res <- lapply(cells, function(x) {
    sample(x$n_species[!is.na(x[, 1])],
      size = n_samples,
      replace = TRUE
    )
  }) %>%
    tibble()
  names(res) <- "richness"
  tibble(
    resl = seq(0.2, 2, by = .2),
    surf = area_from_resolution(resl)
  ) %>%
    bind_cols(sample_richness = res) %>%
    unnest()
}


fit_mdl <- function(d) {
  sar_nls <- nls(mean_spp ~ a * surf^z,
    start = list(a = 5, z = .2)
  )
}


###
if (FALSE) {
  tt <- us_sampled_richness
  rr <- tt %>%
    group_by(surf) %>%
    summarize(mean_spp = mean(richness))
  gom_sar_nls <- nls(richness ~ a * surf^z, data = tt, start = list(a = 5, z = .2))
  preds <- tibble(surf = 1:1e6)
  preds$n_spp <- predict(gom_sar_nls, preds)

  plot(tt$surf, tt$richness)
  lines(preds$surf, preds$n_spp, col = "blue")
}


###### other approach

cells_to_pool <- function(cell_ids) {
  n_rep <- 200
  n_sizes <- 9
  seq_size <- seq_len(n_sizes) - 1
  res <- lapply(seq_size, function(i) {
    setNames(
      replicate(n_rep, sample(cell_ids, size = 2^i, replace = TRUE),
        simplify = FALSE
      ),
      paste0("rep_", seq_len(n_rep))
    )
  })
  names(res) <- seq_size
  res
}


spp_from_cells <- function(recs, rstr) {
  recs <- dplyr::mutate(recs,
    cell = mapply(
      function(x, y) cellFromXY(rstr, c(x, y)),
      recs$decimallongitude, recs$decimallatitude
    )
  )
  cell_to_sample <- unique(recs$cell)
  cells <- cells_to_pool(cell_to_sample)

  raster_area <- raster::area(rstr)

  res <- map_df(cells, function(x) {
    n_spp_df <- purrr::map(x, function(.c) {
      dplyr::filter(recs, cell %in% .c) %>%
        distinct(worms_valid_name) %>%
        nrow()
    }) %>%
      tibble(
        n_spp = .
      ) %>%
      unnest() %>%
      dplyr::mutate(
        reps = seq_along(x),
        n_cell = length(x[[1]])
      )
    area_df <- purrr::map(x, function(.c) {
      sum(raster_area@data@values[.c])
    }) %>%
      tibble(
        area = .
      ) %>%
      unnest()
    dplyr::bind_cols(n_spp_df, area_df)
  })
  res
}

### playground

if (FALSE) {
  gom_sp <- spp_from_cells(combined_gom_records, gom_raster)
  gom_sar_nls <- nls(n_spp ~ a * area^z, data = gom_sp, start = list(a = 5, z = .2))
  predict(gom_sar_nls, data.frame(area = lawn_area(map_gom) / 1e6))

  ## area too small for parameters used
  pnw_sp <- spp_from_cells(combined_pnw_records, pnw_raster)
  pnw_sar_nls <- nls(n_spp ~ a * area^z, data = pnw_sp, start = list(a = 5, z = .2))
  predict(pnw_sar_nls, data.frame(area = lawn_area(map_pnw) / 1e6))

  ## seems fairly reasonable given crudeness of approach
  all_us <- spp_from_cells(combined_records, eez_raster)
  eez_sar_nls <- nls(n_spp ~ a * area^z, data = all_us, start = list(a = 5, z = .2))
  predict(eez_sar_nls, data.frame(area = lawn_area(map_eez) / 1e6), se.fit = T)

  east_us <- spp_from_cells(dplyr::filter(combined_records, is_east_coast), eez_raster)
  east_sar_nls <- nls(n_spp ~ a * area^z, data = east_us, start = list(a = 5, z = .2))
  predict(east_sar_nls, data.frame(area = lawn_area(map_eez) / 2 / 1e6))

  west_us <- spp_from_cells(dplyr::filter(combined_records, is_west_coast), eez_raster)
  west_sar_nls <- nls(n_spp ~ a * area^z, data = west_us, start = list(a = 5, z = .2))
  predict(west_sar_nls, data.frame(area = lawn_area(map_eez) / 2 / 1e6))
}
