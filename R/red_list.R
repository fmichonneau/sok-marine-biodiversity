
fetch_hook_red_list <- function(key, namespace) {
    default_res <- list(name = key, result = NULL)
    if (is.na(key)) return(default_res)
    is_lower_case(key)
    v2("Getting info about ", key, appendLF = FALSE)
    res <- try(rredlist::rl_search(name = key), silent = TRUE)
    if (inherits(res, "try-error")) {
        stop("ERROR")
    }
    if (! (exists("result", res) && exists("name", res))) {
        v2("... missing?")
        return(default_res)
    }
    v2(" ... DONE.")
    res
}


store_red_list <- function(store_path = "data/storr_red_list") {
    storr::storr_external(storr::driver_rds(store_path),
                          fetch_hook_red_list)
}

get_iucn_info <- function(name) {
    iucn_res <- store_red_list()$get(tolower(name))
    if (is.null(iucn_res) || is.null(nrow(iucn_res))) {
        tibble::tibble(
                    worms_valid_name = iucn_res[["name"]][1],
                    iucn_species_name = NA,
                    iucn_marine_system = NA,
                    iucn_category = NA,
                    iunc_criteria = NA
                )
    } else {
        tibble::tibble(
                    worms_valid_name = iucn_res[["name"]][1],
                    iucn_species_name = iucn_res[["result"]][1, "scientific_name"],
                    iucn_marine_system = iucn_res[["result"]][1, "is_marine"],
                    iucn_category = iucn_res[["result"]][1, "category"],
                    iunc_criteria = iunc_res[["results"]][1, "criteria"]
                )

    }

}

add_iucn_status <- function(sp_list) {
    name <- sp_list$worms_valid_name
    res <- purrr::map_df(name, get_iucn_info)
    dplyr::left_join(sp_list, res, by = "worms_valid_name")
}
