## data storr for the iDigBio specimens by UUID. We need to get
## individual records from the API, as if we tried to pool them first,
## the attribution information is aggregated across the query and we
## can't get the correspondance between the UUIDs for the specimen and
## the collection that holds this record.
fetch_hook_idigbio_by_uuid <- function(key, namespace) {
  ridigbio::idig_search_records(rq = list(uuid = key), fields = idigbio_fields())
}

store_idigbio_uuid <- function(coords, store_path = "data/storr_idigbio_uuid") {
  storr::storr_external(
    storr::driver_rds(store_path),
    fetch_hook_idigbio_by_uuid
  )
}


## From a data frame of iDigBio records, creates a data frame that
## contains the attribution information. We recalculate the item
## counts, as there is not a good way to get this info from the raw
## data downloaded during the geographic searches.
idigbio_attribution <- function(idig) {
  stopifnot(exists("uuid", idig))
  store_idigbio_uuid()$mget(idig$uuid) %>%
    map_df(function(uu) {
      attribution <- attr(uu, "attribution")
      if (length(attribution) == 0L) {
        return(NULL)
      }
      if (length(attribution) == 1L) {
        attribution[[1]] %>%
          purrr::map_if(is.null, ~NA_character_) %>%
          as_tibble() %>%
          ## remove nested info, not needed for our purposes
          dplyr::select_if(is.atomic) %>%
          dplyr::distinct()
      }
    }) %>%
    dplyr::rename(recordset_uuid = uuid) %>%
    dplyr::group_by(recordset_uuid) %>%
    dplyr::mutate(itemCount = n()) %>%
    dplyr::distinct()
}
