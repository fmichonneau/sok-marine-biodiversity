make_grid <- function(map_geojson, cellsize = .5) {
  gom_sp <- geojsonio::geojson_sp(map_geojson)
  gom_bb <- get_bounding_box(gom_sp, cellsize = cellsize)
  bb_to_df(gom_bb)
}


internal_obis_hook <- function(wkt_coords, attempt = 0) {
  res <- try(robis::occurrence(geometry = wkt_coords),
    silent = TRUE
  )
  while (inherits(res, "try-error") && attempt <= 3) {
    v3("attempt ", attempt, " sleeping ...")
    Sys.sleep(exp(runif(1) * attempt))
    res <- internal_obis_hook(wkt_coords, attempt + 1)
  }
  if (!inherits(res, "try-error")) {
    if (nrow(res) > 0) {
      res
    } else {
      NULL
    }
  } else {
    stop(res)
  }
}


fetch_hook_obis_by_geo <- function(key, namespace) {
  coords <- unlist(strsplit(key, "\\|"))
  coords_qry <- list(
    xmin = as.numeric(coords[1]),
    ymax = as.numeric(coords[2]),
    xmax = as.numeric(coords[3]),
    ymin = as.numeric(coords[4])
  )
  coords_poly <- list(
    c(coords_qry$xmin, coords_qry$ymin),
    c(coords_qry$xmin, coords_qry$ymax),
    c(coords_qry$xmax, coords_qry$ymax),
    c(coords_qry$xmax, coords_qry$ymin),
    c(coords_qry$xmin, coords_qry$ymin)
  )
  wkt_coords <- wellknown::polygon(coords_poly, fmt = 3)
  internal_obis_hook(wkt_coords, 0)
}


store_obis_by_geo <- function(coords, store_path = "data/storr_obis_by_geo") {
  storr::storr_external(
    storr::driver_rds(store_path, mangle_key = TRUE),
    fetch_hook_obis_by_geo
  )
}

internal_fill_store_obis_by_geo <- function(k, list_phyla) {
  res <- store_obis_by_geo()$get(k)
  phyla_to_keep <- na.omit(list_phyla$phylum[list_phyla$common_phylum != "to_drop"])
  if (is.null(res)) return(NULL)
  if (nrow(res) > 0) {
    res <- res[tolower(res$phylum) %in% phyla_to_keep &
                 (!tolower(res$class) %in% chordata_classes_to_rm()), ]

    if (file.exists("dt_class.rds")) {
      st_dt_class <- readRDS("dt_class.rds")
    } else {
      st_dt_class <- character(0)
    }
    st_dt_class <- purrr::imap_dfr(res,
      function(x, y) {
        tibble(nm = y, class = class(x))
      }) %>%
      bind_rows(st_dt_class) %>%
      distinct(nm, class)
    saveRDS(st_dt_class, "dt_class.rds")

    res <- sok_as_character(
      res,
      c("rightsHolder", "infraphylum", "country", "scientificNameID",
        "scientificName", "type", "gigaclass", "catalogNumber",
        "occurrenceStatus", "basisOfRecord", "id", "parvphylum", "order",
        "recordNumber", "dataset_id", "locality", "collectionCode",
        "occurrenceID", "license", "genus", "collectionID", "eventDate",
        "coordinateUncertaintyInMeters", "originalScientificName",
        "institutionCode", "class", "kingdom", "recordedBy", "phylum",
        "species", "subphylum", "subclass", "family", "node_id", "flags",
        "subfamily", "fieldNumber", "stateProvince", "scientificNameAuthorship",
        "waterBody", "occurrenceRemarks", "infraclass", "suborder", "category",
        "superclass", "infraorder", "superorder", "individualCount",
        "associatedReferences", "taxonRemarks", "modified", "georeferencedDate",
        "verbatimEventDate", "superfamily", "organismID", "dateIdentified",
        "ownerInstitutionCode", "bibliographicCitation", "taxonRank",
        "vernacularName", "eventTime", "identificationRemarks",
        "nomenclaturalCode", "sex", "footprintWKT", "datasetName",
        "geodeticDatum", "taxonomicStatus", "specificEpithet", "lifeStage",
        "coordinatePrecision", "organismRemarks", "datasetID", "identifiedBy",
        "behavior", "verbatimDepth", "typeStatus", "county", "insitutionCode",
        "dynamicProperties", "accessRights", "continent", "countryCode",
        "habitat", "higherClassification", "higherGeography",
        "identificationQualifier", "institutionID", "island", "islandGroup",
        "language", "references", "georeferencedBy", "eventRemarks",
        "infraspecificEpithet", "georeferenceRemarks", "locationRemarks",
        "associatedMedia", "associatedSequences", "georeferenceSources")
    )
    return(res)
  }
  NULL
}

fill_store_obis_by_geo <- function(map_geojson, list_phyla, cellsize = .5) {
  map_grid <- make_grid(map_geojson, cellsize)

  res <- lapply(map_grid$key, internal_fill_store_obis_by_geo, list_phyla)

  dplyr::bind_rows(res) %>%
    dplyr::mutate_if(is.character, tolower) %>%
    dplyr::mutate(
      cleaned_scientificname = tolower(cleanup_species_names(scientificName)),
      is_binomial = is_binomial(cleaned_scientificname)
    )
}

obis_data_types <- function() {
  tibble::tribble(
    ~name, ~type,
    "uuid", "TEXT", ## is `id` in OBIS
    "decimalLongitude", "REAL",
    "decimalLatitude", "REAL",
    ## "lifestage", "TEXT",
    "basisOfRecord", "TEXT",
    "datecollected", "TIMESTAMP", ## is `eventDate` in OBIS
    "institutionCode", "TEXT",
    ## "collectionCode", "TEXT",
    "catalogNumber", "TEXT",
    ## "locality", "TEXT",
    ## "sex", "TEXT",
    ## "identifiedBy", "TEXT",
    ## "individualCount", "INT",
    ## "datasetName", "TEXT",
    "phylum", "TEXT",
    "order", "TEXT",
    "family", "TEXT",
    "genus", "TEXT",
    "scientificName", "TEXT",
    "originalScientificName", "TEXT",
    "scientificNameAuthorship", "TEXT",
    ## "obisID", "INT",
    "resourceID", "INT",
    "yearcollected", "INT",
    ## "species", "TEXT",
    "qc", "INT",
    "aphiaID", "INT",
    "speciesID", "INT",
    ## "dynamicProperties", "TEXT",
    ## "accessRights", "TEXT",
    ## "collectionID", "TEXT",
    ## "continent", "TEXT",
    ## "countryCode", "TEXT",
    ## "county", "TEXT",
    ## "fieldNumber", "TEXT",
    ## "geodeticDatum", "TEXT",
    ## "habitat", "TEXT",
    ## "higherClassification", "TEXT",
    ## "higherGeography", "TEXT",
    ## "identificationQualifier", "TEXT",
    ## "institutionID", "TEXT",
    ## "island", "TEXT",
    ## "islandGroup", "TEXT",
    ## "language", "TEXT",
    ## "modified", "TEXT",
    ## "occurrenceID", "TEXT",
    ## "occurrenceRemarks", "TEXT",
    ## "occurrenceStatus", "TEXT",
    ## "recordedBy", "TEXT",
    ## "references", "TEXT",
    ## "scientificNameID", "TEXT",
    ## "specificEpithet","TEXT",
    ## "stateProvince", "TEXT",
    ## "taxonRank","TEXT",
    ## "type","TEXT",
    ## "typeStatus", "TEXT",
    ## "vernacularName", "TEXT",
    ## "waterBody", "TEXT",
    "class", "TEXT",
    "depth", "REAL",
    "minimumDepthInMeters", "REAL",
    "maximumDepthInMeters", "REAL",
    "within_eez", "BOOLEAN",
    "within_gom", "BOOLEAN",
    "within_pnw", "BOOLEAN"
  ) %>%
    dplyr::mutate(name = tolower(name))
}


create_obis_db <- function(coords, db_table, gom_phyla) {
  obis_types <- setNames(
    obis_data_types()$type,
    obis_data_types()$name
  )

  ## Before putting all the records in the database we need to make
  ## sure they are cached. I am not sure why it's needed here and
  ## not for iDigBio; but it seems that the data retrieval is slower
  ## for the OBIS API.
  ## EDIT: I seems it was a quirk from trying to
  ## functionalize the handling of the DB connection. Commenting it
  ## out now, but if needs to be recreated from scratch, keeping it
  ## here in case it's useful.
  ##
  ## for (q in names(coords)) {
  ##    invisible(store_obis_by_geo()$get(q))
  ## }

  ## Then we can do what we were doing with iDigBio records
  DBI::dbBegin(sok_db())
  on.exit(DBI::dbRollback(sok_db(), db_table))

  ## if (DBI::dbExistsTable(sok_db(), db_table)) {
  ##   DBI::dbRemoveTable(sok_db(), db_table)
  ## }

  ## DBI::dbCreateTable(
  ##   sok_db(),
  ##   db_table,
  ##   fields = obis_types,
  ##   temporary = FALSE
  ## )

  ## lapply(names(coords), function(q) {
  ##   v2("Getting OBIS records for ", q, appendLF = FALSE)
  ##   r <- store_obis_by_geo()$get(q)
  ##   if (!is.null(r)) {
  ##     r <- r %>%
  ##       dplyr::rename_all(tolower) %>%
  ##       ## harmonize fields across databases
  ##       dplyr::rename(uuid = id) %>% ## rename id --> uuid
  ##       dplyr::rename_if(
  ##         grepl("^eventdate$", names(.)),
  ##         function(x) { ## rename "eventdate" -> "datecollected"
  ##           gsub(".+", "datecollected", x)
  ##         }
  ##       ) %>%
  ##       ## apparently some records are missing some fields, so
  ##       ## we standardize them to their intersect
  ##       dplyr::select(UQ(intersect(
  ##         names(.),
  ##         names(obis_types)
  ##       ))) %>%
  ##       ## to make it comparable to iDigBio, content gets lowercased
  ##       dplyr::mutate_if(is.character, tolower) %>%
  ##       ## make sure all dates are parsed
  ##       dplyr::mutate(
  ##         datecollected = anytime::anydate(datecollected)
  ##       )
  ##     DBI::dbAppendTable(
  ##       sok_db(),
  ##       name = db_table,
  ##       value = r
  ##     )
  ##   }
  ##   v2(" DONE.")
  ## })
  ## v3("complete lapply")
  ## DBI::dbCommit(sok_db())
  ## v3("complete commit")

  ## create indexes on uuid before removing duplicates
  ## DBI::dbExecute(
  ##   sok_db(),
  ##   glue::glue("CREATE INDEX uuid_idx ON {db_table} (uuid)")
  ## )
  ## v3("complete creating indexes")
  ## DBI::dbCommit(sok_db())
  ## v3("complete commit")

  DBI::dbExecute(
    sok_db(),
    glue::glue(
      "CLUSTER {db_table} USING uuid_idx"
    )
  )
  v3("done clustering")
  DBI::dbExecute(
    sok_db(),
    glue::glue("ANALYZE {db_table}")
  )
  v3("Done analyzing")

  dbExecute(sok_db(),
    glue::glue("DELETE FROM {db_table} a USING (
      SELECT MIN(ctid) as ctid, uuid
        FROM {db_table}
        GROUP BY uuid HAVING COUNT(*) > 1
      ) dups
      WHERE a.uuid = dups.uuid
      AND a.ctid <> dups.ctid"))

  v3("complete remove duplicates")
  DBI::dbCommit(sok_db())
  v3("complete commit after removing duplicates")

  DBI::dbBegin(sok_db())
  dbplyr::sql_table_analyze(sok_db(), db_table)
  v3("complete table analysis")
  dbExecute(
    sok_db(),
    glue::glue("ALTER TABLE {db_table} ADD PRIMARY KEY (uuid);")
  )
  v3("complete adding primary key")
  add_within_polygon_to_db(db_table)
  v3("complete within polygon operation")
  DBI::dbCommit(sok_db())
  on.exit(NULL)
}
