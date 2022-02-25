insert_worms_ids <- function(wrm_tbl) {
  db <- sok_db()
  on.exit(dbExecute(db, "DISCARD TEMP;"))

  if (!DBI::dbExistsTable(db, "worms_ids")) {
    dbExecute(db, glue::glue(
      "CREATE TABLE worms_ids (",
      ## IS NOT NULL ensures the anti-join will perform
      ## as expected
      "cleaned_scientificname TEXT NOT NULL,",
      "worms_id INT PRIMARY KEY, ",
      "worms_valid_name TEXT, ",
      "is_fuzzy BOOLEAN, ",
      "rank TEXT, ",
      "is_marine BOOLEAN, ",
      "worms_phylum TEXT, ",
      "worms_class TEXT,  ",
      "worms_order TEXT, ",
      "worms_family TEXT, ",
      "phylum TEXT );"
    ))
    dbExecute(db, "CREATE INDEX ON worms_ids (cleaned_scientificname);")
  }

  .wrm_tbl <- dplyr::distinct(wrm_tbl, cleaned_scientificname)

  ## copy wrm_tbl to a new temporary table
  dbplyr::db_copy_to(db, "tmp_names_to_match", .wrm_tbl,
    temporary = TRUE,
    overwrite = TRUE,
    unique_indexes = list("cleaned_scientificname")
  )

  ## use anti-join to look for names from tmp_names_to_match that are not in worms_ids
  res <- dbExecute(db, glue::glue(
    "CREATE TEMPORARY TABLE tmp_worms_ids ",
    "AS SELECT tmp_nm.cleaned_scientificname ",
    " FROM tmp_names_to_match AS tmp_nm ",
    " LEFT JOIN worms_ids ON ",
    " tmp_nm.cleaned_scientificname = worms_ids.cleaned_scientificname ",
    " WHERE worms_ids.cleaned_scientificname IS NULL;"
  ))

  tbl(db, "tmp_worms_ids") %>%
    collect(Inf) %>%
    add_worms(remove_vertebrates = FALSE) %>%
    dplyr::mutate(worms_id = as.integer(worms_id)) %>%
    db_copy_to(db, "tmp_matched_names", ., temporary = TRUE)

  dbExecute(db, glue::glue(
    "INSERT INTO worms_ids ",
    "SELECT * FROM tmp_matched_names;"
  ))

  dbExecute(db, "ANALYZE worms_ids;")
  dbSendQuery(db, glue::glue(
    "SELECT * FROM tmp_names_to_match ",
    "LEFT JOIN worms_ids ON worms_ids.cleaned_scientificname = tmp_names_to_match.cleaned_scientificname;"
  )) %>%
    dbFetch()
}
