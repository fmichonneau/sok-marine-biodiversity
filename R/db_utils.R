## the database is called sok
connect_db <- function(env) {
  sok_db <- DBI::dbConnect(
    drv = RPostgres::Postgres(),
    dbname = "sok",
    user = "michonneauf",
    port = 5435
  )
  assign("sok_db_con", sok_db, envir = env)
}

sok_db <- function() {
  ## create environment if it doesn't exist
  if (!exists("sok_db_env", envir = globalenv())) {
    assign("sok_db_env", new.env(), envir = globalenv())
  }

  ## create connection in the environment if it doesn't exist
  if (!exists("sok_db_con", envir = sok_db_env, where = globalenv())) {
    connect_db(sok_db_env)
  }

  ## check the connection is valid, if not reconnect
  res <- get("sok_db_con", envir = sok_db_env)
  con_status <- try(DBI::dbGetInfo(res), silent = TRUE)
  if (inherits(con_status, "try-error")) {
    connect_db(sok_db_env)
  }
  get("sok_db_con", envir = sok_db_env)
}

## db_create_indexes dropped from dbplyr, quik rewrite
sok_db_create_index <- function(con, table, index) {
  stopifnot(rlang::is_character(index))
  qry <- glue::glue(
    "CREATE INDEX ON {table} ({ cols });",
    cols = glue::glue_collapse(index, sep = ", ")
  )
  message("running: ", qry)
  dbExecute(con, qry)
}

sok_db_create_indexes <- function(con, table, indexes) {
  purrr::walk(indexes, function(idx) {
    sok_db_create_index(con, table, idx)
  })
}
