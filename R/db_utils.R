## the database is called idigbio but it's also used by OBIS

connect_db <- function(env) {
    sok_db <- DBI::dbConnect(drv = RPostgreSQL::PostgreSQL(), dbname = "idigbio",
                             host = "localhost", user = "marinediversity",
                             password = "password")
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
