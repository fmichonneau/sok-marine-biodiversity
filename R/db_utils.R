connect_sok_db <- function() {
    dplyr::src_postgres("idigbio", host = "localhost",
                        user = "marinediversity",
                        password = "password")
}


sok_db_init <- function(db_table, data_types) {

    data_db <- connect_sok_db()
    con <- data_db$con

    db_begin(con)

    if (db_has_table(con, db_table))
        db_drop_table(con, db_table)

    db_create_table(con, db_table, types = data_types, temporary = FALSE)
    con
}

sok_db_exit <- function(con, db_table, idx) {
    on.exit(db_rollback(con, db_table))
    db_create_indexes(con, db_table, indexes = idx)
    db_analyze(con, db_table)
    db_commit(con)
    on.exit(NULL)
}
