connect_sok_db <- function() {
    ## it's called idigbio but it's also used by OBIS
    DBI::dbConnect(RPostgreSQL::PostgreSQL(),
                   dbname = "idigbio", host = "localhost",
                   user = "marinediversity",
                   password = "password")
}
