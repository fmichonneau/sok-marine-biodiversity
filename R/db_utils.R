## the database called idigbio but it's also used by OBIS
if (!exists("sok_db")) {
    sok_db <- DBI::dbConnect(drv = RPostgreSQL::PostgreSQL(), dbname = "idigbio",
                             host = "localhost", user = "marinediversity",
                             password = "password")
}
