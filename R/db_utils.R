## the database called idigbio but it's also used by OBIS
if (!exists("sok_db")) {
    sok_db <- pool::dbPool(drv = RPostgreSQL::PostgreSQL(), dbname = "idigbio",
                           host = "localhost", user = "marinediversity",
                           password = "password", idleTimeout = "Inf")
}
