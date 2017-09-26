## it's called idigbio but it's also used by OBIS
sok_db <- pool::dbPool(drv = RPostgreSQL::PostgreSQL(), dbname = "idigbio",
                       host = "localhost", user = "marinediversity",
                       password = "password")
