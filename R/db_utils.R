connect_sok_db <- function() {
    ## it's called idigbio but it's also used by OBIS
    dplyr::src_postgres("idigbio", host = "localhost",
                        user = "marinediversity",
                        password = "password")
}
