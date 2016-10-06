## Add a table in the SQLite database to include all the synonyms for
## the IRL species
make_synonym_table <- function(store = store_synonyms(),
                               database_path = "data/bold_database.sqlite") {
    sp_nm <- store$list()
    sp_syn <- lapply(sp_nm, function(x) store$get(x))
    sp_len <- vapply(sp_syn, length, integer(1))
    sp_nm_rep <- rep(sp_nm, sp_len)
    res <- data.frame(scientificname = sp_nm_rep,
                      synonyms = unlist(sp_syn))
    res <- res[complete.cases(res), ]
    ## for our purpose we don't want the subgenera
    res$synonyms <- gsub("\\(.*\\)", "", res$synonyms)
    ## we can also remove the varieties/subspecies
    res$synonyms <- gsub("(\\svar\\. .+)$", "", res$synonyms)

    res <- res[!duplicated(res$synonyms), ]

    con <- dbConnect(SQLite(), database_path)
    dbWriteTable(con, "synonyms", res)
    dbDisconnect(con)
    res

}
