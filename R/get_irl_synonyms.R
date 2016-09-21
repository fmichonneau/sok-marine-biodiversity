
fetch_hook_worms_synonyms <- function(key, namespace) {
    wid <- get_wormsid(searchterm = key, ask = FALSE)
    if (is.na(wid)) {
        message(key, " not found or multiple matches found.")
        return(NA)
    }
    worms_synonyms(wid)$scientificname
}

store_synonyms <- function(irl_checklist, store_path = "data/synonym_storr") {
    species <- irl_checklist$`SCIENTIFIC NAME`
    st <- storr_external(driver_rds(store_path),
                         fetch_hook_worms_synonyms)
    lapply(species, function(x) st$get(x))
    invisible(st)
}


make_synonym_table <- function(store_path = "data/synonym_storr",
                               database_path = "data/bold_database.sqlite") {
    st <- storr_rds(store_path)
    sp_nm <- st$list()
    sp_syn <- lapply(sp_nm, function(x) st$get(x))
    sp_len <- vapply(sp_syn, length, integer(1))
    sp_nm_rep <- rep(sp_nm, sp_len)
    res <- data.frame(scientificname = sp_nm_rep,
                      synonyms = unlist(sp_syn))
    res <- res[complete.cases(res), ]
    ## for our purpose we don't want the subgenera
    res$synonyms <- gsub("\\(.*\\)", "", res$synonyms)
    ## we can also remove the var.
    res$synonyms <- gsub("(\\svar\\. .+)$", "", res$synonyms)

    res <- res[!duplicated(res$synonyms), ]

    con <- dbConnect(SQLite(), database_path)
    dbWriteTable(con, "synonyms", res)
    dbDisconnect(con)
    res

}
