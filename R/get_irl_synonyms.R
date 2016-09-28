
fetch_hook_worms_synonyms <- function(key, namespace) {
    wid <- get_wormsid(searchterm = key, ask = FALSE)
    if (is.na(wid)) {
        message(key, " not found or multiple matches found.")
        return(NA)
    }
    worms_synonyms(wid)$scientificname
}

store_synonyms <- function(store_path = "data/synonym_storr") {
    invisible(storr_external(driver_rds(store_path),
                             fetch_hook_worms_synonyms))
}

get_store_synonyms <- function(irl_checklist, store = store_synonyms()) {
    species <- irl_checklist$`SCIENTIFIC NAME`
    lapply(species, function(x) store$get(x))
    invisible(store)
}


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
    ## we can also remove the var.
    res$synonyms <- gsub("(\\svar\\. .+)$", "", res$synonyms)

    res <- res[!duplicated(res$synonyms), ]

    con <- dbConnect(SQLite(), database_path)
    dbWriteTable(con, "synonyms", res)
    dbDisconnect(con)
    res

}
