## Storr for the WoRMS ids: given a taxon name, what is its WoRMS id? ----------
fetch_hook_worms_ids <- function(key, namespace) {
    wid <- get_wormsid(searchterm = key, ask = FALSE)
    if (identical(attr(wid, "match"), "found")) {
        return(wid)
    } else if (identical(attr(wid, "match"), "not found") ||
               identical(attr(wid, "match"), "multi match")) {
        wid <- get_wormsid(searchterm = key, ask = FALSE, accepted = FALSE)
        if (identical(attr(wid, "match"), "not found")) {
            message(key, " not found.")
            return(NA)
        }
        res <- worms_records(ids = wid)$valid_AphiaID
        if (length(res) != 1) stop("More than one record for ", key)
        res
    } else stop("I don't know what to do ", attr(wid, "match"))
}

store_worms_ids <- function(store_path = "data/worms_ids_storr") {
    invisible(storr_external(driver_rds(store_path),
                             fetch_hook_worms_ids))
}


## Storr for the WoRMS synonyms: given a WoRMS id, what are the synonyms? ------
fetch_hook_worms_synonyms <- function(key, namespace) {
    worms_synonyms(key)$scientificname
}

store_synonyms <- function(store_path = "data/synonym_storr") {
    invisible(storr_external(driver_rds(store_path),
                             fetch_hook_worms_synonyms))
}

## Fill the two storrs to get all the synonyms from the IRL species list
get_synonym_store <- function(irl_checklist) {
    species <- irl_checklist$`SCIENTIFIC NAME`
    lapply(species, function(x) {
        wid <- store_worms_ids()$get(x)
        store_synonyms()$get(wid)
    })
}

## Storr for the higher classification: given a WoRMS id, what is the
## higher classification
fetch_hook_worms_classification <- function(key, namespace) {
    taxizesoap:::classification_s.wormsid(key)
}

store_worms_classification <- function(store_path = "data/worms_classification_storr") {
    invisible(storr_external(driver_rds(store_path),
                             fetch_hook_worms_classification))
}

get_classification_store <- function(irl_checklist) {
    species <- irl_checklist$`SCIENTIFIC NAME`
    lapply(species, function(x) {
        wid <- store_worms_ids()$get(x)
        store_worms_classification()$get(wid)
    })
}


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
