assemble_idigbio_records <- function(phyla = c("Echinodermata", "Nemertea", "Phoronida",
                                               "Ctenophora", "Chaetognatha")) {
    res <- lapply(phyla, get_idigbio_records)
    names(res) <- phyla
    res
}

get_idigbio_records <- function(phylum) {
    message("Looking for ", phylum)
    res <- vector("list", 100)
    qry <- list(`data.dwc:phylum` = phylum,
                basisofrecord = "PreservedSpecimen",
                scientificname = list(type = "exists"),
                geopoint = list(type = "geo_bounding_box",
                                top_left = list(lat = 51, lon = -133),
                                bottom_right = list(lat = 23, lon = -51))
                )
    res_1 <- idig_search_records(rq = qry, limit = 10000, fields = c('uuid',
                     'catalognumber',
                     'data.dwc:phylum',
                     'family',
                     'genus',
                     'scientificname',
                     'country',
                     'geopoint'))
    res[[1]] <- res_1
    ## paging doesn't work in iDigBio for now
    ## page <- 1
    ## while (nrow(res_1) == 4999) {
    ##     message("   page ", page)
    ##     res_1 <- idig_search_records(rq = qry, limit = 4999, offset = page * 4999 + 1)
    ##     page <- page + 1
    ##     res[[page]] <- res_1
    ## }
    res <- res[vapply(res, function(x) !is.null(x), logical(1))]
    bind_rows(res)
}

assemble_idigbio_species_list <- function(idig_records) {
    res <- lapply(idig_records, species_list_from_idigbio)
    names(res) <- names(idig_records)
    res
}

species_list_from_idigbio <- function(idig) {
    res <- na.omit(unique(idig$scientificname))
    ## remove sp. and numbered species
    res <- res[!grepl("sp\\.|[0-9]+", res)]
    ## remove species with a single letter word
    res <- res[vapply(res, function(x) !any(nchar(unlist(strsplit(x, " "))) < 2), logical(1))]
    ## remove cf. and aff. from the species names
    res <- gsub("cf\\.|aff\\.", "", res)
    ## remove subgenus
    res <- gsub("\\([^)]*\\)", "", res)
    ## remove everything that comes after varieties
    res <- gsub("var\\. .+$", "", res)
    ## remove extra spaces at the end or in the middle
    res <- gsub("\\s+$", "", res)
    res <- gsub("\\s{2,}", " ", res)
    ## only keep the names with a space in it (more likely it's binomial)
    res <- res[grepl(" ", res)]
    is_binom <- is_binomial(res)
    if (!all(is_binom)) {
        message("Not binomial: ",
                paste(res[!is_binom], collapse = ", "))
    }
    res
}

fetch_hook_bold_specimens_per_species <- function(key, namespace) {
    res <- try(bold_specimens(taxon = paste0("'", key, "'")), silent = TRUE)
    if (inherits(res, "try-error")) {
        message("No record for ", key)
        return(NULL)
    }
    message(nrow(res), " record(s) for ", key)
    res
}

store_bold_specimens_per_species <- function(store_path = "data/bold_specimens_per_species") {
    storr_external(driver_rds(store_path),
                   fetch_hook_bold_specimens_per_species)
}

fill_bold_specimens_per_species <- function(sp_list, phylum, store = store_bold_specimens_per_species()) {
    res <- lapply(sp_list, function(x) store$get(x, namespace = phylum))
    invisible(res)
}

assemble_bold_specimens_per_species <- function(sp_lists) {
    lapply(names(sp_lists), function(x) {
        fill_bold_specimens_per_species(sp_lists[[x]], x)
    })
}

make_table_records_per_species <- function(store = store_bold_specimens_per_species()) {
    nmspc <- setdiff(store$list_namespaces(), "objects")
    phyla <- nmspc
    res <- lapply(phyla, function(p) {
        sp <- st$list(namespace = p)
        t <- lapply(sp, function(s) {
            r <- st$get(s, namespace = p)
            if (is.null(r))
                n <- 0
            else
                n <- NROW(r)
            data.frame(phylum = p,
                       species = s,
                       n_records = n,
                       stringsAsFactors = FALSE)
        })
        bind_rows(t)
    })
    bind_rows(res)
}
