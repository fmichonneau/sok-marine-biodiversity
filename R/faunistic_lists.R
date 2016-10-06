fetch_hook_idigbio_records <- function(key, namespace) {
    key_parts <- unlist(strsplit(key, "-"))
    taxon_level <- key_parts[1]
    taxon_name <- key_parts[2]
    res <- get_idigbio_records(taxon_name, taxon_level)
    res
}

store_idigbio_records <- function(store_path = "data/idigbio_records") {
    storr_external(driver_rds(store_path),
                   fetch_hook_idigbio_records)
}

assemble_idigbio_records <- function(taxon_name, taxon_level) {
    invisible(store_idigbio_records()$get(paste(taxon_level, taxon_name, sep = "-")))
}

get_idigbio_records <- function(taxon_name, taxon_level) {
    message("Looking for ", taxon_name, " ... ", appendLF = FALSE)
    taxon_level <- match.arg(taxon_level, c("phylum", "class", "order", "family", "genus"))
    qry <- list(basisofrecord = "PreservedSpecimen",
                scientificname = list(type = "exists"),
                geopoint = list(type = "geo_bounding_box",
                                top_left = list(lat = 51, lon = -133),
                                bottom_right = list(lat = 23, lon = -51))
                )
    taxon_level <- paste0("data.dwc:", taxon_level)
    qry <- c(setNames(taxon_name, taxon_level), qry)
    res <- idig_search_records(rq = qry, limit = 100000,
                               fields = c(
                                   'uuid',
                                   'catalognumber',
                                   'institutioncode',
                                   'data.dwc:phylum',
                                   'data.dwc:class',
                                   'data.dwc:order',
                                   'data.dwc:family',
                                   'data.dwc:genus',
                                   'scientificname',
                                   'country',
                                   'geopoint'))
    message("found ", nrow(res), " records.")
    res
}

assemble_idigbio_species_list <- function(idig_store = store_idigbio_records()) {
    groups <- idig_store$list()
    res <- lapply(groups, function(x) species_list_from_idigbio(idig_store$get(x)))
    names(res) <- groups
    res
}

species_list_from_idigbio <- function(idig) {
    res <- na.omit(unique(idig$scientificname))
    ## remove sp., spp., / and numbered species
    res <- res[!grepl("spp?\\.|[0-9]+|\\/", res)]
    ## remove species with a single letter word
    res <- res[vapply(res, function(x) !any(nchar(unlist(strsplit(x, " "))) < 2), logical(1))]
    ## remove cf., aff. and ? from the species names
    res <- gsub("cf\\.|aff\\.|\\?", "", res)
    ## remove subgenus
    res <- gsub("\\([^)]*\\)", "", res)
    ## remove everything that comes after varieties
    res <- gsub("var\\. .+$", "", res)
    ## remove extra spaces at the end or in the middle
    res <- gsub("\\s+$", "", res)
    res <- gsub("\\s{2,}", " ", res)
    ## only keep the names with a space in it (more likely it's binomial)
    res <- res[grepl(" ", res)]
    ## deduplicate again
    res <- unique(res)
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
        message("No record for ", key, ". Trying to look for synonyms ...", appendLF = FALSE)
        wid <- store_worms_ids()$get(key)
        if (is.na(wid)) {
            message("Can't find a valid WoRMS ID")
            return("not in worms")
        } else {
            syn <- store_synonyms()$get(wid)
            res <- try(bold_specimens(taxon = paste0("'", syn, "'", collapse = "|")),
                       silent = TRUE)
            if (inherits(res, "try-error")) {
                message("No record for any of the synonyms ",
                        paste(syn, collapse = ", "))
                return(NULL)
            }
            message(" found ", nrow(res), " records for synonyms")
        }
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
            ## TODO -- also check that BOLD taxonomy also indicates
            ## that it's the correct phylum. Some iDigBio records seem
            ## to have the wrong phylum listed for some taxa.
            r <- st$get(s, namespace = p)
            if (is.null(r))
                n <- 0
            else if (identical(r, "not in worms"))
                n <- NA
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
