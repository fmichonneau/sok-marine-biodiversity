make_binomial <- function(genus, species) {
    res <- paste(ifelse(is.na(genus), "", genus),
                 ifelse(is.na(species), "", species))
    gsub("\\s+$", "", res)
}

read_idigbio_mammals <- function(file) {
    res <- feather::read_feather(file)
    res %>%
        dplyr::mutate(cleaned_scientificname =
                          make_binomial(`dwc:genus`, `dwc:specificEpithet`)) %>%
        dplyr::group_by(cleaned_scientificname) %>%
        dplyr::filter(nzchar(cleaned_scientificname)) %>%
        dplyr::summarize(
                   n_records = n(),
                   uuids = first_5(coreid)
               ) %>%
        dplyr::mutate(
                   class = "mammalia",
                   is_binomial = is_binomial(cleaned_scientificname),
                   itis_accepted_name = get_accepted_itis_names(cleaned_scientificname)
               )
}

get_accepted_itis_names <- function(nm) {
    vapply(nm, function(x) {
        if (is_binomial(x))
            store_itis_name()$get(tolower(x))
        else
            NA_character_
    }, character(1))
}

read_asm_mammals <- function(file) {

    lst_spp <- readr::read_csv(file) %>%
        rename(common_name = `Common Name`,
               order = Order,
               family = Family,
               raw_species_name = `Species Name`,
               status = Status,
               states = States,
               distribution = Distribution,
               notes = Notes,
               zoonotic = Zoonotic,
               disease = Disease,
               publications = Publications) %>%
        mutate(species_name = tolower(raw_species_name)) %>%
        ## fixing typos
        mutate(species_name = replace(species_name, species_name == "histrophoca fasciata", "histriophoca fasciata")) %>%
        mutate(species_name = replace(species_name, species_name == "tamias quadirivittatus", "tamias quadrivittatus")) %>%
        mutate(species_name = replace(species_name, species_name == "blarina penninsulae", "blarina peninsulae")) %>%
        mutate(species_name = replace(species_name, species_name == "galucomys sabrinus", "glaucomys sabrinus")) %>%
        ## remove extinct species
        filter(species_name != "pteronotus pristinus")

    ## other species with no matches:
    ## chaetodipus rudinoris: only records are in Mexico
    ## phenacomys ungava: only records are in Canada
}


asm_mammals_idigbio <- function(mam) {
    internal_fetch_idigbio(mam$species_name, 10) %>%
        rename(species_name = scientificname)
}

find_bold_mammals <- function(mam_asm) {
    mam_spp <- mam_asm %>%
        dplyr::select(species_name) %>%
        unique
    res <- find_bold_records(mam_spp, "species_name")
    res
}

asm_mammals_idigbio_summary <- function(mam_asm, mam_idig) {
    summarize_raw_idigbio(mam_idig, mam_asm)
}

make_idig_bold_summary <- function(idig, bold) {
    dplyr::left_join(idig, bold, by = "species_name")
}
