idig_usa_echinodermata <- function() {
    qry <- list(
        phylum = "echinodermata",
        basisofrecord = "preservedspecimen",
        country = list("united states",
                       "USA")
    )
    res <- ridigbio::idig_search_records(
                         rq = qry,
                         fields = idigbio_fields()
                     )
    readr::write_csv(res, "data/echinodermata_idigbio_usa.csv")
    res %>%
        dplyr::rename(decimallatitude = geopoint.lat,
                      decimallongitude = geopoint.lon) %>%
        dplyr::filter(!is.na(decimallongitude), !is.na(decimallatitude)) %>%
        copy_to(sok_db(), ., name = "us_idigbio_echinodermata",
                temporary = FALSE, overwrite = TRUE,
                types = structure(c("TEXT", "TEXT", "TEXT", "TEXT", "TEXT",
                                    "TEXT", "TEXT", "TEXT", "TEXT", "TEXT",
                                    "TEXT", "TEXT", "TEXT", "REAL", "REAL"),
                                  .Names = c("uuid", "catalognumber",
                                             "datecollected", "institutioncode",
                                             "data.dwc:phylum", "data.dwc:class",
                                             "data.dwc:order", "data.dwc:family", "data.dwc:genus",
                                             "data.dwc:fieldNumber", "data.dwc:recordedBy",
                                             "scientificname", "country",
                                             "decimallongitude", "decimallatitude"
                                             )))
    dbExecute(sok_db(), "ALTER TABLE us_idigbio_echinodermata ADD COLUMN within_eez BOOLEAN;")
    dbExecute(sok_db(), "ALTER TABLE us_idigbio_echinodermata ADD COLUMN within_gom BOOLEAN;")
    dbExecute(sok_db(), "ALTER TABLE us_idigbio_echinodermata ADD COLUMN within_pnw BOOLEAN;")
    add_within_polygon_to_db("us_idigbio_echinodermata")

    tbl(sok_db(), "us_idigbio_echinodermata") %>%
        dplyr::select(uuid, within_eez) %>%
        dplyr::collect() %>%
        dplyr::right_join(res, by = "uuid") %>%
        readr::write_csv("data/echinodermata_idigbio_usa_geo.csv")
}
