create_bold_table_statement <- '
CREATE TABLE bold_table
( "processid" TEXT,
	"sampleid" TEXT,
	"museumid" TEXT,
	"fieldid" TEXT,
	"bin_guid" TEXT,
	"bin_name" TEXT,
	"vouchertype" TEXT,
	"inst_reg" TEXT,
	"phylum_reg" TEXT,
	"class_reg" TEXT,
	"order_reg" TEXT,
	"family_reg" TEXT,
	"subfamily_reg" TEXT,
	"genus_reg" TEXT,
	"species_reg" TEXT,
	"taxonomist_reg" TEXT,
	"collectors" TEXT,
	"collectiondate" TEXT,
	"lifestage" TEXT,
	"lat" REAL,
	"lon" REAL,
	"site" TEXT,
	"sector" TEXT,
	"region" TEXT,
	"province_reg" TEXT,
	"country_reg" TEXT,
	"fundingsrc" TEXT,
	"seqentryid" INTEGER,
	"seqdataid" INTEGER,
	"marker_code" TEXT,
	"nucraw" TEXT,
	"aminoraw" TEXT,
	"seq_update" REAL,
	"total_trace_count" INTEGER,
	"high_trace_count" INTEGER,
	"accession" TEXT
);
'

create_bold_db <- function(path, db_file = "data/bold_database.sqlite") {
    bold_files <- list.files(path = path, pattern = "\\.tsv\\.zip$",
                             full.names = TRUE)

    ## create database from scratch (delete if it already exists)
    ##  let's trust remake to do the right thing...
    ##if (file.exists(db_file))
    ##    file.remove(db_file)

    con <- dbConnect(RSQLite::SQLite(), db_file)
    dbSendQuery(con, create_bold_table_statement)
    dbDisconnect(con) ## disconnecting now, and inside to avoid running out of memory

    ## create tables
    for (i in seq_along(bold_files)) {
        bold_data <- readr::read_tsv(bold_files[i])

        ## this field is causing issue as most files have
        ## `total_trace_count` and `high_trace_count`, but a couple of
        ## files have `trace_count` only. Dropping it so all tables
        ## have same names
        if ("trace_count" %in% names(bold_data))
            bold_data <- bold_data[, -match("trace_count", names(bold_data))]

        con <- dbConnect(SQLite(), db_file)
        missing_fields <- setdiff(dbListFields(con, "bold_table"),
                                  names(bold_data))
        if (length(missing_fields)) {
            bold_data <- cbind.data.frame(bold_data,
                                       setNames(
                                           data.frame(matrix(ncol = length(missing_fields),
                                                             nrow = nrow(bold_data))),
                                           missing_fields))
        }
        res <- dbWriteTable(con, name = "bold_table",
                            value = bold_data, append = TRUE)
        dbDisconnect(con)
    }
}

create_joined_data <- function(irl_checklist, bold_db_file) {
    con <- dbConnect(RSQLite::SQLite(), bold_db_file)
    dbWriteTable(con, "irl_checklist", irl_checklist)
    dbDisconnect(con)
}


join_species <- function(bold_db) {
    db <- src_sqlite(bold_db)
    joined <- tbl(db, sql("SELECT bold_table.*, irl_checklist.* FROM bold_table JOIN irl_checklist ON bold_table.species_reg=irl_checklist.`SCIENTIFIC NAME`"))
    collect(joined)

}

join_synonyms <- function(bold_db) {
    db <- src_sqlite(bold_db)
    joined <- tbl(db, sql("SELECT * FROM bold_table JOIN synonyms ON bold_table.species_reg=synonyms.synonyms"))
    collect(joined)
}

summary_field_table <- function(db = "data/bold_database.sqlite") {
    con <- dbConnect(RSQLite::SQLite(), db)
    list_tables <- dbListTables(con)
    res <- lapply(list_tables, function(x)
        dbListFields(con, x))
    uniq_res <- Reduce(unique, res)
    vapply(res, function(x) all(uniq_res %in% x), logical(1))
}


select_records <- function(db = "data/bold_database.sqlite")  {
    my_db <- src_sqlite(db)

    ## only the marine phyla from the US and around it
    select_phyla <- tbl(my_db, sql(paste0("SELECT * FROM bold_table WHERE phylum_reg IN (",
                                          paste0("'", phylum_list(), "'", collapse = ", "),
                                          ")",
                                          " AND class_reg NOT IN ('Insecta', 'Arachnida', 'Collembola', 'Clitellata', 'Diplopoda''Chilopoda')",
                                          " AND (country_reg IS 'United States' OR",
                                          " (lon > -95 AND lon < -56) AND (lat > 5 AND lat < 45))")
                                   )
                        )

    collect(select_phyla)

}

bold_class_plot <- function(filtered_db) {
    filtered_db %>%
        group_by(class_reg) %>%
        tally() %>%
        ggplot() +
        geom_bar(aes(x = reorder(class_reg, n), y = n), stat = "identity") +
        coord_flip()
}
