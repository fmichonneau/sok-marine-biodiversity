#!/usr/bin/env bash

make-ssl-cert generate-default-snakeoil --force-overwrite

pg_createcluster 12 main
pg_ctlcluster 12 main start
psql -d sok -c "CREATE EXTENSION postgis;"

pg_ctlcluster 12 main restart

pg_ctlcluster status

Rscript -e "con = DBI::dbConnect(RPostgres::Postgres(), dbname='sok', user='rstudio'); con"

Rscript -e "setwd('/sok'); remake::make()"
