#!/usr/bin/env bash

# pg_createcluster 12 main
# pg_ctlcluster 12 main start

# createdb michonneauf

# psql --command "CREATE USER marinediversity WITH SUPERUSER PASSWORD 'password';"
# createdb -O marinediversity sok

# psql -d sok -c "CREATE EXTENSION postgis;"


# pg_ctlcluster 12 main status

pg_ctlcluster 12 main status

Rscript -e "con = DBI::dbConnect(RPostgres::Postgres(), dbname='sok', user='michonneauf', port=5435); con"

Rscript -e "setwd('/sok'); remake::make()"
