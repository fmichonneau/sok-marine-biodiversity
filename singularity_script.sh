#!/usr/bin/env bash

pg_createcluster 12 main
pg_ctlcluster 12 main start
psql -d sok -c "CREATE EXTENSION postgis;"

Rscript -e "setwd('/sok'); remake::make()"
