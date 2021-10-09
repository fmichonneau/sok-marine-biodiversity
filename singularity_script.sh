#!/usr/bin/env bash

/etc/init.d/postgresql start
Rscript -e "setwd('/sok'); remake::make()"
