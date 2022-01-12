FROM rocker/geospatial:4.0.5

RUN apt-get update && apt-get install -y \
    postgresql-12 \
    postgresql-12-postgis-3 \
    ttf-ubuntu-font-family \
    ## texlive-extra-utils \
    && rm -rf /var/lib/apt/lists/*

USER root

COPY ./remake.yml .

# Correct the Error: could not open temporary statistics file "/var/run/postgresql/12-main.pg_stat_tmp/global.tmp": No such file or directory
RUN mkdir -p /var/run/postgresql/12-main.pg_stat_tmp
RUN chown postgres.postgres /var/run/postgresql/12-main.pg_stat_tmp -R

# download cpdf
RUN wget https://github.com/coherentgraphics/cpdf-binaries/blob/d4e7ae79f65ef3f085d09e3c876eb9a8ed1ced34/Linux-Intel-64bit/cpdf \
    && chmod +x cpdf \
    && mv cpdf /usr/local/bin/cpdf

## we freeze the CRAN package version to 2021-10-08
RUN R -e "options(repos = c(RSPM = 'https://packagemanager.rstudio.com/all/__linux__/focal/2021-10-08+Y3JhbiwyOjQ1MjYyMTU7MzM5MTJFREY')); remotes::install_github('richfitz/remake@e29028b548950a3132ea2d045b7f67344ce22a6b'); remotes::install_github('ropensci/lawn@10c7c526cd29a2522c7105702a69404e389d5018'); remake::install_missing_packages()"
