FROM rocker/geospatial:4.0.4

RUN apt-get update && apt-get install -y \
    postgresql-12 \
    postgresql-12-postgis-3 \
    && rm -rf /var/lib/apt/lists/*

USER root

COPY ./remake.yml .


## we freeze the CRAN package version to 2021-10-08
RUN R -e "options(repos = c(RSPM = 'https://packagemanager.rstudio.com/all/2021-10-08+Y3JhbjoyMDIxLTEwLTA3LDI6NDUyNjIxNTtCNjY2MDZGOQ')); remotes::install_github('richfitz/remake@e29028b548950a3132ea2d045b7f67344ce22a6b'); remotes::install_github('ropensci/lawn@10c7c526cd29a2522c7105702a69404e389d5018'); remake::install_missing_packages()"

## IPv6 seems to be causing some issues

RUN echo "127.0.0.1       localhost" > /etc/hosts


## start postgres and create database
USER postgres
RUN  /etc/init.d/postgresql start \
    && psql --command "CREATE USER marinediversity WITH SUPERUSER PASSWORD 'password';" \
    && createdb -O marinediversity sok \
    && psql -d sok -c "CREATE EXTENSION postgis;"
VOLUME  ["/etc/postgresql", "/var/log/postgresql", "/var/lib/postgresql"]
