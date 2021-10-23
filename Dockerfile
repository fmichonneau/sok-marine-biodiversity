FROM rocker/geospatial:4.0.5

RUN apt-get update && apt-get install -y \
    postgresql-12 \
    postgresql-12-postgis-3 \
    && rm -rf /var/lib/apt/lists/*

USER root

COPY ./remake.yml .

## we freeze the CRAN package version to 2021-10-08
RUN R -e "options(repos = c(RSPM = 'https://packagemanager.rstudio.com/all/__linux__/focal/2021-10-08+Y3JhbiwyOjQ1MjYyMTU7MzM5MTJFREY')); remotes::install_github('richfitz/remake@e29028b548950a3132ea2d045b7f67344ce22a6b'); remotes::install_github('ropensci/lawn@10c7c526cd29a2522c7105702a69404e389d5018'); remake::install_missing_packages()"


# ## start postgres and create database
# USER postgres

# VOLUME  ["/etc/postgresql", "/var/log/postgresql", "/var/lib/postgresql"]

# RUN  pg_createcluster 12 main \
#     && psql --command "CREATE USER marinediversity WITH SUPERUSER PASSWORD 'password';" \
#     && createdb -O marinediversity sok \
#     && psql -d sok -c "CREATE EXTENSION postgis;" \
#     && /etc/init.d/postgresql start

# RUN echo "host all  all    0.0.0.0/0  md5" >> /etc/postgresql/12/main/pg_hba.conf
# RUN echo "listen_addresses = '*'" >> /etc/postgresql/12/main/postgresql.conf
