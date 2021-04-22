FROM rocker/geospatial:4.0.4

RUN apt-get update && apt-get install -y \
    postgresql-12 \
    postgresql-12-postgis-3 \
    && rm -rf /var/lib/apt/lists/*

## start postgres and create database
USER postgres
RUN  /etc/init.d/postgresql start \
    && psql --command "CREATE USER marinediversity WITH SUPERUSER PASSWORD 'password';" \
    && createdb -O marinediversity sok \
    && psql -d sok -c "CREATE EXTENSION postgis;"

USER root

COPY ./remake.yml .

VOLUME  ["/etc/postgresql", "/var/log/postgresql", "/var/lib/postgresql"]

RUN R -e "remotes::install_github('richfitz/remake@e29028b548950a3132ea2d045b7f67344ce22a6b'); remotes::install_github('ropensci/lawn'); remake::install_missing_packages()"

RUN /etc/init.d/postgresql start
