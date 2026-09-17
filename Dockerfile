FROM rocker/r-ver:4.5.1

WORKDIR /scripts

RUN apt-get update && apt-get install -y \
    libpq-dev \
    libssl-dev \
    libxml2-dev \
    libcurl4-openssl-dev \
    libcurl4t64 \
    libicu-dev

RUN R -e "install.packages(c('RPostgres', 'DBI', 'dplyr', 'dbplyr', 'httr', 'jsonlite', 'stringr'), repos='https://packagemanager.posit.co/cran/__linux__/jammy/latest')"

COPY ./cron ./cron

CMD ["bash"]
