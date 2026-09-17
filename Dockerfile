FROM docker.io/r-base:4.6.0

WORKDIR /scripts

RUN apt-get update && apt-get install -y \
    libpq-dev \
    libssl-dev \
    libxml2-dev \
    libcurl4-openssl-dev \
    libicu-dev

RUN R -e "install.packages(c('RPostgres', 'DBI', 'dplyr', 'dbplyr', 'httr', 'jsonlite', 'stringr'), repos='https://cloud.r-project.org/')"

COPY ./cron ./cron

CMD ["bash"]
