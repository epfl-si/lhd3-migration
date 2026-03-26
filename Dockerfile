FROM docker.io/r-base

WORKDIR /scripts

RUN apt-get update && apt-get install -y \
    libmariadb-dev-compat \
    libssl-dev \
    libxml2-dev \
    libcurl4-openssl-dev

RUN R -e "install.packages(c('RMariaDB', 'DBI', 'dplyr', 'dbplyr', 'httr', 'readxl'), repos='https://cloud.r-project.org/')"

COPY . ./

CMD ["bash"]
