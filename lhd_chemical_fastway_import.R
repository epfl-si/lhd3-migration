library(DBI)
library(RMariaDB)
library(readxl)
library(dplyr)

# Connect to my-db as defined in /etc/mysql/my.cnf
# con <- dbConnect(RMariaDB::MariaDB(), default.file = '/etc/mysql/my.cnf', group = "lhd")

dbNameVar <- Sys.getenv("MYSQL_LHD_V2_DBNAME")
dbHostVar <- Sys.getenv("MYSQL_LHD_V2_HOST")
dbPasswordVar <- Sys.getenv("MYSQL_LHD_V2_PASSWORD")
dbPortVar <- Sys.getenv("MYSQL_LHD_V2_PORT")
dbUserVar <- Sys.getenv("MYSQL_LHD_V2_USER")
con <- dbConnect(RMariaDB::MariaDB(), username = dbUserVar, password = dbPasswordVar, host = dbHostVar, port = dbPortVar, dbname = dbNameVar)

print(con)

# file_path <- "/home/rmaggi/dev/R/lhd3-migration/chemical_substance.xlsx"
file_path <- "/scripts/chemical_substance.xlsx"

chem <- read_excel(file_path)

data <- chem %>%
  select('CAS' , 'Code', 'flow') %>%
  mutate(
    fastway = ifelse(flow == 'Fast', 1, 0)
  ) %>%
  collect()

for (i in seq_len(nrow(data))) {
  dbExecute(con, "
    UPDATE auth_chem
    SET fastway = ?, auth_code = ?
    WHERE cas_auth_chem = ?",
              params = list(data$fastway[i], data$Code[i], data$CAS[i])
  )
}

dbDisconnect(con)
