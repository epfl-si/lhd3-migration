library(RMariaDB)
library(DBI)
library(dplyr)
library(dbplyr)
library(stringr)

# Connect to my-db as defined in /etc/mysql/my.cnf
# con <- dbConnect(RMariaDB::MariaDB(), default.file = '/etc/mysql/my.cnf', group = "lhd")

dbNameVar <- Sys.getenv("MYSQL_LHD_V2_DBNAME")
dbHostVar <- Sys.getenv("MYSQL_LHD_V2_HOST")
dbPasswordVar <- Sys.getenv("MYSQL_LHD_V2_PASSWORD")
dbPortVar <- Sys.getenv("MYSQL_LHD_V2_PORT")
dbUserVar <- Sys.getenv("MYSQL_LHD_V2_USER")
con <- dbConnect(RMariaDB::MariaDB(), username = dbUserVar, password = dbPasswordVar, host = dbHostVar, port = dbPortVar, dbname = dbNameVar)

print(con)

now <- Sys.time()

authorizations <- tbl(con, "authorization") %>%
  filter(expiration_date < now, status == 'Active') %>%
  collect()

for (i in seq_len(nrow(authorizations))) {
  r <- authorizations[i, ]
  print(r)
  query <- paste0("UPDATE authorization SET status = 'Expired' WHERE id_authorization = ",r$id_authorization)
  dbExecute(con, query)
  
  newLog <- data.frame(
    modified_by = 'Cron',
    modified_on = now,
    table_name = 'authorization',
    column_name = 'status',
    old_value = r$status,
    new_value = 'Expired',
    action = 'UPDATE',
    table_id = r$id_authorization
    
  )
  dbAppendTable(con, 'mutation_logs', newLog)
}

# Disconnect from the database
dbDisconnect(con)