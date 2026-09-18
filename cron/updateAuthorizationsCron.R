library(RPostgres)
library(DBI)
library(dplyr)
library(dbplyr)
library(stringr)

lhdApiPassword <- Sys.getenv("LHD_API_PASSWORD")
con <- dbConnect(
  Postgres(),
  host = Sys.getenv("POSTGRESQL_HOST", "127.0.0.1"),
  dbname = Sys.getenv("POSTGRESQL_DBNAME", "lhd"),
  user = Sys.getenv("POSTGRESQL_USER", "root"),
  password = Sys.getenv("POSTGRESQL_PASSWORD", "ROOT"),
  port = Sys.getenv("POSTGRESQL_PORT", 45432)
)

now <- Sys.time()

authorizations <- tbl(con, "authorization") %>%
  filter(expiration_date < now, status == 'Active') %>%
  collect()

for (i in seq_len(nrow(authorizations))) {
  r <- authorizations[i, ]
  query <- paste0("UPDATE \"authorization\" SET \"status\" = 'Expired' WHERE \"id_authorization\" = ",r$id_authorization)
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
