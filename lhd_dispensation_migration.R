library(RMariaDB)
library(DBI)
library(dplyr)
library(dbplyr)
library(stringr)
library(httr)
library(jsonlite)

# Connect to my-db as defined in /etc/mysql/my.cnf
# con <- dbConnect(RMariaDB::MariaDB(), default.file = '/etc/mysql/my.cnf', group = "lhd")

dbNameVar <- Sys.getenv("MYSQL_LHD_V2_DBNAME")
dbHostVar <- Sys.getenv("MYSQL_LHD_V2_HOST")
dbPasswordVar <- Sys.getenv("MYSQL_LHD_V2_PASSWORD")
dbPortVar <- Sys.getenv("MYSQL_LHD_V2_PORT")
dbUserVar <- Sys.getenv("MYSQL_LHD_V2_USER")
lhdApiPassword <- Sys.getenv("LHD_API_PASSWORD")
con <- dbConnect(RMariaDB::MariaDB(), username = dbUserVar, password = dbPasswordVar, host = dbHostVar, port = dbPortVar, dbname = dbNameVar)

print(con)

statusToImport <- c('Expired','Active')
now <- Sys.time()

latest_versions <- tbl(con, "auth_dsps_version") %>%
  filter(status %in% statusToImport) %>%
  group_by(id_auth_dsps) %>%
  summarise(
    max_version_id = max(id_auth_dsps_version, na.rm = TRUE), 
    record_count = n()
  ) %>%
  ungroup()

dispensations <- tbl(con, "auth_dsps") %>%
  inner_join(tbl(con, "auth_dsps_version"), by = 'id_auth_dsps') %>%
  inner_join(latest_versions, by = 'id_auth_dsps') %>%
  inner_join(tbl(con, "dispensation_subject"), by = 'subject') %>%
  filter(id_auth_dsps_version == max_version_id) %>%
  filter(status %in% statusToImport) %>%
  mutate(
    renewals = record_count - 1,
    modified_by = 'Import',
    modified_on = now,
    subject_other = '',
    auth_dsps = str_replace_all(auth_dsps, "DSPS", "DISP")
    ) %>%
  select('id_auth_dsps' , 'auth_dsps', 'author' , 'id_dispensation_subject' ,'requires' , 'comment', 'status', 'date_start', 'date_end', 'date', 'renewals', 'modified_by', 'modified_on', 'subject_other') %>%
  rename(id_dispensation = id_auth_dsps ) %>%
  rename(dispensation = auth_dsps ) %>%
  rename(created_by = author ) %>%
  rename(created_on = date ) %>%
  collect()

dbAppendTable(con, 'dispensation', dispensations)

rooms <- tbl(con, "auth_dsps_lab") %>%
  inner_join(latest_versions, by = c("id_auth_dsps_version" = "max_version_id")) %>%
  inner_join(tbl(con, "auth_dsps_version"), by = 'id_auth_dsps_version') %>%
  filter(status %in% statusToImport) %>%
  select(id_lab, id_dispensation = id_auth_dsps.x) %>%
  collect()

dbAppendTable(con, 'dispensation_has_room', rooms)

holders <- tbl(con, "auth_dsps_holder") %>%
  inner_join(latest_versions, by = c("id_auth_dsps_version" = "max_version_id")) %>%
  inner_join(tbl(con, "auth_dsps_version"), by = 'id_auth_dsps_version') %>%
  filter(status %in% statusToImport) %>%
  select(id_person, id_dispensation = id_auth_dsps.x) %>%
  collect()

dbAppendTable(con, 'dispensation_has_holder', holders)

dbExecute(con, "UPDATE dispensation SET file_path = 'dispensations/97/Imprimante_3D_AI2208.jpg' WHERE id_dispensation = 97")
dbExecute(con, "update dispensation set file_path='dispensations/47/ugh.pdf' where id_dispensation=47")
dbExecute(con, "update dispensation set file_path='dispensations/76/DI_227743.pdf' where id_dispensation=76")
dbExecute(con, "update dispensation set file_path='dispensations/136/Screenshot_2025-04-28_at_12.00.11.png' where id_dispensation=136")
dbExecute(con, "update dispensation set file_path='dispensations/137/AI_0235.jpg' where id_dispensation=137")
dbExecute(con, "update dispensation set file_path='dispensations/88/Peptide_synthesizer_CH_B3_464.jpg' where id_dispensation=88")
dbExecute(con, "update dispensation set file_path='dispensations/89/1687780510897.jpg' where id_dispensation=89")
dbExecute(con, "update dispensation set file_path='dispensations/84/Photofixateur_UPCDA_SV_2732.2.jpg' where id_dispensation=84")

# Disconnect from the database
dbDisconnect(con)
