library(RMariaDB)
library(DBI)
library(dplyr)
library(dbplyr)
library(stringr)
library(httr)
library(jsonlite)
library(readxl)

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

# file_path <- "/home/rmaggi/dev/R/lhd3-migration/import_radio.xlsx"
file_path <- "/scripts/import_radio.xlsx"

data <- read_excel(file_path) 

auth <- data %>% 
  select('id_authorization' , 'authorization', 'id_unit' ,'expiration_date' ,  'status','issue_date' , 'authority') %>%
  rename(creation_date = issue_date ) %>%
  mutate(
    renewals = 0,
    type = 'IonisingRadiation'
  ) %>%
  filter(!is.na(status)) %>%
  collect()
dbAppendTable(con, 'authorization', distinct(auth, authorization,.keep_all = TRUE))


sources <- data %>% 
  select('id_authorization' , 'sources') %>%
  rename(source = sources) %>%
  collect()
dbAppendTable(con, 'authorization_has_radiation', distinct(sources,.keep_all = TRUE))

person_local <- tbl(con, 'person') %>%
  collect()

holdersNotFound <- data %>%
  filter(!is.na(sciper)) %>%
  left_join(person_local, by = 'sciper') %>%
  select('id_authorization' , 'id_person', 'authorization', 'sciper') %>%
  filter(is.na(id_person)) %>%
  collect()

holders <- data %>%
  filter(!is.na(sciper)) %>%
  left_join(person_local, by = 'sciper') %>%
  select('id_authorization' , 'id_person') %>%
  filter(!is.na(id_person)) %>%
  collect()
dbAppendTable(con, 'authorization_has_holder', distinct(holders,.keep_all = TRUE))

lab_local <- tbl(con, "lab") %>%
  collect()

roomsNotFound <- data %>%
  left_join(lab_local, by = 'lab_display') %>%
  select('id_authorization' , 'id_lab', 'authorization', 'lab_display') %>%
  filter(is.na(id_lab)) %>%
  collect()

rooms <- data %>%
  left_join(lab_local, by = 'lab_display') %>%
  select('id_authorization' , 'id_lab') %>%
  filter(!is.na(id_lab)) %>%
  collect()
dbAppendTable(con, 'authorization_has_room', distinct(rooms,.keep_all = TRUE))
