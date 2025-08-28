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

saveNewPerson <- function(sciper) {
  url <- paste0("https://api.epfl.ch/v1/persons?query=",sciper)
  print(url)
  api <- GET(
    url,
    authenticate('lhd',lhdApiPassword),
    add_headers(`accept` = "application/json")
  )
  content <- fromJSON(rawToChar(api$content))
  personFromApi <- content$persons
  if (!is.null(personFromApi) && length(personFromApi) > 0) {
    df <- as.data.frame(personFromApi)
    expected_cols <- c("lastname", "firstname", "email", "id")
    for (col in expected_cols) {
      if (!(col %in% colnames(df))) {
        df[[col]] <- NA
      }
    }

    user <- df %>%
      select(
        surname_person = lastname,
        name_person = firstname,
        email_person = email,
        sciper = id
      )
    
    dbAppendTable(con, "person", user)
    
    # Retrieve the id of the newly inserted row
    # Assuming `sciper` is unique
    new_id <- dbGetQuery(con, paste0(
      "SELECT id_person FROM person WHERE sciper = '", sciper, "'"
    ))
    return(new_id)
  }
  print(-1)
  return(-1)
}

status <- c('Expired','Active')

oldValues <- tbl(con, "auth_sst") %>%
filter(status_auth_sst %in% status) %>% 
select('id_auth_sst' , 'auth_sst', 'id_unit_auth_sst' , 'date_auth_sst' ,'status_auth_sst' , 'created_at') %>%
  rename(authorization = auth_sst ) %>% 
  rename(id_unit = id_unit_auth_sst) %>% 
  rename(expiration_date = date_auth_sst) %>% 
  rename(status = status_auth_sst) %>% 
  rename(creation_date = created_at) %>% 
collect()

result <- oldValues %>%
  rowwise() %>%
  mutate(
    parts = list(strsplit(authorization, "-", fixed = TRUE)[[1]]),
    base_name = if (length(parts) == 3) paste(parts[1:2], collapse = "-") else authorization,
    suffix = if (length(parts) == 3) as.integer(parts[3]) else 0,
    type = 'Chemical'
  ) %>%
  ungroup() %>%
  group_by(base_name) %>%
  # Add earliest created date from group
  mutate(creation_date = min(creation_date, na.rm = TRUE)) %>%
  # Pick newest expiration
  slice_max(order_by = expiration_date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(id_authorization = id_auth_sst, authorization = base_name, renewals = suffix, id_unit, expiration_date, status, creation_date, type) %>%
  filter(!is.na(expiration_date)) %>%
  collect()

dbAppendTable(con, 'authorization', result)

rooms <- tbl(con, "auth_sst") %>%
  left_join(tbl(con, 'auth_req'), by = 'id_auth_req') %>%
  left_join(tbl(con, 'auth_lab'), by = 'id_auth_req') %>%
  distinct(id_lab, id_auth_sst) %>%
  filter(!is.na(id_lab)) %>%
  select(id_lab, id_authorization = id_auth_sst) %>%
  collect()

for (i in seq_len(nrow(result))) {
  r <- result[i, ]
  filteredRooms <- rooms %>% filter(id_authorization == r$id_authorization)
  dbAppendTable(con, 'authorization_has_room', filteredRooms)
}

holders <- tbl(con, "auth_holder") %>%
  inner_join(tbl(con, 'auth_sst'), by = 'id_auth_sst') %>%
  left_join(tbl(con, 'person'), by = 'sciper') %>%
  select(sciper, id_person) %>%
  distinct(sciper, id_person) %>%
  collect()

for (i in seq_len(nrow(holders))) {
  r <- holders[i, ]
  if (is.na(r$id_person)) {
    saveNewPerson(r$sciper)
  } else {
    print(paste0(r$sciper, 'already exists'))
  }
}

holdersFiltered <- tbl(con, "auth_holder") %>%
  inner_join(tbl(con, 'auth_sst'), by = 'id_auth_sst') %>%
  left_join(tbl(con, 'person'), by = 'sciper') %>%
  select(sciper, id_person, id_authorization = id_auth_sst) %>%
  distinct(id_person, id_authorization) %>%
  filter(!is.na(id_person)) %>%
  collect()

for (i in seq_len(nrow(result))) {
  r <- result[i, ]
  filteredHolders <- holdersFiltered %>% filter(id_authorization == r$id_authorization)
  dbAppendTable(con, 'authorization_has_holder', filteredHolders)
}

chemicals <- tbl(con, "auth_sst") %>%
  left_join(tbl(con, 'auth_req'), by = 'id_auth_req') %>%
  left_join(tbl(con, 'auth_rchem'), by = 'id_auth_req') %>%
  distinct(id_auth_chem, id_auth_sst) %>%
  filter(!is.na(id_auth_chem)) %>%
  select(id_chemical = id_auth_chem, id_authorization = id_auth_sst) %>%
  collect()

for (i in seq_len(nrow(result))) {
  r <- result[i, ]
  filteredchemicals <- chemicals %>% filter(id_authorization == r$id_authorization)
  dbAppendTable(con, 'authorization_has_chemical', filteredchemicals)
}

# Disconnect from the database
dbDisconnect(con)