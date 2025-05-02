library(RMariaDB)
library(DBI)
library(dbplyr)
library(httr)
library(jsonlite)
library(stringr)
library(dplyr)

# Connect to my-db as defined in /etc/mysql/my.cnf
# con <- dbConnect(RMariaDB::MariaDB(), default.file = '/etc/mysql/my.cnf', group = "lhd")

dbNameVar <- Sys.getenv("MYSQL_LHD_V2_DBNAME")
dbHostVar <- Sys.getenv("MYSQL_LHD_V2_HOST")
dbPasswordVar <- Sys.getenv("MYSQL_LHD_V2_PASSWORD")
dbPortVar <- Sys.getenv("MYSQL_LHD_V2_PORT")
dbUserVar <- Sys.getenv("MYSQL_LHD_V2_USER")
lhdApiPassword <- Sys.getenv("LHD_API_PASSWORD")
con <- dbConnect(RMariaDB::MariaDB(), username = dbUserVar, password = dbPasswordVar, host = dbHostVar, port = dbPortVar, dbname = dbNameVar)

getScipersFromLhd <- function(con) {
  scipersId <- tbl(con, "unit") %>%
    filter(!is.na(sciper_unit)) %>%
    select('sciper_unit') %>%
    collect()
  return(scipersId)
}

searchResponsible <- function(con, idResponsable) {
  lhdResponsable <- tbl(con, "person") %>%
    filter(sciper == idResponsable) %>%
    select('id_person') %>%
    collect()

  return(lhdResponsable)
}

searchOrCreateResponsible <- function(con, idResponsable, responsible, i) {
  lhdResponsable <- searchResponsible(con, idResponsable)

  if(count(lhdResponsable) == 0) {
    newPerson <- data.frame(
      name = responsible$firstname[i],
      surname = responsible$lastname[i],
      sciper = responsible$id[i],
      email = responsible$email[i]
    )

    dbAppendTable(con, 'person', newPerson)
    lhdResponsable <- searchResponsible(con, idResponsable)
  }
  return(lhdResponsable)
}

createSubunPro <- function(con, responsibleid, unitSciper) {
  unitLhd <- tbl(con, "unit") %>%
    filter(sciper_unit == unitSciper) %>%
    select('id_unit') %>%
    collect()
  subunpro <- tbl(con, "subunpro") %>%
    filter(id_unit == unitLhd$id_unit, id_person == responsibleid) %>%
    collect()

  if(count(subunpro) == 0) {
    newSubunPro <- data.frame(
      id_unit = unitLhd$id_unit,
      id_person = responsibleid
    )

    dbAppendTable(con, 'subunpro', newSubunPro)
  }
}

searchFaculty <- function(con, name) {
  lhdFaculty <- tbl(con, "faculty") %>%
    filter(name_faculty == name) %>%
    select('id_faculty') %>%
    collect()

  return(lhdFaculty)
}

searchOrCreateFaculty <- function(con, name) {
  lhdFaculty <- searchFaculty(con, name)

  if(count(lhdFaculty) == 0) {
    newFaculty <- data.frame(
      name_faculty = name
    )

    dbAppendTable(con, 'faculty', newFaculty)
    lhdFaculty <- searchFaculty(con, name)
  }
  return(lhdFaculty)
}

searchInstitut <- function(con, name) {
  lhdInstitut <- tbl(con, "institut") %>%
    filter(name_institut == name) %>%
    select('id_institut') %>%
    collect()

  return(lhdInstitut)
}

searchOrCreateInstitut <- function(con, name, faculty) {
  lhdInstitut <- searchInstitut(con, name)

  if(count(lhdInstitut) == 0) {
    newInstitut <- data.frame(
      name_institut = name,
      id_faculty = faculty
    )

    dbAppendTable(con, 'institut', newInstitut)
    lhdInstitut <- searchInstitut(con, name)
  }
  return(lhdInstitut)
}

lhdUnitScipers <- getScipersFromLhd(con)

ids <- paste0(lhdUnitScipers$sciper_unit, collapse = ',')
body_json <- paste0('{"endpoint": "/v1/units", "params": {"ids": "',ids,'"}}')
final_body <- str_replace_all(body_json, "\n", "")

apiUnits <- POST(
  'https://api.epfl.ch/v1/units/getter',
  authenticate('lhd', lhdApiPassword),
  add_headers(`Content-Type` = "application/json"),
  body = final_body,
  encode = "json"
)

# Check the apiUnits status
if (status_code(apiUnits) == 200) {
  content <- fromJSON(rawToChar(apiUnits$content))
  unitListFromApi <- content$units
  df <- as.data.frame(unitListFromApi)
  diffScipers <- setdiff(lhdUnitScipers$sciper_unit, df$id)
  # print(diffScipers)
  for (i in 1:nrow(df)) {
    pathArray <- strsplit(df$path[i], split = " ")
    institut <- vapply(pathArray, `[`, 3, FUN.VALUE=character(1))
    faculty <- vapply(pathArray, `[`, 2, FUN.VALUE=character(1))
    lhdFaculty <- searchOrCreateFaculty(con, faculty)
    lhdInstitut <- searchOrCreateInstitut(con, institut, lhdFaculty$id_faculty)

    idResponsable <-df$responsibleid[i]
    if (!is.na(idResponsable) && idResponsable != '') {
      lhdResponsable <- searchOrCreateResponsible(con,idResponsable, df$responsible, i)
    }
    query <- paste0("UPDATE unit SET responsible_id = ", ifelse(is.na(idResponsable), NULL, lhdResponsable$id_person), ", id_institut = ", lhdInstitut$id_institut,", name_unit = '",df$name[i],"' WHERE sciper_unit = ",df$id[i])

    dbExecute(con, query)
    createSubunPro(con, lhdResponsable$id_person, df$id[i])
  }
} else {
  # print(paste("Error:", apiUnits))
}

# ---------------------------------------------------

# Disconnect from the database
dbDisconnect(con)
