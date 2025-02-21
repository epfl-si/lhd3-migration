library(RMariaDB)
library(DBI)
library(dplyr)
library(dbplyr)
library(httr)
library(jsonlite)
library(stringr)

# Connect to my-db as defined in /etc/mysql/my.cnf
con <- dbConnect(RMariaDB::MariaDB(), default.file = '/etc/mysql/my.cnf', group = "lhd")

# dbNameVar <- Sys.getenv("MYSQL_LHD_V2_DBNAME")
# dbHostVar <- Sys.getenv("MYSQL_LHD_V2_HOST")
# dbPasswordVar <- Sys.getenv("MYSQL_LHD_V2_PASSWORD")
# dbPortVar <- Sys.getenv("MYSQL_LHD_V2_PORT")
# dbUserVar <- Sys.getenv("MYSQL_LHD_V2_USER")
# lhdApiPassword <- Sys.getenv("LHD_API_PASSWORD")
# con <- dbConnect(RMariaDB::MariaDB(), username = dbUserVar, password = dbPasswordVar, host = dbHostVar, port = dbPortVar, dbname = dbNameVar)

getLhdLabs <- function(con) {
  scipersId <- tbl(con, "lab") %>% 
    filter(!is.na(sciper_lab)) %>% 
    select('sciper_lab', 'id_labType', 'lab_type_is_different') %>% 
    collect()
  return(scipersId)
}

getScipersFromLhd <- function(con, labs) {
  scipersId <- labs %>% 
    select('sciper_lab') %>% 
    collect()
  return(scipersId)
}

getVolume <- function(surface, height) {
  s <- ifelse(is.na(surface), 0, surface)
  h <- ifelse(is.na(height), 0, height)
  vol <- s * h
  v <- round(vol, 2)
  return(v)
}

getLabType <- function(sciper, apiLabType, lhdLabs) {
  lab <- lhdLabs %>%
    filter(sciper_lab == sciper) %>%
    collect()
  
  lhdLabType <- tbl(con, "labType") %>% 
    filter(id_labType == lab$id_labType) %>% 
    collect()
  
  if (!lab$lab_type_is_different) {
    newLhdLabType <- tbl(con, "labType") %>% 
      filter(labType == apiLabType) %>% 
      collect()
    value <- data.frame(
      lab_type = newLhdLabType$id_labType[1],
      is_different = FALSE
    )
    return(value)
  } else {
    if (apiLabType == lhdLabType$labType) {
      newLhdLabType <- tbl(con, "labType") %>% 
        filter(labType == apiLabType) %>% 
        collect()
      value <- data.frame(
        lab_type = newLhdLabType$id_labType[1],
        is_different = FALSE
      )
      return(value)
    } else {
      value <- data.frame(
        lab_type = lhdLabType$id_labType,
        is_different = TRUE
      )
      return(value)
    }
  }
}

lhdLabs <- getLhdLabs(con)
lhdScipers <- getScipersFromLhd(con, lhdLabs)

ids <- paste0(lhdScipers$sciper_lab, collapse = ',')
body_json <- paste0('{"endpoint": "/v1/rooms", "params": {"ids": "',ids,'"}}')
final_body <- str_replace_all(body_json, "\n", "")

apiRooms <- POST(
  'https://api.epfl.ch/v1/rooms/getter',
  authenticate('lhd', lhdApiPassword),
  add_headers(`Content-Type` = "application/json"),
  body = final_body,
  encode = "json"
)

# Check the apiRooms status
if (status_code(apiRooms) == 200) {
  content <- fromJSON(rawToChar(apiRooms$content))
  listFromApi <- content$rooms
  
  lhdLabType2 <- tbl(con, "labType") %>% 
    collect()
  
  df <- as.data.frame(listFromApi)
  diffScipers <- setdiff(lhdScipers$sciper_lab, df$id)
  difflabType <- setdiff(df$facultyuse, lhdLabType2$labType)
  print(diffScipers)
  for (i in 1:nrow(df)) {
    parts <- strsplit(df$name[i], " ")[[1]]
    labId <- tail(parts, 1)
    newLabType <- getLabType(df$id[i], str_trim(df$facultyuse[i]), lhdLabs)
    query <- paste0('UPDATE lab SET building = "', df$building$name[i], '"', ifelse(df$sector[i] != "Z", paste0(', sector = "', df$zone[i], '"'), ""), ', floor = "', 
                    df$floor[i], '", lab = "', labId, '", lab_display = "', df$name[i], '", site = "', df$building$site$label[i], '", vol = ', getVolume(df$surface[i], df$height[i]), 
                    ', lab_type_is_different = ', newLabType$is_different, ', id_labType = ', newLabType$lab_type,' WHERE sciper_lab = ',df$id[i])

    print(query)
    dbExecute(con, query)
  }
} else {
  print(paste("Error:", apiUnits))
}

# ---------------------------------------------------

# Disconnect from the database
dbDisconnect(con)