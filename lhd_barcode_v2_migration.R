library(RMariaDB)
library(DBI)
library(dplyr)
library(dbplyr)

# Connect to my-db as defined in /etc/mysql/my.cnf
con <- dbConnect(RMariaDB::MariaDB(), default.file = '/etc/mysql/my.cnf', group = "lhd")
con_barcode <- dbConnect(RMariaDB::MariaDB(), default.file = '/etc/mysql/my.cnf', group = "barcode_test")

# dbNameVar <- Sys.getenv("MYSQL_LHD_V2_DBNAME")
# dbHostVar <- Sys.getenv("MYSQL_LHD_V2_HOST")
# dbPasswordVar <- Sys.getenv("MYSQL_LHD_V2_PASSWORD")
# dbPortVar <- Sys.getenv("MYSQL_LHD_V2_PORT")
# dbUserVar <- Sys.getenv("MYSQL_LHD_V2_USER")
# lhdApiPassword <- Sys.getenv("LHD_API_PASSWORD")
# con <- dbConnect(RMariaDB::MariaDB(), username = dbUserVar, password = dbPasswordVar, host = dbHostVar, port = dbPortVar, dbname = dbNameVar)

storage_catalyse <- dbReadTable(con, "storage_catalyse")
stoType_catalyse <- dbReadTable(con, "stoType_catalyse")
stoProperty_catalyse <- dbReadTable(con, "stoProperty_catalyse")
stoPlace_catalyse <- dbReadTable(con, "stoPlace_catalyse")
sub_storage_catalyse <- dbReadTable(con, "sub_storage_catalyse")

dbWriteTable(con_barcode, 'storage_catalyse', storage_catalyse)
dbWriteTable(con_barcode, 'stoType_catalyse', stoType_catalyse)
dbWriteTable(con_barcode, 'stoProperty_catalyse', stoProperty_catalyse)
dbWriteTable(con_barcode, 'stoPlace_catalyse', stoPlace_catalyse)
dbWriteTable(con_barcode, 'sub_storage_catalyse', sub_storage_catalyse)

# Disconnect from the database
dbDisconnect(con)
dbDisconnect(con_barcode)
