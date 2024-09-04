library(RMariaDB)
library(DBI)
library(dplyr)
library(dbplyr)

source('lhd_biological.R') # max_id~1000
source('lhd_chemical.R')
source('lhd_gas.R')
source('lhd_cryo.R')
source('lhd_laser.R')
source('lhd_mag.R') # ~160
source('lhd_electrical.R')
source('lhd_emr.R')
source('lhd_ils.R')
source('lhd_nano.R') # ~1200
source('lhd_biological_filePath.R')

# Connect to my-db as defined in /etc/mysql/my.cnf
# con <- dbConnect(RMariaDB::MariaDB(), default.file = '/etc/mysql/my.cnf', group = "lhd")
dbNameVar <- Sys.getenv("MYSQL_LHD_V2_DBNAME")
dbHostVar <- Sys.getenv("MYSQL_LHD_V2_HOST")
dbPasswordVar <- Sys.getenv("MYSQL_LHD_V2_PASSWORD")
dbPortVar <- Sys.getenv("MYSQL_LHD_V2_PORT")
dbUserVar <- Sys.getenv("MYSQL_LHD_V2_USER")
con <- dbConnect(RMariaDB::MariaDB(), username = dbUserVar, password = dbPasswordVar, host = dbHostVar, port = dbPortVar, dbname = dbNameVar)

print(con)

# --------------------------------------------------- Base data
# get category by category name
category <- function(categoryName) {
  categoryObject <- tbl(con, "hazard_category") %>%
    filter(hazard_category_name == categoryName) %>% 
    select('id_hazard_category')%>% 
    collect()
  return(categoryObject)
}

# get last form history by category name
formHistory <- function(categoryName) {
  formHistoryObject <- tbl(con, "hazard_form_history") %>%
    inner_join(tbl(con, "hazard_form")) %>%
    inner_join(tbl(con, "hazard_category")) %>%
    filter(hazard_category_name == categoryName) %>% 
    select('id_hazard_form_history', 'version') %>% 
    collect()
  return(formHistoryObject)
}

# get last form child history by category name
formChildHistory <- function(categoryName) {
  formChildHistoryObject <- tbl(con, "hazard_form_child_history") %>%
    inner_join(tbl(con, "hazard_form_child")) %>%
    inner_join(tbl(con, "hazard_form"), by = 'id_hazard_form') %>%
    inner_join(tbl(con, "hazard_category")) %>%
    filter(hazard_category_name == categoryName) %>% 
    select('id_hazard_form_child_history') %>% 
    collect()
  return(formChildHistoryObject)
}
# ---------------------------------------------------

# --------------------------------------------------- Additional Info
addAdditionalInfo <- function(data, categoryName, comments) {
  # get the correct structure for insert into lab_has_hazards_additional_info
  if(is.null(comments)) {
    vec <- list(id_lab = integer(0), comment = character(0))
    comments <- bind_rows(vec)
  }
  additionalInfo <- data %>% 
    left_join(comments, by = 'id_lab') %>%
    distinct(id_lab, comment) %>% 
    mutate(
      id_hazard_category = category(categoryName)$id_hazard_category,
      modified_by = 'Import from LHDv2',
      modified_on = Sys.time()
    )
  
  dbAppendTable(con, 'lab_has_hazards_additional_info', additionalInfo)
}
# ---------------------------------------------------

biologicalToBeMigrated <- insertBiological(con, formHistory('Biological'), formChildHistory('Biological'))
addAdditionalInfo(biologicalToBeMigrated, 'Biological', NULL)

magneticToBeMigrated <- insertMagnetic(con, formHistory('StaticMagneticField'), formChildHistory('StaticMagneticField'))
addAdditionalInfo(magneticToBeMigrated, 'StaticMagneticField', NULL)

nanoToBeMigrated <- insertNano(con, formHistory('Nanoparticles'), formChildHistory('Nanoparticles'))
addAdditionalInfo(nanoToBeMigrated, 'Nanoparticles', NULL)

chemicalToBeMigrated <- insertChemical(con, formHistory('Chemical'))
addAdditionalInfo(chemicalToBeMigrated, 'Chemical', NULL)

gasToBeMigrated <- insertGas(con, formHistory('CompressedGas'))
addAdditionalInfo(gasToBeMigrated, 'CompressedGas', NULL)

resultCryo <- getCryo()
cryoToBeMigrated <- insertCryo(con, formHistory('Cryogenics'), resultCryo)
addAdditionalInfo(cryoToBeMigrated, 'Cryogenics', getCommentsCryo(resultCryo))

gasToBeMigrated <- insertLaser(con, formHistory('Laser'))
addAdditionalInfo(gasToBeMigrated, 'Laser', NULL)

electricalToBeMigrated <- insertElectrical(con, formHistory('Electrical'))
addAdditionalInfo(electricalToBeMigrated, 'Electrical', NULL)

emrToBeMigrated <- insertEmr(con, formHistory('TimeVaryingEMF'))
addAdditionalInfo(emrToBeMigrated, 'TimeVaryingEMF', NULL)

ilsToBeMigrated <- insertIls(con, formHistory('IncoherentLightSource'))
addAdditionalInfo(ilsToBeMigrated, 'IncoherentLightSource', NULL)

insertBioOrgFilePath()

# ---------------------------------------------------

# Disconnect from the database
dbDisconnect(con)