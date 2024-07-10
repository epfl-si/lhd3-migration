library(RMariaDB)
library(DBI)
library(dplyr)
library(dbplyr)

source('/home/rmaggi/dev/R/lhd3-migration/lhd_biological.R') # max_id~1000
source('/home/rmaggi/dev/R/lhd3-migration/lhd_chemical.R')
source('/home/rmaggi/dev/R/lhd3-migration/lhd_gas.R')
source('/home/rmaggi/dev/R/lhd3-migration/lhd_cryo.R')
source('/home/rmaggi/dev/R/lhd3-migration/lhd_laser.R')
source('/home/rmaggi/dev/R/lhd3-migration/lhd_mag.R') # ~160
source('/home/rmaggi/dev/R/lhd3-migration/lhd_electrical.R')
source('/home/rmaggi/dev/R/lhd3-migration/lhd_emr.R')
source('/home/rmaggi/dev/R/lhd3-migration/lhd_ils.R')
source('/home/rmaggi/dev/R/lhd3-migration/lhd_nano.R') # ~1200
source('/home/rmaggi/dev/R/lhd3-migration/lhd_biological_filePath.R')

# Connect to my-db as defined in /etc/mysql/my.cnf
con <- dbConnect(RMariaDB::MariaDB(), default.file = '/etc/mysql/my.cnf', group = "lhd")

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