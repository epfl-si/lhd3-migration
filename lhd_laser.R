insertLaser <- function(con, formHistory) {
  # --------------------------------------------------- Lab_Has_Hazard
  # get all hazards to migrate
  result <- tbl(con, "laser") %>%
    collect()
  
  # mutate data to have to correct structure for insert
  dataToBeMigrated <- result %>%
    mutate(
      id_hazard_form_history = formHistory$id_hazard_form_history[1],
      submission = paste0('{"data":{"laserClass":"',replaceNA(laser_class),'","laserMode":"',replaceNA(laser_mode),'","status":"Default","delete":false,"wavelengthNM":"',replaceNA(laser_wave),'","powerW":"',replaceNA(laser_power),'","energyMJ":"',ifelse(is.na(laser_energy), '', laser_energy*1000),'","pulseDurationNs":"',replaceNA(laser_pulse),'","frequencyHz":"',replaceNA(laser_frequency),'"}}')
    ) %>% 
    select('id_lab', 'id_hazard_form_history', 'submission')
  
  dbAppendTable(con, 'lab_has_hazards', dataToBeMigrated)
  return(dataToBeMigrated)
  # ---------------------------------------------------
}

replaceNA <- function(field) {
  return(ifelse(is.na(field), '', field))
}
