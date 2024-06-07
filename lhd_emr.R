insertEmr <- function(con, formHistory) {
  # --------------------------------------------------- Lab_Has_Hazard
  # get all hazards to migrate
  result <- tbl(con, "nirad") %>%
    filter(nirad_type == 'EM') %>%
    collect()
  
  # mutate data to have to correct structure for insert
  dataToBeMigrated <- result %>%
    mutate(
      id_hazard_form_history = formHistory$id_hazard_form_history[1],
      submission = paste0('{"data":{"emRadiation":"EMF f<300Hz","comment":"","status":"Default","delete":false,"frequencyMHz":"',ifelse(is.na(freq), '', freq/1000000),'"}}'),
    ) %>%
    select('id_lab', 'id_hazard_form_history', 'submission')
  
  dbAppendTable(con, 'lab_has_hazards', dataToBeMigrated)
  return(dataToBeMigrated)
  # ---------------------------------------------------
}