insertNano <- function(con, formHistory, formChildHistory) {
  # ---------------------------------------------------Lab_Has_Hazard
  # get all hazards to migrate
  result <- tbl(con, "nano") %>%
    collect()
  
  # mutate data to have to correct structure for insert
  dataToBeMigrated <- result %>%
    mutate(
      id_hazard_form_history = formHistory$id_hazard_form_history[1],
      id_lab_has_hazards = 1200 + id_nano,
      submission = paste0('{"data":{"status":"Default","delete":false,"roomClass":',nano_class,'}}')
    ) %>% 
    select('id_lab_has_hazards', 'id_lab', 'id_hazard_form_history', 'submission')

  dbAppendTable(con, 'lab_has_hazards', dataToBeMigrated)
  # ---------------------------------------------------
  
  # ---------------------------------------------------Lab_Has_Hazard Child
  # mutate data to have to correct structure for insert children
  dataToBeMigratedForChild <- result %>%
    filter(!is.na(nano_type)) %>%
    mutate(
      id_hazard_form_child_history = formChildHistory$id_hazard_form_child_history[1],
      id_lab_has_hazards = 1200 + id_nano,
      submission = paste0('{"data":{"materialType":"',nano_type,'","status":"Default","delete":false}}')
    ) %>% 
    select('id_lab_has_hazards', 'id_hazard_form_child_history', 'submission')
  
  dbAppendTable(con, 'lab_has_hazards_child', dataToBeMigratedForChild)
  # ---------------------------------------------------
  return(dataToBeMigrated)
}
