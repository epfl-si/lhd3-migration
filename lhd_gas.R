insertGas <- function(con, formHistory) {
  # --------------------------------------------------- Lab_Has_Hazard
  # get all hazards to migrate
  result <- tbl(con, "gaschem") %>% 
    inner_join(tbl(con, "haz")) %>% 
    inner_join(tbl(con, 'lab')) %>%
    filter(haz_category=='Gas') %>%
    collect()
  
  # mutate data to have to correct structure for insert
  dataToBeMigrated <- result %>%
    mutate(
      id_hazard_form_history = formHistory$id_hazard_form_history[1],
      submission = paste0('{"data":{"gas":"',haz_en,'","status":"Default","delete":false,"level":',score,'}}')
    ) %>% 
    select('id_lab', 'id_hazard_form_history', 'submission') 
  
  dbAppendTable(con, 'lab_has_hazards', dataToBeMigrated)
  return(dataToBeMigrated)
  # ---------------------------------------------------
}