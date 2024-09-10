insertTemp <- function(con, formHistory) {
  # --------------------------------------------------- Lab_Has_Hazard
  # get all hazards to migrate
  result <- tbl(con, "tdegree") %>%
    collect()
  
  # mutate data to have to correct structure for insert
  dataToBeMigrated <- result %>%
    mutate(
      id_hazard_form_history = formHistory$id_hazard_form_history[1],
      submission = paste0('{"data":{"type":"',ifelse(is.na(tdegree_type), '', ifelse(tdegree_type=='EN', 'Environment', ifelse(tdegree_type=='CO', 'Contact', 'Projection'))),'","place":"',ifelse(is.na(tdegree_place), '', ifelse(tdegree_place=='I', 'Indoor', 'Outdoor')),'","status":"Default","delete":false,"temperature":',tdegree,'}}'),
    ) %>%
    select('id_lab', 'id_hazard_form_history', 'submission')
  
  dbAppendTable(con, 'lab_has_hazards', dataToBeMigrated)
  return(dataToBeMigrated)
  # ---------------------------------------------------
}