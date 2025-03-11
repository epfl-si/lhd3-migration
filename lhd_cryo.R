# get all hazards to migrate
getCryo <- function() {
  resultCryo <- tbl(con, "dewar") %>% 
    inner_join(tbl(con, "cryo")) %>% 
    inner_join(tbl(con, 'lab')) %>%
    collect()
  return(resultCryo)
}

insertCryo <- function(con, formHistory, resultCryo) {
  # --------------------------------------------------- Lab_Has_Hazard
  # mutate data to have to correct structure for insert
  dataToBeMigrated <- resultCryo %>%
    mutate(
      id_hazard_form_history = formHistory$id_hazard_form_history[1],
      submission = paste0('{"data":{"container":{"name":"Dewar"},"typeOfCryoliquid":"',liquid,'","o2DeficiencyDetectorNecessary":false,"numberOfO2DeficiencyDetectors":0,"comment":"","status":"Default","delete":false,"volumeL":"',ifelse(is.na(liters), '', liters),'"}}')
    ) %>% 
    select('id_lab', 'id_hazard_form_history', 'submission') 
  
  dbAppendTable(con, 'lab_has_hazards', dataToBeMigrated)
  return(dataToBeMigrated)
  # ---------------------------------------------------
}

getCommentsCryo <- function(resultCryo) {
  # get all comments in one single field for each room
  comments <- resultCryo %>% 
    group_by(id_lab) %>% 
    mutate(comment = paste0(ifelse(is.na(comment), '', gsub("\r?\n", "\\\\n", comment)), collapse = "\n")) %>%
    select('id_lab', 'comment') %>%
    distinct(id_lab, comment)
  return(comments)
}