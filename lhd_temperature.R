insertTemp <- function(con, formHistory) {
  # --------------------------------------------------- Lab_Has_Hazard
  # get all hazards to migrate
  result <- tbl(con, "tdegree") %>%
    filter(!is.null(tdegree)) %>%
    collect()
  
  # mutate data to have to correct structure for insert
  dataToBeMigrated <- result %>%
    rowwise() %>%
    mutate(
      id_hazard_form_history = formHistory$id_hazard_form_history[1],
      level = level(tdegree_type, tdegree),
      submission = paste0('{"data":{"type":"',ifelse(tdegree_type=='CO', 'Hot surface', ifelse(tdegree>=28, 'Hot environment', 'Cold environment')),'"',level,',"status":"Default","delete":false,"temperature":',tdegree,'}}')
    ) %>%
    ungroup() %>%
    select('id_lab', 'id_hazard_form_history', 'submission')
  
  dbAppendTable(con, 'lab_has_hazards', dataToBeMigrated)
  return(dataToBeMigrated)
  # ---------------------------------------------------
}

level <- function(tdegree_type, tdegree) {
  if (tdegree_type == "EN" && tdegree >= 28) {
    return(paste0(',"level":',2))
  } else if (tdegree_type == "CO" && tdegree  >= 60 && tdegree  <= 69) {
    return(paste0(',"level":',1))
  } else if (tdegree_type == "CO" && tdegree  >= 70 && tdegree  <= 89) {
    return(paste0(',"level":',2))
  } else if (tdegree_type == "CO" && tdegree  >= 90) {
    return(paste0(',"level":',3))
  } else if (tdegree_type == "EN" && tdegree  >= 10 && tdegree  <= 15) {
    return(paste0(',"level":',1))
  } else if (tdegree_type == "EN" && tdegree  >= -5 && tdegree  <= 9) {
    return(paste0(',"level":',2))
  } else if (tdegree_type == "EN" && tdegree  >= -18 && tdegree  <= -6) {
    return(paste0(',"level":',3))
  } else if (tdegree_type == "EN" && tdegree  <= -19) {
    return(paste0(',"level":',4))
  } else {
    return('');
  }
}