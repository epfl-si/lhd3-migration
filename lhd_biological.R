insertBiological <- function(con, formHistory, formChildHistory) {
  # ---------------------------------------------------Lab_Has_Hazard
  # get all hazards to migrate
  result <- tbl(con, "bio") %>% 
    inner_join(tbl(con, 'lab'), by = 'id_lab') %>% 
    left_join(tbl(con, 'bio_org_lab'), by = 'id_bio') %>% 
    left_join(tbl(con, 'bio_org'), by = 'id_bio_org') %>% 
    collect()
  
  # mutate data to have to correct structure for insert
  dataToBeMigrated <- result %>%
    mutate(
      id_hazard_form_history = formHistory$id_hazard_form_history[1],
      submission = paste0('{"data":{"comment":"',ifelse(is.na(comment), '', comment),'","status":"Default","delete":false,"biosafetyLevel":',bio_level,'}}')
    ) %>% 
    select('id_bio', 'id_lab', 'id_hazard_form_history', 'submission')  %>%
    rename(id_lab_has_hazards = id_bio) %>%
    distinct(id_lab_has_hazards, id_lab, id_hazard_form_history, submission)
  
  dbAppendTable(con, 'lab_has_hazards', dataToBeMigrated)
  # ---------------------------------------------------
  
  # ---------------------------------------------------Lab_Has_Hazard Child
  # mutate data to have to correct structure for insert children
  dataToBeMigratedForChild <- result %>%
    mutate(
      id_hazard_form_child_history = formChildHistory$id_hazard_form_child_history[1],
      submission = paste0('{"data":{"organism":{"organism":"',organism,'","risk_group":',risk_group,',"fileLink":"https://lhd.epfl.ch/hazards/bio/lhd_bio_file.php?id=',id_bio_org,'"},"status":"Default","delete":false,"riskGroup":',risk_group,',"linkFile":"https://lhd.epfl.ch/hazards/bio/lhd_bio_file.php?id=',id_bio_org,'"}}')
    ) %>% 
    select('id_bio', 'id_hazard_form_child_history', 'submission', 'organism') %>%
    filter(!is.na(organism)) %>%
    select('id_bio', 'id_hazard_form_child_history', 'submission') %>%
    rename(id_lab_has_hazards = id_bio)
  
  dbAppendTable(con, 'lab_has_hazards_child', dataToBeMigratedForChild)
  # ---------------------------------------------------
  return(dataToBeMigrated)
}
