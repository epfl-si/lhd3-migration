insertMagnetic <- function(con, formHistory, formChildHistory) {
  mag <- tbl(con, "mag") %>% 
    rename(comment = mag_comment,
           id_lab_mag = id_lab) %>%
    left_join(tbl(con, 'mag_f'), by = 'id_mag') %>%
    filter(!is.na(line_place)) %>%
    collect()
  
  # ---------------------------------------------------Lab_Has_Hazard
  # # mutate data to have to correct structure for insert
  dataToBeMigrated <- mag %>%
    filter(id_lab_mag == id_lab) %>%
    mutate(
      id_lab_has_hazards = id_mag + 1000,
      id_hazard_form_history = formHistory$id_hazard_form_history[1],
      submission = paste0('{"data":{"status":"Default","delete":false,"maximumField":"',ifelse(is.na(bmax), '', bmax),'"},"metadata":{}}')
    ) %>%
    select('id_lab_has_hazards', 'id_lab', 'id_hazard_form_history', 'submission') %>%
    distinct(id_lab_has_hazards, id_lab, id_hazard_form_history, submission)

  dbAppendTable(con, 'lab_has_hazards', dataToBeMigrated)
  # ---------------------------------------------------
  
  # ---------------------------------------------------Lab_Has_Hazard Child
  labs <- tbl(con, "lab") %>%
    collect()
  
  otherRooms <- mag %>%
    filter(id_lab_mag != id_lab) %>%
    # select(id_lab, line) %>%
    inner_join(labs) %>%
    rename(id_lab_other = id_lab,
           line_other_room = line) %>%
    mutate(
      lab_displayJson = paste0('"',lab_display,'"')
    ) %>%
    group_by(id_lab_mag, line_other_room) %>%
    summarize(other_rooms = paste0(lab_displayJson, collapse = ","), .groups = 'drop')
  
  # mutate data to have to correct structure for insert children
  dataToBeMigratedForChild <- mag %>%
    filter(id_lab_mag == id_lab) %>%
    left_join(otherRooms, by = c("line" = "line_other_room", "id_lab" = "id_lab_mag")) %>%
    mutate(
      id_lab_has_hazards = id_mag + 1000,
      id_hazard_form_child_history = formChildHistory$id_hazard_form_child_history[1],
      other_rooms = ifelse(is.na(other_rooms), '', other_rooms)
    ) %>%
    mutate(
      submission = ifelse(is.na(line_place), '', paste0('{"data":{"line":"',line,'","position":"',ifelse(line_place=='f', 'on the floor', ifelse(line_place=='n', 'not drawn on the floor', 'on the magnet')),'","otherImpactedRooms":[',other_rooms,'],"status":"Default","delete":false,"comment":"',ifelse(is.null(comment), '', ifelse(is.na(comment), '', gsub("\r?\n", "\\\\n", comment))),'"}}'))
    ) %>%
    filter(submission != '') %>%
    select('id_lab_has_hazards', 'id_hazard_form_child_history', 'submission')

  dbAppendTable(con, 'lab_has_hazards_child', dataToBeMigratedForChild)
  # ---------------------------------------------------
  return(dataToBeMigrated)
}
