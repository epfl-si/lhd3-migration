insertElectrical <- function(con, formHistory) {
  # --------------------------------------------------- Lab_Has_Hazard
  # get all hazards to migrate
  result <- tbl(con, "elec") %>%
    collect()
  
  # mutate data to have to correct structure for insert
  dataToBeMigrated <- result %>%
    mutate(
      id_hazard_form_history = formHistory$id_hazard_form_history[1],
      capacitor_boolean = capacitor == 'y',
      currentType = ifelse(!is.na(v_ac), 'AC', ifelse(!is.na(v_dc), 'DC', '')),
      voltage = ifelse(!is.na(v_ac), v_ac, ifelse(!is.na(v_dc), v_dc, '')),
      stateNew = ifelse(state == 0, 'Electrod has not been modified', ifelse(state == 1, 'Electrod has been modified', '')),
      accessNew = ifelse(access == 0, 'Electrod is not accessible, encapsulated, isolated', 
                         ifelse(access == 1, 'Electrod is behind a fence or barrier', 
                                ifelse(access == 2, 'Electrod is accessible and can be touched', '')))
    ) %>%
    mutate(
      submission = paste0('{"data":{"capacitor":',ifelse(is.na(capacitor_boolean),'""',capacitor_boolean),',"currentType":"',currentType,'","state":"',ifelse(is.na(stateNew),'',stateNew),'","access":"',ifelse(is.na(accessNew),'',accessNew),'","comment":"","status":"Default","delete":false,"qBatteryMAh":"',ifelse(is.na(i_battery), '', i_battery),'","voltageV":"',voltage,'","current":"',ifelse(is.na(i), '', i),'"}}'),
    ) %>%
    select('id_lab', 'id_hazard_form_history', 'submission') %>%
    filter(submission != '{"data":{"capacitor":"","currentType":"","state":"","access":"","comment":"","status":"Default","delete":false,"qBatteryMAh":"","voltageV":"","current":""}}')
  
  dbAppendTable(con, 'lab_has_hazards', dataToBeMigrated)
  return(dataToBeMigrated)
  # ---------------------------------------------------
}