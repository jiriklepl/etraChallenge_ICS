read_data <- function(pth) {
  
  df_tmp <- read_csv(file.path(local_data_pth, pth))
  
  # set participant_id, trial id and stimulus id
  parsed_name <- pth %>% str_split("_") %>% unlist()
  df_tmp <- df_tmp %>% 
    mutate(participant_id = parsed_name[1],
           trial_id = parsed_name[2],
           fv_fixation = parsed_name[3],
           task_type = parsed_name[4],
           stimulus_id = parsed_name[5])
  df_tmp %>% select(participant_id, trial_id, fv_fixation, task_type, stimulus_id, everything())
}