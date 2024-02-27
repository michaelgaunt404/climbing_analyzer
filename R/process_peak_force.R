process_peak_force = function(data){

  temp_data = data %>%
    mutate(rep_total = left.max.weight + right.max.weight) %>%
    pivot_longer(cols = c("left.max.weight", "right.max.weight")
                 ,names_to = "hand") %>%
    mutate(
      date = ydm_hms(date)
      ,grip_type = gsub(".*_", "\\1", tag)
      ,test_train = gsub("_.*", "\\1", tag)
      ,hand = gsub("\\..*", "\\1", hand)
      ,date_day = lubridate::floor_date(as_date(date), "day")
      ,date_week = lubridate::floor_date(as_date(date), "week")
      ,across(c(value, rep_total), ~round(., 0))
    ) %>%
    arrange(date) %>%
    group_by(grip_type, hand, date_day) %>%
    mutate(index = row_number()) %>%
    ungroup() %>%
    group_by(grip_type, hand) %>%
    mutate(rep_number = row_number()
           ,mean_grip_type_hand = mean(value)
           ,mean_grip_type_hand_pdiff = round(100*(value-mean_grip_type_hand)/mean_grip_type_hand, 0)
           ,max_grip_type_hand_pdiff = round(100*(value-max(value))/max(value), 0)
           ,value_centered = (scale(value)[,1]) %>%
             round(2)) %>%
    ungroup() %>%
    group_by(date_day) %>%
    mutate(session_number = row_number()) %>%
    ungroup() %>%
    group_by(grip_type) %>%
    mutate(session_type = case_when(
      date_day == max(date_day)~"Most Recent", T~"Past"
    )) %>%
    ungroup() %>%
    mutate(label = str_glue("{grip_type} - {hand}\nForce: {value}Lbf ({value_centered} sd)\n% Off GMean: {mean_grip_type_hand_pdiff}%\n% Off GMax: {max_grip_type_hand_pdiff}%"))  %>%
    select(grip_type, test_train, hand, value, value_centered, mean_grip_type_hand, mean_grip_type_hand_pdiff, max_grip_type_hand_pdiff, rep_total, date_day, index, rep_number, session_number, session_type, label, comment)

  return(temp_data)

}

