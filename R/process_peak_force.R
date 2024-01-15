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
      ,across(c(value, rep_total), gauntlet::dgt0)
    ) %>%
    arrange(date) %>%
    group_by(grip_type, hand, date_day) %>%
    mutate(index = row_number()) %>%
    ungroup() %>%
    group_by(grip_type, hand) %>%
    mutate(rep_number = row_number()) %>%
    ungroup() %>%
    ungroup() %>%
    group_by(date_day) %>%
    mutate(session_number = row_number()) %>%
    ungroup() %>%
    select(grip_type, test_train, hand, value, rep_total, date_day, index, rep_number, session_number)

  return(temp_data)

}

