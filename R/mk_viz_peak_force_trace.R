
mk_viz_peak_force_trace = function(data){

  data_pro_sd = crosstalk::SharedData$new(data)

  temp_item = crosstalk::bscols(
    widths = c(3, 12)
    ,crosstalk::filter_select(
      id = "peak_force", label = "Grip Type", data_pro_sd, ~grip_type, multiple = T)
    ,data_pro_sd %>%
      plot_ly(
        x = ~rep_number, y = ~value, color = ~hand
        ,split = ~grip_type
        ,mode = 'lines+markers') %>%
      layout(
        xaxis = list(title = "Max Force (lbf)")
        ,yaxis = list(title = "Rep Number")
      )
  )

  return(temp_item)
}


