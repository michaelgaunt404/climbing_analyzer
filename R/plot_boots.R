plot_boots = function(data){
  temp_plot = data %>%
    mutate(label_hand = str_glue("Average: {mean}")
           ,label_hdi = str_glue("HDI Band\nLow: {hdi.low}\nHigh: {hdi.high}")) %>%
    group_by(grip_type) %>%
    group_map(~{
      plot_ly(.x) %>%
        add_segments(
          x = ~hdi.low, xend = ~hdi.high,
          y = ~hand, yend = ~hand, color = ~hand
          ,showlegend = (.y == "halfcrimp")
          ,legendgroup = ~hand
          ,text = ~label_hdi, hoverinfo = "text"
        ) %>%
        add_markers(x = ~mean, y = ~hand, showlegend = F, size = 23
                    ,text = ~label_hand, hoverinfo = "text") %>%
        layout(yaxis = list(
          titlefont = list(size = 11)
          ,title = paste0(
            c(rep("&nbsp;", 20),
              paste("<b>", as.character(.y), "</b>"),
              rep("&nbsp;", 20)),
            collapse = "")
        ))
    }) %>%
    subplot(
      .,
      nrows = length(.), margin = .05, shareX = T, shareY = T, titleY = T) %>%
    layout(
      xaxis = list(title = "Output Force (Lbf)")
    )

  return(temp_plot)

}
