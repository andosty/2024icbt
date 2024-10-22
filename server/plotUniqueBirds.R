
  output$plot_unique_birds_site <- renderPlotly({
    data %>%
      group_by(site_name) %>%
      summarise(unique_birds = n_distinct(common_name)) %>%
      arrange(unique_birds) %>%
      mutate(site_name = if_else(site_name == "Dynon Road Tidal Canal Wildlife Sanctuary", "Dynon Road Tidal Canal<br>Wildlife Sanctuary", site_name)) %>%
      mutate(site_name = factor(site_name, levels = .$site_name)) %>%
      plot_ly(
        x = ~unique_birds,
        y = ~site_name,
        type = "bar",
        marker = list(color = plot_colour),
        orientation = "h"
      ) %>%
      layout(
        xaxis = list(title = "Unique Birds"),
        yaxis = list(title = "")
      ) %>%
      config(displayModeBar = FALSE)
  })
