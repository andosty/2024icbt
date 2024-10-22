# 
# custom_theme <- bs_theme(
#   version = 5,
#   bg = "#FFFFFF",
#   fg = "#000000",
#   primary = "#0199F8",
#   secondary = "#FF374B",
#   base_font = "Maven Pro"
# )

# bs_theme_preview(theme = custom_theme, with_themer = FALSE)

hidden(fluidRow(
  id = "main",
  column(
    12,
    tags$button(
      id = "submit_sign_out",
      type = "button",
      "Sign Out",
      class = "btn-danger pull-right",
      style = "color: white;"
    )
  ),
  column(
    12,
    div(
      
      ################ start
      
      source("ui/dashboard/dashboardhome.R", local = TRUE)$value,
      
      ################ end
      
    )
  )
))

