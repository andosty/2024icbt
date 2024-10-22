tabItem(
  tabName = "errorData",
  fluidRow(
    column(
          width = 12,
          align = 'left',
          
          div(
            tags$head(tags$style("#icbtFinalErrors {white-space: nowrap;}")),
            style = "padding: 20px",
            h4("View Content Errors in ICBT DATA"),
            
            DT::dataTableOutput('icbtFinalErrors')
            ),
          )
    )
)
