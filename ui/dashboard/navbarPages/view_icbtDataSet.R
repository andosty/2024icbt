tabItem(
  tabName = "icbtData",
  fluidRow(
    column(
          width = 12,
          align = 'left',
          
          div(
              tags$head(tags$style("#icbtFinalDaset {white-space: nowrap;}")),
            style = "padding: 20px",
            h4("View ICBT Data set"),
            
            DT::dataTableOutput('icbtFinalDaset')
            ),
          )
    )
)