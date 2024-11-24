tabItem(
      tabName = "monitorReport",

      column(width = 6,
             fluidRow(
               div(
                 # style = "padding: 50px",
                 # h1("Monitor Report goes hered!"),
                 # br(),
                 # br(),
                 fluidRow(
                   column(
                     width = 6,
                     fluidRow(
                       style = "bootstrap",
                       align = 'left',
                       column(  h3("MONITOR REPORT"),width = 2),
                       column(width = 2,
                              uiOutput(outputId = "quarterDataCollection")
                       ),
                       column(width = 2,
                              uiOutput(outputId = "monthDataCollection")
                       ),
                       column(width = 3,
                              uiOutput(outputId = "dateRangeDataCollection")
                       )
                     )
                   ),
                 ),
                 
                 column(
                   width = 6,
                   fluidRow(
                     style = "bootstrap",
                     tabBox(
                       width = 8,
                       id = "tabsReport",
                       
                       selected = "Enumerator",
                       status = "primary",
                       solidHeader = FALSE,
                       type = "tabs",
                       
                       tabPanel("Enumerator",DT::dataTableOutput('enumStatsReport')),
                       tabPanel("Team", DT::dataTableOutput('teamStatsReport')),
                       tabPanel("Border", DT::dataTableOutput('borderStatsReport')),
                       tabPanel("District", DT::dataTableOutput('districtStatsReport')),
                       tabPanel("Region", DT::dataTableOutput('regionalStatsReport'))
                       
                     )
                   )
                 )
                 
                 
               )
               
             ))
        )
    
    
    