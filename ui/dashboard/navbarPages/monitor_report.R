tabItem(
      tabName = "monitorReport",
  
      fluidRow(
              div(
                  # style = "padding: 50px",
                  # h1("Monitor Report goes hered!"),
                  # br(),
                  # br(),
                  h4("Monitor Report"),
                  # box(
                  #   selectInput("month_selct", label="Month",
                  #                 choice=list(monthChoices) )
                  #                 ),
                  
                  uiOutput(outputId = "monthDataCollection"),
                  uiOutput(outputId = "quarterDataCollection"),
                  uiOutput(outputId = "dateRangeDataCollection"),
                  
                  
                  tabBox(
                    id = "tabsReport",
                    # title = "Monitor Report Statistics",
                    # title = "A card with tabs",
                    selected = "Enumerator",
                    status = "primary",
                    solidHeader = FALSE,
                    type = "tabs",
                    # tabPanel("enumstat1",DT::dataTableOutput('enumStatsReport')),
                    # tabPanel("teamstat", DT::dataTableOutput('teamStatsReport')),
                    # tabPanel("Bar", "This is the bar tab"),
                    
                    
                    # selected = "enumstat1respondent",
                    # status = "primary",
                    # solidHeader = FALSE,
                    # type = "tabs",
                    # tabPanel("enumstat1respondent",DT::dataTableOutput('enumStatsReport')),
                    tabPanel("Enumerator",DT::dataTableOutput('enumStatsReport')),
                    tabPanel("Team", DT::dataTableOutput('teamStatsReport')),
                    tabPanel("Border", DT::dataTableOutput('borderStatsReport')),
                    tabPanel("District", DT::dataTableOutput('districtStatsReport')),
                    tabPanel("Region", DT::dataTableOutput('regionalStatsReport'))

                  ),
                
                
                # DT::dataTableOutput('enumStatsReport'),
              )
    
            # column(
            #       12,
            #        
            #     ),
            # column(
            #       12,
            #         div(
            #           style = "padding: 50px",
            #           h1("Monitor Report goes hered!"),
            #           br(),
            #           br(),
            #           h4("enum is a preview"),
            #           
            #           # DT::dataTableOutput('enumStatsReport'),
            #         )
            #     ),
            # column(
            #       12,
            #         div(
            #           style = "padding: 50px",
            #           h1("Monitor Report goes hered!"),
            #           br(),
            #           br(),
            #           h4("team is a preview"),
            #           
            #           # DT::dataTableOutput('teamStatsReport'),
            #         )
            #     ),
            )
        )
    
    
    