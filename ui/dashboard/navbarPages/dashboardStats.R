tabItem(
  tabName = "dashboardSummary",

      h4("Statitics"),
      div(
          fluidRow( 
            tabBox(
              width=12,
              id = "StaticsReport",
              selected = "summary",
              status = "primary",
              solidHeader = FALSE,
              type = "tabs",

              tabPanel("summary",
                column(
                  12,
                  div(
                    style = "padding: 10px",
                    # h4("You have now signed in and your email address has been verified!"),
                    # br(),
                    #total summaries
                   fluidRow(
                     column(
                       width = 4,
                       infoBox(
                         width = 12,
                         title = 'Total Traded Product',
                         value= textOutput(outputId = "totalCommoditiesTraded"), #compute them in server function  
                         
                         icon = ionicon(name ="basket"),
                         color = "success"
                       )
                     ),
                     
                     column(
                       width = 4,
                       infoBox(
                         width = 12,
                         title = 'Total Transpondent',
                         value= textOutput(outputId = "totalTranspondent"), #compute them in server function  
                         icon = ionicon(name ="person"),
                         color = "warning"
                       )
                     )
                   ),
                   #trade flow summaries
                   fluidRow(
                     
                     column(
                       width = 4,
                       infoBox(
                         width = 12,
                         title = 'Trade Outflow (Export)',
                         value= textOutput(outputId = "totalCommoditiesOutflow"), #compute them in server function  
                         # icon = icon("list"),
                         # icon = ionicon(name ="enter-outline"),
                         icon = ionicon(name ="cloud-upload"),
                         color = "purple"
                       )
                     ),
                     
                     column(
                       width = 4,
                       infoBox(
                         width = 12,
                         title = 'Trade Inflow (Import)',
                         value= textOutput(outputId = "totalCommoditiesInflow"), #compute them in server function  
                         icon = ionicon(name ="cloud-download"),
                         # icon = ionicon(name ="download"),
                         # icon = icon("list"),
                         color = "teal"
                       )
                     ),
                 
                     
                     column(
                       width = 4,
                       infoBox(
                         width = 12,
                         title = 'Net Trade',
                         value= textOutput(outputId = "totalCommoditiesNetflow"), #compute them in server function  
                         # icon = icon("list"),
                         icon = ionicon(name ="cloud"),
                         color = "navy"
                       )
                     )
                   )
                    
                   
                    # # h3("This is all the information that comes with the signed in user with 'Email/Password' authentication on Firebase"),
                    # h3("total obs"),
                    # textOutput(outputId = "totalData_Obs"),
                    # h3("total error obs"),
                    # textOutput(outputId = "totalError_Obs"),
                    #
                    # br(),
                    # DT::DTOutput("user_access"),
                    #
                    # DT::DTOutput("user_out")
                  )
                ),
              ),
              
              
              tabPanel("Trade Partners",div( h4('yet to be tabulated'))),
              tabPanel("Trading Commodities", div( h4('yet to be tabulated'))),
              tabPanel("Trade by Sex", div( h4('yet to be tabulated'))),
              tabPanel("Trade by Region", div( h4('yet to be tabulated')))
              
            ),
            ),
      )
)
 