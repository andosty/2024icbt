 tabItem(
  tabName = "manualRejectionMenu",

  
  fluidPage(
    div(
      h4("MANUAL INTERVIEWER CASE REJECTION"),
      fluidRow(width = 6,
               style = "bootstrap",
               column(width = 2,
                      uiOutput(outputId = "rejectionRegion")
                      ),
               column(width = 2,
                      uiOutput(outputId = "rejectionTeamNumber")
                      ),
               column(width = 3,
                      uiOutput(outputId = "rejectionEnumeratorName")
                      ),
               column(width = 2,
                      uiOutput(outputId = "rejectionEnumeratorCase")
                      ),
               column(width = 1,
                      uiOutput(outputId = "tabletHistoryButton")
                      ),
               column(width = 1,
                      uiOutput(outputId = "clickManualRejection")
                      )
             ),
      
      br(),
      h4("TABLET SYNC HISTORY TABLE"),
      DT::dataTableOutput('tabletSyncStat'),
      
      
      #get a table of last 10 sync activity, for today and previous day
      # br(),
      # helpText("Click to Reject & Hence, Return Error CASES back to selected enumerators' tablet"),
      # br(),
      # actionButton("doManualReject", "Reject Case",type="button", class="btn btn-default action-button"),
      # actionButton("doManualReject", "Reject Case", style = "color: white; background-color: blue;"),
      
      # downloadButton('RejectAction','doReject'),
      # actionButton("sendRejectedCases", "Reject Cases", class = "btn-primary btn-lg")
      # actionButton("doReject", "Reject Cases", class = "btn-primary btn-lg")
      
      # tableOutput(outputId = 'Reject Caes'),
      
    )
  )
  # fluidPage(
  #   div(
  #     h3("Data Download - Further Report Generation and Analysis"),
  #     selectInput("dataset", "select the dataset", choices=c("icbt data","icbt errors")),
  #     br(),
  #     helpText("select the download format"),
  #     radioButtons("type","format type:",
  #                  choices = c("Excel (CSV)","Stata","R")
  #                   ),
  #      br(),
  #     helpText("Click on the download button to download the dataset observations assigned to you"),
  #     downloadButton('downloadData','Download'),
  #     
  #     # tableOutput(outputId = 'selectedDataset'),
  #     
  #   )
  # )
  
  

)

