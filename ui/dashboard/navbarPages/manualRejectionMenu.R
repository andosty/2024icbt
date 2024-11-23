 tabItem(
  tabName = "manualRejectionMenu",

  
  fluidPage(
    div(
      h3("Manual Rejection of Cases with Errors to Interviewers"),
      uiOutput(outputId = "rejectionRegion"),
      uiOutput(outputId = "rejectionTeamNumber"),
      
      
      actionButton("doReject", "Reject Cases", class = "btn-warning btn"),
      helpText("Click to return CASES with errors back to the responsible enumerators' tablet"),
      br(),
      br(),
      # downloadButton('RejectAction','doReject'),
      # actionButton("sendRejectedCases", "Reject Cases", class = "btn-primary btn-lg")
      # actionButton("doReject", "Reject Cases", class = "btn-primary btn-lg")
      
      # tableOutput(outputId = 'Reject Caes'),
      
    )
  ),
  fluidPage(
    div(
      h3("Data Download - Further Report Generation and Analysis"),
      selectInput("dataset", "select the dataset", choices=c("icbt data","icbt errors")),
      br(),
      helpText("select the download format"),
      radioButtons("type","format type:",
                   choices = c("Excel (CSV)","Stata","R")
                    ),
       br(),
      helpText("Click on the download button to download the dataset observations assigned to you"),
      downloadButton('downloadData','Download'),
      
      # tableOutput(outputId = 'selectedDataset'),
      
    )
  )
  
  

)

