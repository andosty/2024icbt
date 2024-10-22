tabItem(
  tabName = "actionMenu",
  # jumbotron(
  #   title = "DQM Data Response Action",  #heading
  #   status = "info",
  #   lead = "Return Error Cases back to responsible enumerators",  #sub heading
  #   btnName = "Reject Cases with Errors",
  #   href = "doReject",
  #   downloadButton('RejectAction','doReject'),
  #   
  #   "Click to return CASES with errors back to the responsible enumerators' tablet. They would receive them when they sync to HQ again, so as to effect needed corrections"
  # ),
  
  fluidPage(
    div(
      h3("DQM Data Response Action"),
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
      # radioButtons("type","format type:",
      #              choices = c("Excel (CSV)","Stata","R")
      #              ),
      # br(),
      helpText("Click on the download button to download the dataset observations assigned to you"),
      downloadButton('downloadData','Download'),
      
      # tableOutput(outputId = 'selectedDataset'),
      
    )
  )
  
  

)

