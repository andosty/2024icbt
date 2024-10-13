tabItem(
  tabName = "actionMenu",
  jumbotron(
    title = "Welcome",  #heading
    status = "info",
    lead = "Visual representation of survey results",  #sub heading
    btnName = "download dataset",
    href = "https://data.melbourne.vic.gov.au/api/explore/v2.1/catalog/datasets/bird-survey-results-for-areas-in-the-city-of-melbourne-february-and-march-2018/exports/csv?lang=en&timezone=Australia%2FSydney&use_labels=true&delimiter=%2C",
    "Data avalibale from city f Melbourn"
  ),
  
  fluidPage(
    div(
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
