# setwd("C:/2024ICBT/")
source("server/datapackages.R",  local = TRUE)


# to use new packages
# library(modelData)
# library(DataExplorer)

####  Plot Colour
# plot_colour <- "#8965CD"

# # Get the Data
# data_path <- "https://data.melbourne.vic.gov.au/api/explore/v2.1/catalog/datasets/bird-survey-results-for-areas-in-the-city-of-melbourne-february-and-march-2018/exports/csv?lang=en&timezone=Australia%2FSydney&use_labels=true&delimiter=%2C"
# 
# data <- read_csv(data_path) %>%
#   # select and rename relevant columns
#   select(
#     sighting_date = `Sighting Date`,
#     common_name = `Common Name`,
#     scientific_name = `Scientific Name`,
#     sighting_count = `Sighting Count`,
#     lat,
#     lon,
#     location_desc = loc1_desc,
#     site_name
#   )

# # Info box values ---------------------------------------------------------
# num_sightings <- sum(data$sighting_count)
# unique_birds <- length(unique(data$common_name))
# unique_locations <- length(unique(data$site_name))
# avg_daily_sightings <- data %>%
#   group_by(sighting_date) %>%
#   summarise(sighting_count = sum(sighting_count)) %>%
#   pull(sighting_count) %>%
#   mean()

### end of get the data
##################################

### UI DASHBOARD #####
###################################
# Define UI for application that draws a histogram

#auth-pages

# ui <- 
# fluidPage(
#   shiny::singleton(
#     shiny::tags$head(
#       tags$link(rel = "stylesheet", href = "styles.css"),
#       tags$link(rel = "stylesheet", href = "snackbar.css"),
#       tags$script(src="snackbar.js"),
#       tags$script(src="https://www.gstatic.com/firebasejs/5.7.0/firebase-app.js"),
#       tags$script(src="https://www.gstatic.com/firebasejs/5.7.0/firebase-auth.js"),
#       shiny::tags$script(src="auth.js")
#     )
#   ),
#   
#   # load shinyjs on
#   shinyjs::useShinyjs(),
#   
#   source("ui/authpages/sign-in.R", local = TRUE)$value,
#   source("ui/authpages/register.R", local = TRUE)$value,
#   source("ui/authpages/verify-email.R", local = TRUE)$value,
#   
#   source("ui/main.R", local = TRUE)$value,
# )
ui <- fluidPage(
  source("ui/dashboard/dashboardhome.R", local = TRUE)$value,
)



### END OF UI DASHBOARD #####
###################################


### Server Functions #####
###################################


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # Include the logic (server) for each tab
  source(file.path("server/auth/serverloginfunction.R"),  local = TRUE)$value
  source(file.path("server", "plotUniqueBirds.R"),  local = TRUE)$value
  source(file.path("server", "distributionPlotBirds.R"),  local = TRUE)$value
  # source(file.path("server", "tab2.R"),  local = TRUE)$value
  
  #get data from server  #schedule for every 2hrs refresh
  icbt_dataset <- reactive({
    invalidateLater(7200000) # scheduled for every 2 hours
    source(file.path("server/hqdata/01_hq_data_download.R"),  local = TRUE)$value
    source(file.path("server/hqdata/02_merge_for_final_file.R"),  local = TRUE)$value
    source(file.path("server/hqdata/03_errorchecks.R"),  local = TRUE)$value
    list(icbt_dataset_final=icbt_data,
         icbt_error_dataset=errorChecks
         )
    # icbt_data
    # errorChecks
  })
  
  #create a sample dataframe
  dataInput <- reactive({
    data.frame(rating=1:4,
               animal=c('koala', 'hedgehog', 'sloth', 'panda'),
               country=c('Australia', 'Italy', 'Peru', 'China'),
               avg_sleep_hours=c(21, 18, 17, 10),
               stringsAsFactors=FALSE)
  })
  
  output$testData <-renderTable({
    dataInput()
  })  
    
  output$icbtFinalDaset <- DT::renderDataTable({
   DT::datatable(icbt_dataset()[['icbt_dataset_final']] %>%
                   select(-regionCode,-interview_id,-enumerator_contact,
                          -gps_Latitude, -gps_Longitude, -gps_Accuracy, -gps_Altitude
                          )
                 ) 
  })
  
  output$icbtFinalErrors <- DT::renderDataTable({
   DT::datatable(icbt_dataset()[['icbt_error_dataset']] %>% 
                   select(-regionCode,-districtCode,-townCity,-interview_id,-districtName,-commodityObervedDescription,-observedRespondentDescription,-enumerator_contact) %>%
                   dplyr::rename(region=RegionName,
                          border=borderPostName,
                          team=team_number,
                          # enumPhone=enumerator_contact,
                          Pno=transpondent_id,
                          Cno=Commodity_id,
                          # transpondentDescription=observedRespondentDescription
                 ) 
           ) 
  })
  
  serverDataDownload<- reactive({
    switch( input$dataset,
            "icbt data" = icbt_dataset()[['icbt_dataset_final']],
            "icbt errors" = icbt_dataset()[['icbt_error_dataset']]
    )
  })
  
  # downloadFileFormatType <- ({
  #   switch(
  #     input$type,
  #     "Excel (CSV)"="csv",
  #     "Stata"="dta",
  #     "R"="rds"
  #   )
  # })
  
  output$downloadData <- downloadHandler(
          filename = function(){
                      paste(input$dataset, ".", "downloadFileFormatType",sep="")
                    },

                  content = function(file){
                    # if(paste(input$type)=="Excel (CSV)"){
                    #   write.csv(serverDataDownload(), file, row.names = FALSE)
                    #   
                    # }else if(paste(input$type)=="Stata"){
                    #   write.dta(serverDataDownload(),file)
                    #   
                    # }else if(paste(input$type)=="R"){
                    #  saveRDS(serverDataDownload(),file)
                    # }
                    # 
                    # # sep <- switch (input$type,
                    # #                "Excel (CSV)" = ",",
                    # #                "Stata" = "",
                    # #                "R" = ""
                    # # )
              
                    # write.table(serverDataDownload(),file, sep = sep)
                    saveRDS(serverDataDownload(),file)
                  }
  )
  
  # output$selectedDataset<- renderTable( {
  #     head(serverDataDownload())
  #   } )
  

}
### End of Server Functions #####
###################################

# Run the application 
shinyApp(ui = ui, server = server)
