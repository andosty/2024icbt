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

ui <-
fluidPage(
  shiny::singleton(
    shiny::tags$head(
      tags$link(rel = "stylesheet", href = "styles.css"),
      tags$link(rel = "stylesheet", href = "snackbar.css"),
      tags$script(src="snackbar.js"),
      tags$script(src="https://www.gstatic.com/firebasejs/5.7.0/firebase-app.js"),
      tags$script(src="https://www.gstatic.com/firebasejs/5.7.0/firebase-auth.js"),
      shiny::tags$script(src="auth.js")
    )
  ),

  # load shinyjs on
  shinyjs::useShinyjs(),

  source("ui/authpages/sign-in.R", local = TRUE)$value,
  source("ui/authpages/register.R", local = TRUE)$value,
  source("ui/authpages/verify-email.R", local = TRUE)$value,

  source("ui/main.R", local = TRUE)$value,
)


# ui <- fluidPage(
#   source("ui/dashboard/dashboardhome.R", local = TRUE)$value,
# )



### END OF UI DASHBOARD #####
###################################


### Server Functions #####
###################################


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # Include the logic (server) for each tab
  source(file.path("server/auth/serverloginfunction.R"),  local = TRUE)$value
  # source(file.path("server", "plotUniqueBirds.R"),  local = TRUE)$value
  # source(file.path("server", "distributionPlotBirds.R"),  local = TRUE)$value
  # source(file.path("server", "tab2.R"),  local = TRUE)$value
  
  #getAuthCredDataAccess
 
  
  #get data from server  #schedule for every 2hrs refresh
  icbt_dataset <- reactive({
    invalidateLater(7200000) # scheduled for every 2 hours
    source(file.path("server/hqdata/01_A_download_merge.R"),  local = TRUE)$value
    # source(file.path("server/hqdata/02_merge_for_final_file.R"),  local = TRUE)$value
    source(file.path("server/hqdata/03_errorchecks.R"),  local = TRUE)$value
    list(icbt_dataset_final=icbt_data ,
         icbt_error_dataset=errorChecks
         )
    # icbt_data
    # errorChecks
  })
  

    
  output$icbtFinalDaset <- DT::renderDataTable({
   DT::datatable(icbt_dataset()[['icbt_dataset_final']] %>%
                   dplyr::rename(
                                team=team_number,
                                border=borderPostName,
                               region=RegionName
                                 ) %>%
                   select(.,-c("regionCode","interview_id","enumerator_contact",
                               "gps_Latitude","gps_Longitude","gps_Accuracy","gps_Altitude",
                               "responsibleId","UserName","assignment_id") #"s2q6ao"
                          ) %>%
                   mutate(
                     gps_Timestamp=  format(as.POSIXct(sub('T',' ',gps_Timestamp )), "%Y-%m-%d %I:%M %p"),
                     createdDate=  format(as.POSIXct(sub('T',' ',createdDate )), "%Y-%m-%d %I:%M %p")
                     # createdDate =format(as.Date.character(createdDate), "%Y-%m-%d %I:%M %p"), 
                     # gps_Timestamp =format(as.Date.character(gps_Timestamp), "%Y-%m-%d %I:%M %p"),
                   ) %>% as.data.frame() %>% ungroup() # %>% select(.,'interview_key')
                  
                 ,
                 filter = "top",
                 extensions = 'FixedColumns',
                 rownames = FALSE,
                 
                options = list(
                                fixedColumns = list(leftColumns = 4),
                                scrollX=TRUE,
                               lengthMenu = c(5,8,10,15,100)
                               )
               )
    
  })
  
  output$icbtFinalErrors <- DT::renderDataTable(server = FALSE,{
   DT::datatable(icbt_dataset()[['icbt_error_dataset']] %>% 
                   select(-regionCode,-districtCode,-townCity,-interview_id,-districtName,-commodityObervedDescription,-observedRespondentDescription,-enumerator_contact,-responsibleId) %>%  #-responsibleId,
                   dplyr::rename(
                     region=RegionName,
                     border=borderPostName,
                     team=team_number,
                     # enumPhone=enumerator_contact,
                          Pno=transpondent_id,
                          Cno=Commodity_id,
                          # transpondentDescription=observedRespondentDescription
                 ) %>% 
                   mutate(
                     # createdDate =format(as.Date.character(createdDate), "%Y-%m-%d %I:%M %p"), 
                     # gps_Timestamp =format(as.Date.character(gps_Timestamp), "%Y-%m-%d %I:%M %p"),
                     gps_Timestamp=  format(as.POSIXct(sub('T',' ',gps_Timestamp )), "%Y-%m-%d %I:%M %p"),
                     createdDate=  format(as.POSIXct(sub('T',' ',createdDate )), "%Y-%m-%d %I:%M %p")
                     
                   )
          ,
          filter = "top",
          extensions = c('FixedColumns','Buttons'),
          rownames = FALSE,
          # extensions = 'Buttons', 
          options = list(scrollX=TRUE, 
                         lengthMenu = c(5,10,15,100),
                         paging = TRUE, 
                         searching = TRUE,
                         fixedColumns = TRUE, 
                         autoWidth = TRUE,
                         ordering = TRUE, 
                         dom = 'Blfrtip', 
                         autoWidth = TRUE,
                         scrollX = TRUE,
                         
                           fixedColumns = list(leftColumns = 4),
                           style = "bootstrap",
                           selection = "single",
    
                         buttons = list(
                           list(
                             extend = 'collection',
                             buttons = list(
                               list(extend = "csv", filename = "page",exportOptions = list(
                                 columns = ":visible",modifier = list(page = "current"))
                               ),
                               list(extend = 'excel', filename = "page", title = NULL, 
                                    exportOptions = list(columns = ":visible",modifier = list(page = "current")))),
                             text = 'Download current page'),
                           
                           # code for the  second dropdown download button
                           # this will download the entire dataset using modifier = list(page = "all")
                           list(
                             extend = 'collection',
                             buttons = list(
                               list(extend = "csv", filename = "data",exportOptions = list(
                                 columns = ":visible",modifier = list(page = "all"))
                               ),
                               list(extend = 'excel', filename = "data", title = NULL, 
                                    exportOptions = list(columns = ":visible",modifier = list(page = "all")))),
                             text = 'Download all data')
                           
                         )
                         
                         
                         )
                         )
           
  })
  
  ## MONITOR REPORT CALCs ####
  ######################
  
  #--- Enum Stats report
  #------------------------------------------
  enumMonitorReport <- reactive({
   #enum monitor report
    source(file.path("server/report/enumStat.R"),  local = TRUE)$value
  }) 
  
  #--- Team Stats report
  teamMonitorReport <- reactive({
    
    
    teamStatREPORT <-enumMonitorReport() %>% 
      group_by(regionCode, region, districtCode, district, townCity, 
               border, team
      ) %>%
      summarise(
        `total Respondents` = sum(`total Respondents`),
        `total IN_MIGRATION(X)` = sum(`total IN_MIGRATION(X)`),
        `total OUT_MIGRATION(M)` =sum(`total OUT_MIGRATION(M)`),
        `Net Migration(X-M)` = sum(`Net Migration(X-M)`),
        `total Male Respondents` = sum(`total Male Respondents`),
        `total Female Respondents` = sum(`total Female Respondents`),
        `total OtherSpecified Transport Means` = sum(`total OtherSpecified Transport Means`) , 
        
        
        `total Traded Commodities` = sum(`total Traded Commodities`),
        `total Goods IN(Imports)` = sum(`total Goods IN(Imports)`),
        `total Goods OUT(Exports)` = sum(`total Goods OUT(Exports)`),
        `OtherSpecified Commodities` = sum(`OtherSpecified Commodities`),
        `OtherSpecified Unit of Measures` = sum( `OtherSpecified Unit of Measures`),
        `total Traded Livestocks` = sum(`total Traded Livestocks`),
        `total Missed Transpondents` = sum(`total Missed Transpondents`),
        
        `otherSpecified Commodity Percent` =  round((`OtherSpecified Commodities` /`total Traded Commodities`)*100,2),
        `otherSpecified unit of Measurement Percent` =  round((`OtherSpecified Unit of Measures`/`total Traded Commodities`)*100,2),
        
      ) %>% ungroup()
  }) 
  
  #--- border Stats report
  borderMonitorReport <- reactive({
    borderStatREPORT<- enumMonitorReport() %>% 
      group_by(regionCode, region, districtCode, district, townCity, 
               border
      ) %>%
      summarise(
        `total Respondents` = sum(`total Respondents`),
        `total IN_MIGRATION(X)` = sum(`total IN_MIGRATION(X)`),
        `total OUT_MIGRATION(M)` =sum(`total OUT_MIGRATION(M)`),
        `Net Migration(X-M)` = sum(`Net Migration(X-M)`),
        `total Male Respondents` = sum(`total Male Respondents`),
        `total Female Respondents` = sum(`total Female Respondents`),
        `total OtherSpecified Transport Means` = sum(`total OtherSpecified Transport Means`) , 
        
        
        `total Traded Commodities` = sum(`total Traded Commodities`),
        `total Goods IN(Imports)` = sum(`total Goods IN(Imports)`),
        `total Goods OUT(Exports)` = sum(`total Goods OUT(Exports)`),
        `OtherSpecified Commodities` = sum(`OtherSpecified Commodities`),
        `OtherSpecified Unit of Measures` = sum( `OtherSpecified Unit of Measures`),
        `total Traded Livestocks` = sum(`total Traded Livestocks`),
        `total Missed Transpondents` = sum(`total Missed Transpondents`),
        
        `otherSpecified Commodity Percent` =  round((`OtherSpecified Commodities` /`total Traded Commodities`)*100,2),
        `otherSpecified unit of Measurement Percent` =  round((`OtherSpecified Unit of Measures`/`total Traded Commodities`)*100,2),
        
      ) %>% ungroup()
     
  })
  
  districtMonitorReport <- reactive({
    districtStatReport <- enumMonitorReport() %>% 
      group_by(regionCode, region, districtCode, district
      ) %>%
      summarise(
        `total Respondents` = sum(`total Respondents`),
        `total IN_MIGRATION(X)` = sum(`total IN_MIGRATION(X)`),
        `total OUT_MIGRATION(M)` =sum(`total OUT_MIGRATION(M)`),
        `Net Migration(X-M)` = sum(`Net Migration(X-M)`),
        `total Male Respondents` = sum(`total Male Respondents`),
        `total Female Respondents` = sum(`total Female Respondents`),
        `total OtherSpecified Transport Means` = sum(`total OtherSpecified Transport Means`) , 
        
        
        `total Traded Commodities` = sum(`total Traded Commodities`),
        `total Goods IN(Imports)` = sum(`total Goods IN(Imports)`),
        `total Goods OUT(Exports)` = sum(`total Goods OUT(Exports)`),
        `OtherSpecified Commodities` = sum(`OtherSpecified Commodities`),
        `OtherSpecified Unit of Measures` = sum( `OtherSpecified Unit of Measures`),
        `total Traded Livestocks` = sum(`total Traded Livestocks`),
        `total Missed Transpondents` = sum(`total Missed Transpondents`),
        
        `otherSpecified Commodity Percent` =  round((`OtherSpecified Commodities` /`total Traded Commodities`)*100,2),
        `otherSpecified unit of Measurement Percent` =  round((`OtherSpecified Unit of Measures`/`total Traded Commodities`)*100,2),
        
      ) %>% ungroup() 
      
    
  })
  
  regionalMonitorReport <- reactive({
   regionStatReport <- enumMonitorReport() %>% 
      group_by(regionCode, region
      ) %>%
      summarise(
        `total Respondents` = sum(`total Respondents`),
        `total IN_MIGRATION(X)` = sum(`total IN_MIGRATION(X)`),
        `total OUT_MIGRATION(M)` =sum(`total OUT_MIGRATION(M)`),
        `Net Migration(X-M)` = sum(`Net Migration(X-M)`),
        `total Male Respondents` = sum(`total Male Respondents`),
        `total Female Respondents` = sum(`total Female Respondents`),
        `total OtherSpecified Transport Means` = sum(`total OtherSpecified Transport Means`) , 
        
        
        `total Traded Commodities` = sum(`total Traded Commodities`),
        `total Goods IN(Imports)` = sum(`total Goods IN(Imports)`),
        `total Goods OUT(Exports)` = sum(`total Goods OUT(Exports)`),
        `OtherSpecified Commodities` = sum(`OtherSpecified Commodities`),
        `OtherSpecified Unit of Measures` = sum( `OtherSpecified Unit of Measures`),
        `total Traded Livestocks` = sum(`total Traded Livestocks`),
        `total Missed Transpondents` = sum(`total Missed Transpondents`),
        
        `otherSpecified Commodity Percent` =  round((`OtherSpecified Commodities` /`total Traded Commodities`)*100,2),
        `otherSpecified unit of Measurement Percent` =  round((`OtherSpecified Unit of Measures`/`total Traded Commodities`)*100,2),
        
      ) %>% ungroup()
  })
  
  #     Datatable  
  #-----------------------------------------
  #---  Enum Stats datatable
  output$enumStatsReport <- DT::renderDataTable(server = FALSE,{
    DT::datatable(enumMonitorReport() %>%
                    select(-regionCode, -districtCode,-townCity)
                  ,
                  filter = "top",
                  extensions = c('FixedColumns','Buttons'),
                  rownames = FALSE,
                  # extensions = 'Buttons', 
                  options = list(scrollX=TRUE, 
                                 lengthMenu = c(5,10,15,100),
                                 paging = TRUE, 
                                 searching = TRUE,
                                 fixedColumns = TRUE, 
                                 autoWidth = TRUE,
                                 ordering = TRUE, 
                                 dom = 'Blfrtip', 
                                 autoWidth = TRUE,
                                 scrollX = TRUE,
                                 
                                 fixedColumns = list(leftColumns = 5),
                                 style = "bootstrap",
                                 selection = "single",
                                 
                                 buttons = list(
                                   list(
                                     extend = 'collection',
                                     buttons = list(
                                       list(extend = "csv", filename = "page",exportOptions = list(
                                         columns = ":visible",modifier = list(page = "current"))
                                       ),
                                       list(extend = 'excel', filename = "page", title = NULL, 
                                            exportOptions = list(columns = ":visible",modifier = list(page = "current")))),
                                     text = 'Download current page'),
                                   
                                   # code for the  second dropdown download button
                                   # this will download the entire dataset using modifier = list(page = "all")
                                   list(
                                     extend = 'collection',
                                     buttons = list(
                                       list(extend = "csv", filename = "data",exportOptions = list(
                                         columns = ":visible",modifier = list(page = "all"))
                                       ),
                                       list(extend = 'excel', filename = "data", title = NULL, 
                                            exportOptions = list(columns = ":visible",modifier = list(page = "all")))),
                                     text = 'Download all data')
                                   
                                 )
                                 
                                 
                  )
                  
                  )
     
  })
  
  #--- Team Stats datatable  
  output$teamStatsReport <- DT::renderDataTable(server = FALSE,{
    DT::datatable(teamMonitorReport()
                  %>%
                    select(-regionCode, -districtCode,-townCity)
                  ,
                  filter = "top",
                  extensions = c('FixedColumns','Buttons'),
                  rownames = FALSE,
                  # extensions = 'Buttons', 
                  options = list(scrollX=TRUE, 
                                 lengthMenu = c(5,10,15,100),
                                 paging = TRUE, 
                                 searching = TRUE,
                                 fixedColumns = TRUE, 
                                 autoWidth = TRUE,
                                 ordering = TRUE, 
                                 dom = 'Blfrtip', 
                                 autoWidth = TRUE,
                                 scrollX = TRUE,
                                 
                                 fixedColumns = list(leftColumns = 4),
                                 style = "bootstrap",
                                 selection = "single",
                                 
                                 buttons = list(
                                   list(
                                     extend = 'collection',
                                     buttons = list(
                                       list(extend = "csv", filename = "page",exportOptions = list(
                                         columns = ":visible",modifier = list(page = "current"))
                                       ),
                                       list(extend = 'excel', filename = "page", title = NULL, 
                                            exportOptions = list(columns = ":visible",modifier = list(page = "current")))),
                                     text = 'Download current page'),
                                   
                                   # code for the  second dropdown download button
                                   # this will download the entire dataset using modifier = list(page = "all")
                                   list(
                                     extend = 'collection',
                                     buttons = list(
                                       list(extend = "csv", filename = "data",exportOptions = list(
                                         columns = ":visible",modifier = list(page = "all"))
                                       ),
                                       list(extend = 'excel', filename = "data", title = NULL, 
                                            exportOptions = list(columns = ":visible",modifier = list(page = "all")))),
                                     text = 'Download all data')
                                   
                                 )
                  )

    ) 
  })
  
  #--- Border Stats datatable  
  output$borderStatsReport <- DT::renderDataTable(server = FALSE,{
    DT::datatable(borderMonitorReport() 
                  %>%
                    select(-regionCode, -districtCode,-townCity)
                  ,
                  filter = "top",
                  extensions = c('FixedColumns','Buttons'),
                  rownames = FALSE,
                  # extensions = 'Buttons', 
                  options = list(scrollX=TRUE, 
                                 lengthMenu = c(5,10,15,100),
                                 paging = TRUE, 
                                 searching = TRUE,
                                 fixedColumns = TRUE, 
                                 autoWidth = TRUE,
                                 ordering = TRUE, 
                                 dom = 'Blfrtip', 
                                 autoWidth = TRUE,
                                 scrollX = TRUE,
                                 
                                 fixedColumns = list(leftColumns = 4),
                                 style = "bootstrap",
                                 selection = "single",
                                 
                                 buttons = list(
                                   list(
                                     extend = 'collection',
                                     buttons = list(
                                       list(extend = "csv", filename = "page",exportOptions = list(
                                         columns = ":visible",modifier = list(page = "current"))
                                       ),
                                       list(extend = 'excel', filename = "page", title = NULL, 
                                            exportOptions = list(columns = ":visible",modifier = list(page = "current")))),
                                     text = 'Download current page'),
                                   
                                   # code for the  second dropdown download button
                                   # this will download the entire dataset using modifier = list(page = "all")
                                   list(
                                     extend = 'collection',
                                     buttons = list(
                                       list(extend = "csv", filename = "data",exportOptions = list(
                                         columns = ":visible",modifier = list(page = "all"))
                                       ),
                                       list(extend = 'excel', filename = "data", title = NULL, 
                                            exportOptions = list(columns = ":visible",modifier = list(page = "all")))),
                                     text = 'Download all data')
                                   
                                 )
                  )
    ) 
  })
  
  #--- District Stats datatable  
  output$districtStatsReport <- DT::renderDataTable(server = FALSE,{
    DT::datatable(districtMonitorReport() 
                  %>%
                    select(-regionCode, -districtCode)
                  ,
                  filter = "top",
                  extensions = c('FixedColumns','Buttons'),
                  rownames = FALSE,
                  # extensions = 'Buttons', 
                  options = list(scrollX=TRUE, 
                                 lengthMenu = c(5,10,15,100),
                                 paging = TRUE, 
                                 searching = TRUE,
                                 fixedColumns = TRUE, 
                                 autoWidth = TRUE,
                                 ordering = TRUE, 
                                 dom = 'Blfrtip', 
                                 autoWidth = TRUE,
                                 scrollX = TRUE,
                                 
                                 fixedColumns = list(leftColumns = 3),
                                 style = "bootstrap",
                                 selection = "single",
                                 
                                 buttons = list(
                                   list(
                                     extend = 'collection',
                                     buttons = list(
                                       list(extend = "csv", filename = "page",exportOptions = list(
                                         columns = ":visible",modifier = list(page = "current"))
                                       ),
                                       list(extend = 'excel', filename = "page", title = NULL, 
                                            exportOptions = list(columns = ":visible",modifier = list(page = "current")))),
                                     text = 'Download current page'),
                                   
                                   # code for the  second dropdown download button
                                   # this will download the entire dataset using modifier = list(page = "all")
                                   list(
                                     extend = 'collection',
                                     buttons = list(
                                       list(extend = "csv", filename = "data",exportOptions = list(
                                         columns = ":visible",modifier = list(page = "all"))
                                       ),
                                       list(extend = 'excel', filename = "data", title = NULL, 
                                            exportOptions = list(columns = ":visible",modifier = list(page = "all")))),
                                     text = 'Download all data')
                                   
                                 )
                  )
    ) 
  })
  
  #--- Region Stats datatable  
  output$regionalStatsReport <- DT::renderDataTable(server = FALSE,{
    DT::datatable(regionalMonitorReport() 
                  %>%
                    select(-regionCode)
                  ,
                  filter = "top",
                  extensions = c('FixedColumns','Buttons'),
                  rownames = FALSE,
                  # extensions = 'Buttons', 
                  options = list(scrollX=TRUE, 
                                 lengthMenu = c(5,10,15,100),
                                 paging = TRUE, 
                                 searching = TRUE,
                                 fixedColumns = TRUE, 
                                 autoWidth = TRUE,
                                 ordering = TRUE, 
                                 dom = 'Blfrtip', 
                                 autoWidth = TRUE,
                                 scrollX = TRUE,
                                 
                                 fixedColumns = list(leftColumns = 2),
                                 style = "bootstrap",
                                 selection = "single",
                                 
                                 buttons = list(
                                   list(
                                     extend = 'collection',
                                     buttons = list(
                                       list(extend = "csv", filename = "page",exportOptions = list(
                                         columns = ":visible",modifier = list(page = "current"))
                                       ),
                                       list(extend = 'excel', filename = "page", title = NULL, 
                                            exportOptions = list(columns = ":visible",modifier = list(page = "current")))),
                                     text = 'Download current page'),
                                   
                                   # code for the  second dropdown download button
                                   # this will download the entire dataset using modifier = list(page = "all")
                                   list(
                                     extend = 'collection',
                                     buttons = list(
                                       list(extend = "csv", filename = "data",exportOptions = list(
                                         columns = ":visible",modifier = list(page = "all"))
                                       ),
                                       list(extend = 'excel', filename = "data", title = NULL, 
                                            exportOptions = list(columns = ":visible",modifier = list(page = "all")))),
                                     text = 'Download all data')
                                   
                                 )
                                 
                                 
                  )
    ) 
  })
  
  ##
  
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
  
  
  observeEvent(input$doReject, {
    if(nrow(icbt_dataset()[['icbt_error_dataset']])>0)   { 
      CaseReject <- icbt_dataset()[['icbt_error_dataset']] %>%
        filter(!is.na(responsibleId)) %>%
        group_by(interview_key,responsibleId,interview_id) %>%
        distinct(errorCheck) %>%
        summarize(errorMessage = str_c(errorCheck, collapse = " ; ")) %>%
        ungroup()
      
      ##send cases back
      for (i in 1:nrow(CaseReject)  ) {
        row <- CaseReject[i,]  #filter that row
        # print(row$interview_key)
        #send the case back as rejected
        #Rejecting a case as hq
        rejections<- reject_interview_as_hq(
          interview_id=row$interview_id,
          comment = row$errorMessage,
          responsible_id = row$responsibleId,
          verbose = FALSE
        ) }
      
      shinyalert(title = "Error Cases Rejected Succesfullly!", type = "success")
      
    }else{
      shinyalert(title = "No Error Messages to Reject!", type = "warning")
    }
  })
    
  # all_users <- get_interviewers()


}
### End of Server Functions #####
###################################

# Run the application 
shinyApp(ui = ui, server = server)
