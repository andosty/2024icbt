# setwd("C:/2024ICBT/")
source("server/datapackages.R",  local = TRUE)

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
  # source("ui/authpages/register.R", local = TRUE)$value,
  source("ui/authpages/verify-email.R", local = TRUE)$value,

  source("ui/main.R", local = TRUE)$value,
)



# Define server logic required 
server <- function(input, output, session) {
  final_data_toLoad <- paste('Data_final/','icbt_data.RDS' , sep = '')
  final_error_toLoad <- paste('Data_final/','error_data.RDS' , sep = '')
  
  #TradeSummaries
  
  output$totalTranspondent <- renderText({
    value<- nrow( icbt_dataset()[['icbt_dataset_final']] %>% distinct(interview_key, interview_id, transpondent_id))
    format(as.integer(value), big.mark=",")
  })
  output$totalCommoditiesTraded <- renderText({
   value<- nrow( icbt_dataset()[['icbt_dataset_final']] %>% distinct(interview_key, interview_id, transpondent_id,Commodity_id))
    format(as.integer(value), big.mark=",")
  })
  output$totalCommoditiesInflow <- renderText({
    value<- nrow( icbt_dataset()[['icbt_dataset_final']] %>% filter(tradeDirection=='Coming in (Import)') %>% distinct(interview_key, interview_id, transpondent_id,Commodity_id))
    format(as.integer(value), big.mark=",")
  })
  output$totalCommoditiesOutflow <- renderText({
    value<- nrow( icbt_dataset()[['icbt_dataset_final']] %>% filter(tradeDirection=='Going out (Export)') %>% distinct(interview_key, interview_id, transpondent_id,Commodity_id))
    format(as.integer(value), big.mark=",")
  })
  output$totalCommoditiesNetflow <- renderText({
    value<- nrow( icbt_dataset()[['icbt_dataset_final']] %>% filter(tradeDirection=='Going out (Export)') %>% distinct(interview_key, interview_id, transpondent_id,Commodity_id))- 
      nrow( icbt_dataset()[['icbt_dataset_final']] %>% filter(tradeDirection=='Coming in (Import)') %>% distinct(interview_key, interview_id, transpondent_id,Commodity_id))
    format(as.integer(value), big.mark=",")
    
  })
  
  #Error Summaries
  
  output$totalData_Obs <- renderText({
    nrow( icbt_dataset()[['icbt_dataset_final']])
  })
  
  
  output$totalData_Cases <- renderText({
    nrow( icbt_dataset()[['icbt_dataset_final']] %>% distinct(interview_key, interview_id))
  })
  output$totalError_Obs <- renderText({
    nrow( icbt_dataset()[['icbt_error_dataset']])
  })
  output$totalError_Cases <- renderText({
    nrow( icbt_dataset()[['icbt_error_dataset']] %>% distinct(interview_key, interview_id))
  })
  
  #plots
  output$regionalErrorTotal <- renderPlotly({
    regionalErrors<- icbt_dataset()[['icbt_error_dataset']] %>%
      distinct(regionCode, RegionName,interview_key, interview_id) %>%
      group_by(RegionName, regionCode) %>% summarise(totalErrors = n()) %>% arrange(-totalErrors) %>% 
      mutate(stringsAsFactors = FALSE,
             RegionName = as.character(RegionName)
      ) %>% left_join(
           icbt_dataset()[['icbt_dataset_final']] %>% 
            distinct(regionCode, RegionName,interview_key, interview_id) %>%
            group_by(RegionName, regionCode) %>% 
            summarise(
              TotalCases= n()
            )
      )
    
    
    regionErrorData <- data.frame(regionalErrors$regionCode,regionalErrors$RegionName,regionalErrors$totalErrors, regionalErrors$TotalCases, stringsAsFactors = FALSE)
    
    regionErrorData$regionalErrors.RegionName <- factor(regionErrorData$regionalErrors.RegionName, levels = unique(regionErrorData$regionalErrors.RegionName)[order(regionErrorData$regionalErrors.TotalCases, decreasing = F)])
    regionErrorData <- regionErrorData %>% 
      rename(  regionCode = regionalErrors.regionCode ,
              RegionName= regionalErrors.RegionName, 
             TotalCaseWithErrors= regionalErrors.totalErrors,
             TotalCases= regionalErrors.TotalCases
             )
    plot_ly(regionErrorData,  y = ~RegionName,x = ~TotalCaseWithErrors, marker = list(color = "red"),name = "TotalCase With Errors", type = 'bar', orientation = 'h') %>%
      # add_bars(x = ~TotalCaseWithErrors, name = "TotalCaseWithErrors", marker = list(color = "red")) %>%
      add_bars(x = ~TotalCases, name = "TotalCases in Data", marker = list(color = "blue")) %>%
      layout( #showlegend = F,
              xaxis = list(categoryorder = "total descending" ,title = ''
                          ),
             yaxis = list(title = ''
                          )
             )
    
  })
  
  output$regionalErrorCount <- renderPlotly({
    
    regionalErrors<- icbt_dataset()[['icbt_error_dataset']]  %>%
      group_by(RegionName) %>% summarise(totalErrors = n()) %>% arrange(-totalErrors) %>% mutate(stringsAsFactors = FALSE,
                                                                                                 RegionName = as.character(RegionName)
      )
    regionErrorData <- data.frame(regionalErrors$RegionName,regionalErrors$totalErrors, stringsAsFactors = FALSE)
    
    regionErrorData$regionalErrors.RegionName <- factor(regionErrorData$regionalErrors.RegionName, levels = unique(regionErrorData$regionalErrors.RegionName)[order(regionErrorData$regionalErrors.totalErrors, decreasing = F)])
    
    regionErrorData <- regionErrorData %>% 
      rename(
        RegionName= regionalErrors.RegionName,
        totalErrors = regionalErrors.totalErrors
      )

    plot_ly(regionErrorData, x = ~totalErrors, y = ~RegionName, type = 'bar', orientation = 'h')   %>%
      layout(
        xaxis = list(categoryorder = "total descending" ,title = ''
        ),
        yaxis = list(title = ''
        )
      )
  })
  
  
  # output$clock <- renderText({
  #   invalidateLater(180,000) #keep active every 3imins to prevent timeout
  #   Sys.time()
  # })
  
  # Include the logic (server) for each tab
  source(file.path("server/auth/serverloginfunction.R"),  local = TRUE)$value
  
  #getAuthCredDataAccess
  dataRefresh <- reactiveFileReader(3000, session, final_data_toLoad, readRDS)
  errorDataRefresh <- reactiveFileReader(3000, session, final_error_toLoad, readRDS)
  
  #get data from server  #schedule for every 2hrs refresh
  icbt_dataset <- reactive({
    # invalidateLater(7200000) # scheduled for every 2 hours
    # source(file.path("server/hqdata/01_A_download_merge.R"),  local = TRUE)$value
    # source(file.path("server/hqdata/02_merge_for_final_file.R"),  local = TRUE)$value

    # NEW PROCESSING OF DATA LOAD WHEN CHANGED
    user_assigned_data <- read_excel("server/loginDataAccess.xlsx") %>% 
      filter( 
        email ==  user_out_email()
      )
    
    head(user_assigned_data)
    head(  dataRefresh())
    
    icbt_data <- data.frame()
    for(i in 1:nrow(user_assigned_data)) {
      row <- user_assigned_data[i,]
      filteredDataset <-  dataRefresh() %>%
        filter(
          regionCode %in% (row$startRegionCode:row$endRegionCode)
        )  %>%
        # filter(
        #   parse_number(team_number)>=row$startTeamNumber &   parse_number(team_number)<=row$endTeamNumber
        # ) %>%
        arrange(RegionName, districtName, townCity, borderPostName, team_number, enumerator_name )
      
      icbt_data <- dplyr::bind_rows(icbt_data, filteredDataset)
      rm(filteredDataset,row)
    }

   
    icbt_errors <- data.frame()
      for(i in 1:nrow(user_assigned_data)) {
      row <- user_assigned_data[i,]
      filteredErrorDataset <-  errorDataRefresh() %>%
        filter(
          regionCode %in% (row$startRegionCode:row$endRegionCode)
        )  %>%
        # filter(
        #   parse_number(team_number)>=row$startTeamNumber &   parse_number(team_number)<=row$endTeamNumber
        # ) %>%
        arrange(RegionName, districtName, townCity, borderPostName, team_number, enumerator_name )

      icbt_errors <- dplyr::bind_rows(icbt_errors, filteredErrorDataset)
      rm(filteredErrorDataset,row)
    }

    # source(file.path("server/hqdata/03_errorchecks.R"),  local = TRUE)$value
  
    list(icbt_dataset_final=icbt_data ,
         icbt_error_dataset=icbt_errors
         )
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
                          )
                  
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
                          `Tno#`=transpondent_id,
                          `Cno#`=Commodity_id,
                          # transpondentDescription=observedRespondentDescription
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
                               list(extend = "csv", filename = "icbt_page_Errors",exportOptions = list(
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
                               list(extend = "csv", filename = "iocbt_all_errorData",exportOptions = list(
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
                                       list(extend = "csv", filename = "EnumMonitorReport_page",exportOptions = list(
                                         columns = ":visible",modifier = list(page = "current"))
                                       ),
                                       list(extend = 'excel', filename = "EnumMonitorReport_allData", title = NULL, 
                                            exportOptions = list(columns = ":visible",modifier = list(page = "current")))),
                                     text = 'Download current page'),
                                   
                                   # code for the  second dropdown download button
                                   # this will download the entire dataset using modifier = list(page = "all")
                                   list(
                                     extend = 'collection',
                                     buttons = list(
                                       list(extend = "csv", filename = "EnumMonitorReport_page",exportOptions = list(
                                         columns = ":visible",modifier = list(page = "all"))
                                       ),
                                       list(extend = 'excel', filename = "EnumMonitorReport_allData", title = NULL, 
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
  
  downloadFileFormatType <-reactive ({
    switch(
      input$type,
      "Excel (CSV)"="csv",
      "Stata"="dta",
      "R"="rds"
    )
  })
  
  output$downloadData <- downloadHandler(
          filename = function(){
                      paste(input$dataset, ".", downloadFileFormatType(),sep="")
                    },

                  content = function(file){
                    if(downloadFileFormatType()=="csv"){
                      write.csv(serverDataDownload(), file, row.names = FALSE)

                    }else if(downloadFileFormatType()=="dta"){
                      readstata13::save.dta13(serverDataDownload(), file)
                      # write.dta(serverDataDownload(),file  )

                    }else if(downloadFileFormatType()=="rds"){
                     saveRDS(serverDataDownload(),file)
                    }

                    # sep <- switch (input$type,
                    #                "Excel (CSV)" = ",",
                    #                "Stata" = "",
                    #                "R" = ""
                    # )
              
                    # write.table(serverDataDownload(),file, sep = sep)
                    # saveRDS(serverDataDownload(),file)
                  }
  )
  
  
  observeEvent(input$doReject, {
    if(nrow(icbt_dataset()[['icbt_error_dataset']])>0)   { 
      CaseReject <- icbt_dataset()[['icbt_error_dataset']] %>%
        filter(!is.na(responsibleId)) %>%
        # filter(enumerator_name == "Ibrahim Bukari") %>%
        arrange(regionCode, districtCode, borderPostName,interview_key, interview_id)%>%
        group_by( regionCode ,interview_key,responsibleId,interview_id) %>%
        # group_by(interview_key,responsibleId,interview_id) %>%
        distinct(errorCheck) %>%
        summarize(errorMessage = str_c(errorCheck, collapse = " ; ")) %>%
        ungroup()
      
      #server cred login
      
      sqlSvr <- readRDS("server/credentials/icbt_svr.rds")
      set_credentials(
        server = sqlSvr$server, workspace = sqlSvr$workspace, 
        user =sqlSvr$usr, password = sqlSvr$pswd
      )
      server_qnr <- susoapi::get_questionnaires() %>% 
        filter(title == "ICBT MAIN-Field Practice") %>%
        # filter(title == sqlSvr$title) %>%
        # dplyr::pull(questionnaireId)
        # filter(version==max(version))
        filter(version==1)
      
      server_qnr_id <- server_qnr %>%
        # filter(version==6) %>%
        dplyr::pull(id)
      
      ##send cases back
      for (i in 1:nrow(CaseReject)  ) {
        row <- CaseReject[i,]  #filter that row
        # print(row$interview_key)
        #send the case back as rejected
        #Rejecting a case as hq
        
        # print(row$interview_id)
        # print(row$responsibleId)
        
        # get_interview_stats(
        #   interview_id=row$interview_id
        # )
      
      # reject_interview_as_hq(
      #     interview_id=row$interview_id,
      #     comment = "",
      #     responsible_id = row$responsibleId,
      #     # verbose = TRUE
      #   )
        
        rejections<- reject_interview_as_hq(
          interview_id=row$interview_id,
          comment = row$errorMessage,
          responsible_id = row$responsibleId,
          verbose = TRUE
        )
        }
      
  
      
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
