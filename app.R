# setwd("C:/2024ICBT/")
source("server/datapackages.R",  local = TRUE)
# library("assertthat")
# install.packages("shiny.worker")
# library(shiny.worker)
# https://www.appsilon.com/post/shiny-worker-package
# https://rstudio.github.io/promises/articles/promises_06_shiny.html


library("bslib")

theme=bs_theme(
  version = 5,
  bg = "#FFFFFF",
  fg = "#000000",
  primary = "#0199F8",
  secondary = "#FF374B",
  base_font = "Maven Pro"
)


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
  
  # bs_themer()
  
  final_data_toLoad <- paste('Data_final/','icbt_data.RDS' , sep = '')
  final_error_toLoad <- paste('Data_final/','error_data.RDS' , sep = '')
  newInterviewersDataFile <- paste('Data_final/','newInterviewerUsers.RDS' , sep = '')
  lastAccessedData <- paste('Data_final/','lastDataAccessedDateTime.RDS' , sep = '')
  
  #TradeSummaries
  output$lastAccessedDataDownload <- renderText({
      accessedDate()
    })

  
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
  
  
  ## Monitor report filters
  output$quarterDataCollection <- tryCatch({
    renderUI({
      selectInput(inputId = "selectQuarter",
                  label = "Select Quarter:",
                  choices = array(unlist(unique(icbt_dataset()[['icbt_dataset_final']]$quarter))),
                  selected= first(array(unlist(unique(icbt_dataset()[['icbt_dataset_final']]$quarter))) )
      )
    })
  },
  error=function(cond) {
    message(paste("colnames caused a warning:"))
    # message(paste("colnames caused a warning:", temp_colnames))
  },
  warning=function(cond) {
    message(paste("colnames caused a warning:"))
  })
  
  
  observeEvent(input$selectQuarter, {
    output$monthDataCollection <-  renderUI({
      selectInput(inputId = "selectMonth",
                  label = "Month of data collection:",
                  choices = array(unlist((icbt_dataset()[['icbt_dataset_final']]%>% filter(quarter==input$selectQuarter) %>% distinct(month)))),
                  selected= first(
                    array(unlist(icbt_dataset()[['icbt_dataset_final']]%>% filter(quarter==input$selectQuarter) %>% distinct(month)))
                  )
      )
    })
  })
  
  quaterSubsetICBTDataset <- reactive({
    icbt_dataset()[['icbt_dataset_final']] %>% subset(as.integer(quarter) %in% as.integer(input$selectQuarter))
  })
  
  monthSubsetICBTDataset <- reactive({
    quaterSubsetICBTDataset() %>% subset(
      (as.integer(quarter) %in% as.integer(input$selectQuarter)) & 
        (month %in% input$selectMonth)
      )
  })
  
  dateRangeSubsetICBTDataset <- reactive({
    monthSubsetICBTDataset() %>% subset(
      (as.integer(quarter) %in% as.integer(input$selectQuarter)) &
        (month %in% input$selectMonth)  &
        ( dates >= as.Date(input$selectdateRange[1]) & dates <= as_date(input$selectdateRange[2]) )
      )
  })

  # filteredIcbtData <- quaterSubsetICBTDataset()
  
  observeEvent(input$selectMonth, {
    output$dateRangeDataCollection <-  renderUI({
      dateRange <- icbt_dataset()[['icbt_dataset_final']]%>% 
        filter(quarter==input$selectQuarter & month==input$selectMonth ) %>% 
        distinct(dates) %>%
        summarise(min = min(dates),
                  max = max(dates))
      
      dateRangeInput(inputId = "selectdateRange", 
                     label = "Data Range:",
                     start = as.Date(dateRange$min),
                     end = as.Date(dateRange$max),
                     min = as.Date(dateRange$min),
                     max = as.Date(dateRange$max),
                     format = "yyyy-mm-dd",
      )
    })
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
      ) %>% right_join(
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
  
  newInterviewers <- reactiveFileReader(3000, session, newInterviewersDataFile, readRDS)
  
  accessedDate <- reactiveFileReader(3000, session, lastAccessedData, readRDS)

  # collectionPeriod <- reactiveFileReader(3000, session, paste('Data_final/','dataCollectionPeriod.RDS' , sep = ''), readRDS)
  

  
  # output$dateRangeDataCollection <- renderUI({
  #     dateRangeInput(inputId = "selectdateRange", 
  #                 label = "Data Range:",
  #                 # start = as.Date(min(ymd(dataRefresh()$CreatedDate))),
  #                 # end = as.Date(max(dataRefresh()$CreatedDate)),
  #                 # min = as.Date(min(dataRefresh()$CreatedDate)),
  #                 # max = as.Date(max(dataRefresh()$CreatedDate))
  #                 
  #                
  #                 start =  date(as.character(icbt_data%>% filter(month== input$selectMonth) %>% filter(quarter==input$selectQuarter) %>% select(createdDate) %>% slice_min(createdDate) %>%first())),
  #                 # start = as.Date(as.character(dataRefresh()%>% filter(month== input$selectMonth)  %>% filter(quarter==input$selectQuarter) %>% summarise(min(createdDate)))),
  #                 end = date(as.character(icbt_data%>% filter(month== input$selectMonth) %>% filter(quarter==input$selectQuarter) %>% select(createdDate) %>% slice_max(createdDate) %>%first())),
  #                 min = date(as.character(icbt_data%>% filter(month== input$selectMonth) %>% filter(quarter==input$selectQuarter) %>% select(createdDate) %>% slice_min(createdDate) %>%first())),
  #                 max =date(as.character(icbt_data%>% filter(month== input$selectMonth) %>% filter(quarter==input$selectQuarter) %>% select(createdDate) %>% slice_max(createdDate) %>%first()))
  #                 )
  #   })
  
  # output$monthChoices <- renderTable({
  #   # unique(collectionPeriod()$month)
  #   collectionPeriod() %>% distinct(month)
  # })
  
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
    
    # head(user_assigned_data)
    # head(  dataRefresh())
    
    icbt_data <- data.frame()
    for(i in 1:nrow(user_assigned_data)) {
      row <- user_assigned_data[i,]
      filteredDataset <-  dataRefresh() %>%
        subset(
          regionCode %in% (row$startRegionCode:row$endRegionCode) & 
            (team_number >= row$startTeamNumber & team_number<=row$endTeamNumber)
        ) %>%
        arrange(RegionName, districtName, townCity, borderPostName, team_number, enumerator_name )
      
        # filter(
        #   regionCode %in% (row$startRegionCode:row$endRegionCode)
        # )  %>%
        # filter(
        #   parse_number(team_number)>=row$startTeamNumber &   parse_number(team_number)<=row$endTeamNumber
        # ) %>%
      
      icbt_data <- dplyr::bind_rows(icbt_data, filteredDataset)
      rm(filteredDataset,row)
    }

   
    icbt_errors <- data.frame()
      for(i in 1:nrow(user_assigned_data)) {
      row <- user_assigned_data[i,]
      filteredErrorDataset <-  errorDataRefresh() %>%
        subset(
          regionCode %in% (row$startRegionCode:row$endRegionCode) & 
            (team_number >= row$startTeamNumber & team_number<=row$endTeamNumber)
        ) %>%
        # filter(
        #   regionCode %in% (row$startRegionCode:row$endRegionCode)
        # )  %>%
        # filter(
        #   parse_number(team_number)>=row$startTeamNumber &   parse_number(team_number)<=row$endTeamNumber
        # ) %>%
        arrange(RegionName, districtName, townCity, borderPostName, team_number, enumerator_name )

      icbt_errors <- dplyr::bind_rows(icbt_errors, filteredErrorDataset)
      rm(filteredErrorDataset,row)
    }

    # source(file.path("server/hqdata/03_errorchecks.R"),  local = TRUE)$value
  
    list(icbt_dataset_final=icbt_data %>% mutate(enumerator_name=str_to_title(str_squish(trim(enumerator_name))))  ,
         icbt_error_dataset=icbt_errors %>%  mutate(enumerator_name=str_to_title(str_squish(trim(enumerator_name)))) 
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
                    arrange(region, district,team, border, enumerator_name) # %>%
                    # select(-regionCode, -districtCode,-townCity)
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
                  %>%   arrange(region,district, team, border) %>%
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
                  %>%  arrange(region, district, border) %>%
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
                  %>%  arrange(region, district) %>% 
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
                  %>% arrange(region) %>%
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
            "icbt data" = icbt_dataset()[['icbt_dataset_final']] %>% select(-gps_Latitude ,-gps_Longitude, -gps_Accuracy, -gps_Altitude),
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
  
  #manual rejection filters
  #-------------------------start
  #Select region
  #select the REGION  ####################
  output$rejectionRegion <- tryCatch({
    renderUI({
            selectInput(inputId = "selectRegionToReject",
                        label = "Select Region:",
                        choices = array(unlist(icbt_dataset()[['icbt_error_dataset']] %>% distinct(RegionName) %>% arrange(RegionName))),
                        # choices = unique(icbt_dataset()[['icbt_error_dataset']] %>% distinct(RegionName)),
                        selected= first(array(unlist(icbt_dataset()[['icbt_error_dataset']] %>% distinct(RegionName)%>% arrange(RegionName))) )
            )
          })
    },
    error=function(cond) {
      message(paste("colnames caused a warning:"))
      # message(paste("colnames caused a warning:", temp_colnames))
      # message(cond)
      
    },
    warning=function(cond) {
      message(paste("colnames caused a warning:"))
      # message(paste("colnames caused a warning:", temp_colnames))
      # message(cond)
      
    },
  finally={
    message(paste("Processing colnames: "))
    # message(paste("Processing colnames: ", temp_colnames))
  })
  #select the rEGION END ####################
  
  #select team number
  #select the tEAM  ####################
  observeEvent(input$selectRegionToReject, {
    if(!is.null(input$selectRegionToReject)){ 
        output$rejectionTeamNumber <-  renderUI({
                selectInput(inputId = "selectTeamToReject",
                            label = "Select Team:",
                            choices = array(unlist(icbt_dataset()[['icbt_error_dataset']] %>% filter(RegionName==input$selectRegionToReject) %>% distinct(team_number) %>% arrange(team_number))),
                            selected= first(
                              array(unlist(icbt_dataset()[['icbt_error_dataset']] %>% filter(RegionName==input$selectRegionToReject) %>% distinct(team_number) %>% arrange(team_number)))
                            )
                )
              })
    } else {
      output$rejectionTeamNumber <-  renderUI({
        selectInput(inputId = "selectTeamToReject",
                    label = "No Team Selected:",
                    choices = c(""),
                    selected= ""
        )
      })
      }
  })
  #select the tEAM END ####################
  
  # #select the enumerator  ####################
  observeEvent(input$selectTeamToReject, {
          if(!is.null(input$selectTeamToReject)){ 
                output$rejectionEnumeratorName <-  renderUI({
                        selectInput(inputId = "selectEnumeratorToReject",
                                    label = "Select Enumarator:",
                                    choices = array(unlist( icbt_dataset()[['icbt_error_dataset']] %>% filter(RegionName==input$selectRegionToReject & team_number==input$selectTeamToReject) %>% distinct(enumerator_name) %>% arrange(enumerator_name))),
                                    selected= first(
                                      array(unlist( icbt_dataset()[['icbt_error_dataset']] %>% filter(RegionName==input$selectRegionToReject & team_number==input$selectTeamToReject) %>% distinct(enumerator_name) %>% arrange(enumerator_name)))
                                                    )
                                    )
                })
          } else {
                      output$rejectionEnumeratorName <-  renderUI({
                        selectInput(inputId = "selectEnumeratorToReject",
                                    label = "No Enumerator Selected:",
                                    choices = c(""),
                                    selected= ""
                        )
                      })
          }
  })
  
  # #select the CASE
  # #select the CASE  ####################
  observeEvent(input$selectEnumeratorToReject, {
    if(!is.null(input$selectEnumeratorToReject)){ 
      output$rejectionEnumeratorCase <-  renderUI({
                selectInput(inputId = "selectEnumeratorCaseToReject",
                            label = "Select Case:",
                            choices = array(unlist( icbt_dataset()[['icbt_error_dataset']] %>% filter(RegionName==input$selectRegionToReject & team_number==input$selectTeamToReject & enumerator_name==input$selectEnumeratorToReject) %>% distinct(interview_key) %>% arrange(interview_key))),
                            selected= first(
                              array(unlist( icbt_dataset()[['icbt_error_dataset']] %>% filter(RegionName==input$selectRegionToReject & team_number==input$selectTeamToReject & enumerator_name==input$selectEnumeratorToReject) %>% distinct(interview_key) %>% arrange(interview_key)))                                )
                            )
      })
    } else {
      output$rejectionEnumeratorName <-  renderUI({
        selectInput(inputId = "selectEnumeratorCaseToReject",
                    label = "No Case is Selected:",
                    choices = c(""),
                    selected= ""
        )
      })
    }
  })
  
  # #button to click  to get Tablet Sync History  ####################
  observeEvent(input$selectEnumeratorCaseToReject, {
    if(!is.null(input$selectEnumeratorCaseToReject)){ 
      div(style = "bootstrap",
        output$tabletHistoryButton <-  renderUI({
          actionButton("doGetTabletHistoryButton", "Sync Stats",type="button", class="btn btn-primary")
        }),
        output$clickManualRejection <-  renderUI({
          actionButton("doManualReject", "Reject Case",type="button", class="btn btn-danger")
        })
        
      )
    } else {
        output$tabletHistoryButton <-  renderUI()
     }
   })
    

  
  interview_idToReject <- reactive(
                filter(
                  icbt_dataset()[['icbt_error_dataset']] %>% 
                    filter(RegionName==input$selectRegionToReject &
                             team_number==input$selectTeamToReject &
                             enumerator_name==input$selectEnumeratorToReject &
                             interview_key==input$selectEnumeratorCaseToReject 
                    )
                ) %>% distinct(interview_key,enumerator_name,responsibleId, .keep_all = T) %>% pull(interview_id)
  )
  
  interviewerCaseStatus  <- reactive({
    get_interview_stats(
      interview_idToReject()
    )
  })
  
  getTabletActivity <- reactive({
    get_user_action_log(
      user_id = interviewerCaseStatus()$ResponsibleId,
      start =  as.character(today()-1),
      end = as.character(today()+1)
    ) %>% filter( Message !="Sync started (Online)")
  })
  
  
  # Get tablet history
  observeEvent(input$doGetTabletHistoryButton, {
    interview_idToReject()
    interviewerCaseStatus()
    # getTabletActivity()
    
    output$tabletSyncStat <- DT::renderDataTable({
                        DT::datatable(getTabletActivity() %>%
                                        mutate(
                                            region = input$selectRegionToReject ,
                                            team=input$selectTeamToReject ,
                                            enumerator_name=input$selectEnumeratorToReject,
                                            Time = format(ymd_hms(Time,tz=Sys.timezone()), "%Y-%m-%d %I:%M %p"),
                                          ) %>% select(-UserId) %>% arrange(region,team,enumerator_name, desc(Time))
                                      )
    })
    
    # if(not_empty(input$selectRegionToReject) &
    #             not_empty(input$selectTeamToReject) &
    #             not_empty(input$selectEnumeratorToReject) &
    #             not_empty(input$selectEnumeratorCaseToReject) ){
    # 
    #       # if(!is.null(input$selectEnumeratorCaseToReject) & !is.null(input$selectEnumeratorCaseToReject)){
    #   # toReject <- filter(
    #   #                     icbt_dataset()[['icbt_error_dataset']] %>% 
    #   #                       filter(RegionName==input$selectRegionToReject &
    #   #                                team_number==input$selectTeamToReject &
    #   #                                enumerator_name==input$selectEnumeratorToReject &
    #   #                                interview_key==input$selectEnumeratorCaseToReject 
    #   #                              )
    #   #                 ) %>% distinct(interview_key,enumerator_name,responsibleId, .keep_all = T)
    #   #                                                                    
    #   
    #   # interviewerCase_status<- reactive({
    #   #   get_interview_stats(
    #   #     toReject$interview_id
    #   #   )
    #   # }) 
    #     
    #   # get_tablet_activity <- reactive({
    #   #     get_user_action_log(
    #   #   user_id = interviewerCase_status()$ResponsibleId,
    #   #   start =  as.character(today()-1),
    #   #   end = as.character(today()+1)
    #   # ) %>% filter( Message !="Sync started (Online)")
    #   # })
    #   
    #   ######
    #   tryCatch({
    #     interview_idToReject()
    #     interviewerCaseStatus()
    #     getTabletActivity()
    #     
    #     output$tabletSyncStat <- DT::renderDataTable({
    #       DT::datatable(getTabletActivity() %>% mutate(
    #         region = input$selectRegionToReject ,
    #         team_number=input$selectTeamToReject ,
    #         enumerator_name=input$selectEnumeratorToReject
    #       ) 
    #       )
    #     })
    #     
    #   },
    #   warning=function(cond) {
    #     message(paste("colnames caused a warning:"))
    #     # message(cond)
    #   })
      ########
          
    # }  
      
    # else {
    #   output$tabletSyncStat <- DT::renderDataTable({
    #     DT::datatable(data.frame() )  #pass empty dataframe
    #   })
    # }
  })
  
        
     
   
  observeEvent(input$doManualReject, {

    if(nrow(interviewerCaseStatus()) >0)   {
      
      if(reject_interview_as_hq(
        interview_id= interviewerCaseStatus()$InterviewId,
        comment = "check and correct your errors",
        responsible_id = interviewerCaseStatus()$ResponsibleId,
        verbose = TRUE
      )==TRUE){
        shinyalert(
          title ="Task Successfull!",
          text = paste0("Case-Key '",interviewerCaseStatus()$InterviewKey,"' Rejected Succesfullly!"," to Interviewer Name:", str_to_title(input$selectEnumeratorToReject) ),
          type = "success"
          )
      } else {
        shinyalert(
          title ="Rejection Error!",
          text = paste0("Case-Key '",interviewerCaseStatus()$InterviewKey,"' Rejected Failed!"," for Interviewer Name: ", str_to_title(input$selectEnumeratorToReject) ),
          type = "warning"
        )
      }
      
    } else{
      shinyalert(title = "There are NO CASES with Error Messages to Reject!", type = "warning")
    }
  })    
  
  #get tablet sync history table
  # observeEvent(input$getTabletHistory, {
  #   
  # })
    
  
  
  # #select the CASE END ####################
  
  
  
  #select team number
  #select the tEAM start ####################
  # if(!is.null(input$selectRegionToReject)){
  #   output$rejectionTeamNumber <- tryCatch({
  #     renderUI({
  #       selectInput(inputId = "selectTeamToReject",
  #                   label = "Select Team:",
  #                   choices = array(unlist(icbt_dataset()[['icbt_error_dataset']] %>% filter(RegionName==input$selectRegionToReject) %>% distinct(team_number) %>% arrange(team_number))),
  #                   selected= first(
  #                     array(unlist(icbt_dataset()[['icbt_error_dataset']] %>% filter(RegionName==input$selectRegionToReject) %>% distinct(team_number) %>% arrange(team_number)))
  #                   )
  #       )
  #     })
  #     # temp_t_test_result <- t.test(formula(paste0("`", temp_colnames,"`~group")), data = temp_data)
  #     
  #   },
  #   error=function(cond) {
  #     message(paste("colnames caused a warning:"))
  #     # message(paste("colnames caused a warning:", temp_colnames))
  #     # message(cond)
  #     
  #   },
  #   warning=function(cond) {
  #     message(paste("colnames caused a warning:"))
  #     # message(paste("colnames caused a warning:", temp_colnames))
  #     # message(cond)
  #     
  #   },
  #   finally={
  #     message(paste("Processing colnames: "))
  #     # message(paste("Processing colnames: ", temp_colnames))
  #   })
  # } else {
  #   renderUI({
  #     selectInput(inputId = "selectTeamToReject",
  #                 label = "Select Team:",
  #                 choices = c(""),
  #                 selected= ""
  #     )
  #   })
  # }
  

  
  #select the tEAM END ####################
  
  # #select the enumerator
  # #select the enumerator start ####################
  # output$rejectionEnumeatorName <- tryCatch({
  #   renderUI({
  #       selectInput(inputId = "selectEnumeratorToReject",
  #                   label = "Select Enumarator:",
  #                   choices = array(unlist( icbt_dataset()[['icbt_error_dataset']] %>% filter(RegionName==input$selectRegionToReject & team_number==input$selectTeamToReject) %>% distinct(enumerator_name) %>% arrange(enumerator_name))),
  #                   selected= first(
  #                     array(unlist( icbt_dataset()[['icbt_error_dataset']] %>% filter(RegionName==input$selectRegionToReject & team_number==input$selectTeamToReject) %>% distinct(enumerator_name) %>% arrange(enumerator_name)))
  #                                   )
  #                   )
  #   })
  #   # temp_t_test_result <- t.test(formula(paste0("`", temp_colnames,"`~group")), data = temp_data)
  #   
  # },
  # error=function(cond) {
  #   message(paste("colnames caused a warning:"))
  #   # message(paste("colnames caused a warning:", temp_colnames))
  #   # message(cond)
  #   
  # },
  # warning=function(cond) {
  #   message(paste("colnames caused a warning:"))
  #   # message(paste("colnames caused a warning:", temp_colnames))
  #   # message(cond)
  #   
  # },
  # finally={
  #   message(paste("Processing colnames: "))
  #   # message(paste("Processing colnames: ", temp_colnames))
  # })
  # #select the enumerator eND ####################
  # #select the case
  # 
  # #select the CASE start ####################
  # output$rejectionEnumeratorCase <- tryCatch({
  #   renderUI({
  #         selectInput(inputId = "selectEnumeratorCaseToReject",
  #                     label = "Select Case:",
  #                     choices = array(unlist( icbt_dataset()[['icbt_error_dataset']] %>% filter(RegionName==input$selectRegionToReject & team_number==input$selectTeamToReject & enumerator_name==input$selectEnumeratorToReject) %>% distinct(interview_key) %>% arrange(interview_key))),
  #                     selected= first(
  #                       array(unlist( icbt_dataset()[['icbt_error_dataset']] %>% filter(RegionName==input$selectRegionToReject & team_number==input$selectTeamToReject & enumerator_name==input$selectEnumeratorToReject) %>% distinct(interview_key) %>% arrange(interview_key)))                                )
  #                     )
  #   })
  #   # temp_t_test_result <- t.test(formula(paste0("`", temp_colnames,"`~group")), data = temp_data)
  #   
  # },
  # error=function(cond) {
  #   message(paste("colnames caused a warning:"))
  #   # message(paste("colnames caused a warning:", temp_colnames))
  #   # message(cond)
  #   
  # },
  # warning=function(cond) {
  #   message(paste("colnames caused a warning:"))
  #   # message(paste("colnames caused a warning:", temp_colnames))
  #   # message(cond)
  #   
  # },
  # finally={
  #   message(paste("Processing colnames: "))
  #   # message(paste("Processing colnames: ", temp_colnames))
  # })
  # #select the case end ####################
  
  # END ALL -------------------------------
  
  # output$rejectionTeamNumber <- renderUI({
  #   selectInput(inputId = "selectTeamToReject",
  #               label = "Select Team:",
  #               choices = array(unlist(icbt_dataset()[['icbt_error_dataset']] %>% filter(RegionName==input$selectRegionToReject) %>% distinct(team_number) %>% arrange(team_number))),
  #               selected= first(
  #                 array(unlist(icbt_dataset()[['icbt_error_dataset']] %>% filter(RegionName==input$selectRegionToReject) %>% distinct(team_number) %>% arrange(team_number)))
  #                               )
  #               )
  # })

  # output$rejectionEnumeatorName <- renderUI({
  #   selectInput(inputId = "selectEnumeratorToReject",
  #               label = "Select Enumarator:",
  #               choices = array(unlist( icbt_dataset()[['icbt_error_dataset']] %>% filter(RegionName==input$selectRegionToReject & team_number==input$selectTeamToReject) %>% distinct(enumerator_name) %>% arrange(enumerator_name))),
  #               selected= first(
  #                 array(unlist( icbt_dataset()[['icbt_error_dataset']] %>% filter(RegionName==input$selectRegionToReject & team_number==input$selectTeamToReject) %>% distinct(enumerator_name) %>% arrange(enumerator_name)))
  #                               )
  #               )
  # })
# 
#   output$rejectionEnumeratorCase <- renderUI({
#     selectInput(inputId = "selectEnumeratorCaseToReject",
#                 label = "Select Case:",
#                 choices = array(unlist( icbt_dataset()[['icbt_error_dataset']] %>% filter(RegionName==input$selectRegionToReject & team_number==input$selectTeamToReject & enumerator_name==input$selectEnumeratorToReject) %>% distinct(interview_key) %>% arrange(interview_key))),
#                 selected= first(
#                   array(unlist( icbt_dataset()[['icbt_error_dataset']] %>% filter(RegionName==input$selectRegionToReject & team_number==input$selectTeamToReject & enumerator_name==input$selectEnumeratorToReject) %>% distinct(interview_key) %>% arrange(interview_key)))                                )
#                 )
#   })
  
  # 
  # #get enum case
  # caseToReject <- icbt_dataset()[['icbt_error_dataset']] %>% 
  #   filter(RegionName==input$selectRegionToReject & team_number==input$selectTeamToReject & enumerator_name==input$selectEnumeratorToReject & interview_key==input$selectEnumeratorCaseToReject  ) %>%
  #   distinct(interview_key, .keep_all = T) 
  # 
  # interviewerCase_status<- get_interview_stats(
  #   caseToReject$interview_id
  # )
  # 
  # #to display in table
  # # InterviewKey, ResponsibleName, NumberOfInterviewers, NumberRejectionsBySupervisor,NumberRejectionsByHq,InterviewDuration,UpdatedAtUtc
  # 
  # get_tablet_activity <- get_user_action_log(
  #   user_id = interviewerCase_status$ResponsibleId,
  #   start =  as.character(today()-1),
  #   end = as.character(today()+1)
  # ) %>% filter( Message !="Sync started (Online)")
  # 
  
  
  #-------------------------end
  
  # reject all enum cases  
  #-------------------------start
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
      
      failedRejections = data.frame()
      
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
        
        if(reject_interview_as_hq(
          interview_id=row$interview_id,
          comment = row$errorMessage,
          responsible_id = row$responsibleId,
          verbose = TRUE
        )==TRUE){
          print("case successfully rejected")
        } else {
          #add case to failedRejections Dataframe be retried for re-rejections
          # print("rejection failed")
          failedRejections <- rbind(failedRejections,row)
        }
        
      }
      
      if(nrow(failedRejections)>0){
        # library(dplyr)
        
        needManualRejections =  data.frame()
        
        for (i in 1:nrow(failedRejections)  ) {
            errorRetryRow <- failedRejections[i,]
            # extract the user ID for the target user
            print("to Retry Error Re-Rejection Again")
            target_user_id <- newInterviewers() %>%
                              filter( 
                                # str_to_upper(FullName) == str_to_upper(errorRetryRow$enumerator_name) & 
                                       UserId == errorRetryRow$responsibleId
                                    ) %>%
                              pull(UserId)
            
            # collect a log of activity on the tablet within specified dates
            tablet_activity <- get_user_action_log(
              user_id = target_user_id,
              start = "2020-02-15",
              end = "2025-12-30"
            )
            
            
            assignment_id <- get_assignments(
              # search_by = "",
              # qnr_id = "",
              # qnr_version = "",
              responsible = target_user_id,
              # supervisor_id = "",
              # show_archive = FALSE,
              # order = "",
              server = Sys.getenv("SUSO_SERVER"),
              workspace = Sys.getenv("SUSO_WORKSPACE"),
              user = Sys.getenv("SUSO_USER"),
              password = Sys.getenv("SUSO_PASSWORD")
            ) %>% 
             arrange(
                CreatedAtUtc
              ) %>% 
              last() %>%
              # filter(ResponsibleId==target_user_id) %>%
              pull(Id)
            
            print(assignment_id)
            
            # Get details for a single assignment
            ass_Details<- get_assignment_details(
              id=assignment_id
              # server = Sys.getenv("SUSO_SERVER"),
              # workspace = Sys.getenv("SUSO_WORKSPACE"),
              # user = Sys.getenv("SUSO_USER"),
              # password = Sys.getenv("SUSO_PASSWORD")
            ) %>% arrange(
              CreatedAtUtc
            ) %>% last()
           
            
            #retry rejection
            if(
            reject_interview_as_hq(
              interview_id= errorRetryRow$interview_id ,
              comment = "check and fix errors",
              responsible_id = ass_Details$ResponsibleId,
              verbose = TRUE,
              server = Sys.getenv("SUSO_SERVER"),
              workspace = Sys.getenv("SUSO_WORKSPACE"),
              user = Sys.getenv("SUSO_USER"),
              password = Sys.getenv("SUSO_PASSWORD")
            )==TRUE){
              print("retry Rejection of case successfully")
            } else {
              #add case to failedRejections Dataframe be retried for re-rejections
              print("retry to Reject still failed")
              needManualRejections <- rbind(needManualRejections,errorRetryRow)
            }
            
          } #end
        
        if(nrow(needManualRejections)>0 ){
          saveRDS(needManualRejections,paste0("Data_final/","finalFailedRejections.RDS"))
        }
      }
    
      
    shinyalert(title = "Error Cases Rejected Succesfullly!", type = "success")
      
    }else{
      shinyalert(title = "There are NO CASES with Error Messages to Reject!", type = "warning")
    }
  })
    
}
### End of Server Functions #####
###################################

# Run the application 
shinyApp(ui = ui, server = server)
# run_with_themer(shinyApp(ui = ui, server = server))



# div(style="display:inline-block;vertical-align:top;",
#     fluidRow(
#       column(4,
#              br(),
#              bsButton("q1", label = "click for more info", icon = icon("question"), style = "info", size = "extra-small")),
#       column(8, 
#              fileInput("file", "Upload File *", accept = ".txt"))
#       
#     )),
# bsPopover(id = "q1", title = "data",
#           content = "more info",
#           placement = "right", 
# )
# 
# 
# ui <- fluidPage(
#   fluidRow(
#     div(plotOutput('plot'), style = 'width: 80%; display: inline-block; vertical-align: middle;'),
#     div(HTML('Place me vertically centered'), style = 'display: inline-block; vertical-align: middle;')
#   )
# )
# 
# 
# fluidRow(
#   column(8, align = "right", plotOutput("plot")), 
#   column(4, align = "left", "Place me vertically centered.")
# )