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
                choices = array(unlist(unique(icbt_dataset()[['icbt_dataset_final']]%>% filter(quarter==input$selectQuarter) %>% distinct(month)))),
                selected= first(
                                array(unlist(icbt_dataset()[['icbt_dataset_final']]%>% filter(quarter==input$selectQuarter) %>% distinct(month)))                  
                                )
                  )
  })
})



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
                   max = as.Date(dateRange$max)
                   )
  })
})


filteredIcbtData <- reactive({
  icbt_dataset()[['icbt_dataset_final']]   %>%
    filter(quarter==input$selectQuarter)  %>%
    filter(month==input$selectMonth)
})

# filtered_ICBT_data <- icbt_dataset()[['icbt_dataset_final']]   %>%
#   filter(quarter==input$selectQuarter)  %>%
#   filter(month==input$selectMonth) # %>%
  # arrange(regionCode, team_number,districtCode, borderPostName, enumerator_name) %>%
  # filter(RegionName=="Western North")
  # filter(month== input$selectMonth)

filtered_ICBT_Errors<-  icbt_dataset()[['icbt_error_dataset']] %>%
  mutate(
    year = year(as.Date( gps_Timestamp )),
    month = as.character(month(gps_Timestamp, label = TRUE, abbr = FALSE))
  ) # %>%
  # filter(quarter==input$selectQuarter)  %>% 
  # filter(month== input$selectMonth)

# output$dateRangeDataCollection <- renderUI({
#   dateRangeInput(inputId = "selectdateRange", 
#                  label = "Data Range:",
#                  # start = as.Date(min(ymd(dataRefresh()$CreatedDate))),
#                  # end = as.Date(max(dataRefresh()$CreatedDate)),
#                  # min = as.Date(min(dataRefresh()$CreatedDate)),
#                  # max = as.Date(max(dataRefresh()$CreatedDate))
#                  # start = as.Date(as.character(dataRefresh()%>% filter(month== input$selectMonth)  %>% filter(quarter==input$selectQuarter) %>% summarise(min(createdDate)))),
#                  # start=
#                  
#                  # start = date(min(filtered_ICBT_data$createdDate)),
#                  # start = as.Date("2024-10-01"),
#                  # end =  as.Date("2024-12-10"),
#                  # min =   as.Date("2024-10-01"),
#                  # max =  as.Date("2024-12-10"),
#   )
# })

enumMeta <- filteredIcbtData() %>%   select(regionCode, RegionName, districtCode, districtName, townCity, borderPostName,team_number,
                                                                enumerator_name, enumerator_contact ,) %>%
  distinct(regionCode, RegionName, districtCode, districtName, townCity, borderPostName,team_number,
           enumerator_name, enumerator_contact
  )

enumStat_respondent_flow<- filteredIcbtData() %>% 
  distinct(regionCode, RegionName, districtCode, districtName, townCity, borderPostName,team_number,
           enumerator_name, enumerator_contact ,interview_key,
           interview_id, interview_key, transpondent_id, observedRespondentDescription,.keep_all = T) %>%
  group_by (regionCode, RegionName, districtCode, districtName, townCity, borderPostName,team_number,
            enumerator_name, enumerator_contact,
  ) %>%
  summarise(
    `total Cases` = n_distinct(interview_key),
    `total Respondents` = n(),
    `total IN_MIGRATION(X)` = sum(ifelse(tradeDirection=='Coming in (Import)',1,0)),
    `total OUT_MIGRATION(M)` = sum(ifelse(tradeDirection=='Going out (Export)',1,0)),
    `Net Migration(X-M)` = `total IN_MIGRATION(X)`  - `total OUT_MIGRATION(M)`,
    `total Male Respondents` =   sum(ifelse(sex=='Male',1,0)),
    `total Female Respondents` = sum(ifelse(sex=='Female',1,0)),
    `total OtherSpecified Transport Means` = sum(ifelse(transportMeans_otherSpecify!="",1,0)) , 
    # `total Missed Transpondents` = sum(unobservedTraderCount, na.rm = T)
  ) %>%
  # left_join(enumErrsMeta) %>%
  select(
    regionCode, RegionName, districtCode, districtName, townCity, borderPostName,team_number,
    enumerator_name, enumerator_contact,
    everything(),#rrorCasesCount,ErrorsCount,
  ) %>%
  mutate(
    across(where(is.numeric), ~replace_na(., 0)),
    `other Transport %` =  round((`total OtherSpecified Transport Means` /`total Respondents`)*100,2),
  )  %>% left_join(
            filtered_ICBT_Errors  %>%  
                        group_by(regionCode, RegionName, districtCode, districtName, townCity, borderPostName,team_number,
                                 enumerator_name, enumerator_contact)  %>%
              summarise(
                          totalCasesWithErrors = n_distinct(interview_key),
                          totalErrors =  n()
                        ) %>% ungroup()
  ) %>%
  mutate(
    across(where(is.numeric), ~replace_na(., 0)),
  ) %>% relocate(
    c("totalCasesWithErrors","totalErrors"), .after = `total Respondents`
  )

# enum errors
# enumErrorStats<-icbt_dataset()[['icbt_error_dataset']]  %>%  
#   group_by(regionCode, RegionName, districtCode, districtName, townCity, borderPostName,team_number,
#                             enumerator_name, enumerator_contact)  %>%
#   summary(
#     totalCasesWithErrors = n_distinct(interview_key),
#       totalErros =  n()
#   ) %>% ungroup()
  


enumStat_trade <- filteredIcbtData() %>%
  group_by(regionCode, RegionName, districtCode, districtName, townCity, borderPostName,team_number,
           enumerator_name, enumerator_contact) %>%
  summarise(
    `total Traded Commodities` = sum(ifelse(tradeDirection=='Coming in (Import)',1,0))  +  sum(ifelse(tradeDirection=='Going out (Export)',1,0)),
    `total Goods IN(Imports)` = sum(ifelse(tradeDirection=='Coming in (Import)',1,0)) , #distinct count of persons in case roster
    `total Goods OUT(Exports)` = sum(ifelse(tradeDirection=='Going out (Export)',1,0)),
    `OtherSpecified Commodities` = sum(ifelse(productObserved_otherSpecify !='',1,0)) ,
    `OtherSpecified Unit of Measures` = sum(ifelse(unit_of_measure_otherSpecify !='',1,0)) ,
    `total Traded Livestocks` = sum(ifelse(commodity_isLivestock=='Yes',1,0)),
    `total Missed Transpondents` = sum(unobservedTraderCount, na.rm = T)
  ) %>%
  mutate(
    across(where(is.numeric), ~replace_na(., 0)),
    `otherSpecified Commodity Percent` =  round((`OtherSpecified Commodities` /`total Traded Commodities`)*100,2),
    `otherSpecified unit of Measurement Percent` =  round((`OtherSpecified Unit of Measures`/`total Traded Commodities`)*100,2),
  ) %>%
  select(
    everything(),
    `total Traded Commodities`,
    `total Goods IN(Imports)`,
    `total Goods OUT(Exports)` ,
    `OtherSpecified Commodities`,
    `otherSpecified Commodity Percent`,
    `OtherSpecified Unit of Measures`,
    `otherSpecified unit of Measurement Percent`,
    `total Traded Livestocks`,
    `total Missed Transpondents`
  )

enumStatREPORT <- left_join(enumMeta,enumStat_respondent_flow, 
                            by = join_by(regionCode, RegionName, districtCode, districtName, townCity, 
                                         borderPostName, team_number, enumerator_name,enumerator_contact)
) %>%
  left_join(enumStat_trade,
            by = join_by(regionCode, RegionName, districtCode, districtName, townCity, 
                         borderPostName, team_number, enumerator_name,enumerator_contact)
  ) %>% 
  ungroup() %>%
  arrange(
    regionCode,districtCode,townCity,borderPostName,team_number,enumerator_name
  ) %>%
  dplyr::rename(
    border=borderPostName,
    # border=borderPostName,
    district=districtName,
    region=RegionName,
    team = team_number
  ) %>%
  arrange(regionCode, team,districtCode, border, enumerator_name) #%>%
  # filter(region=="Western North")