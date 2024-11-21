setwd("C:/2024ICBT/")
#downloaded_icbt_data
#install some packages
# if(!require(labelled )) install.packages("labelled ")
# if(!require(shinyalert)) install.packages("shinyalert")
# Load the package
# library(shinyalert)
# library(bs4Dash)
library(dplyr)
library(DT)
library(foreign)
library(fresh)
library(gdata)
library(haven)
library(hcandersenr)
library(htmltools)
library(leaflet)
library(lubridate)
library(openxlsx)
library(plotly)
library(pluralize)
library(readr)
library(readxl)
# library(shiny)
# library(shinyjs)
library(SnowballC)
library(splitstackshape)
library(stopwords)
library(stringi)
library(stringr)
# library(susoapi)
# library(susoflows)
library(tidyr)
library(tidytext)
library(tidyverse)
library(tm)

final_data_toLoad <- paste('Data_final/','icbt_data.RDS' , sep = '')
final_error_toLoad <- paste('Data_final/','error_data.RDS' , sep = '')

filtered_ICBT_data <-  readRDS(final_data_toLoad)
  icbt_error_dataset <- readRDS(final_error_toLoad)

enumStat_respondent_flow<- filtered_ICBT_data %>% 
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
    everything(),#ErrorCasesCount,ErrorsCount,
  ) %>%
  mutate(
    across(where(is.numeric), ~replace_na(., 0)),
    `other Transport %` =  round((`total OtherSpecified Transport Means` /`total Respondents`)*100,2),
  ) %>% left_join(
    icbt_error_dataset %>%  
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



tbe <- icbt_error_dataset %>%  
  group_by(regionCode, RegionName, districtCode, districtName, townCity, borderPostName,team_number,
           enumerator_name, enumerator_contact)  %>%
  summarise(
    totalCasesWithErrors = n_distinct(interview_key),
    totalErrors =  n()
  ) %>% ungroup()


dateONly <- icbt_error_dataset %>%
  mutate(
    dates = as.Date(gps_Timestamp)
  )