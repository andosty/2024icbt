 library(readr)
 icbtdata <- readRDS("testdata/icbtdata.rds")
 
 enumStat <- icbtdata %>%
   group_by(RegionName, regionCode, districtName, districtCode,townCity, borderPostName,team_number, enumerator_name, enumerator_contact ) %>%
   summarise(
       `total Respondents` = n_distinct(interview_key, enumerator_name,transpondent_id,observedRespondentDescription),
       `total Male Respondents` =   sum(ifelse(sex=='Male',1,0)),
       `total Female Respondents` = sum(ifelse(sex=='Female',1,0)),
       `total Traded Commodities` = sum(ifelse(tradeDirection=='Coming in (Import)',1,0))  +  sum(ifelse(tradeDirection=='Going out (Export)',1,0)),
       `total IN(Imports)` = sum(ifelse(tradeDirection=='Coming in (Import)',1,0)) , #distinct count of persons in case roster
       `total OUT(Exports)` = sum(ifelse(tradeDirection=='Going out (Export)',1,0)),
       # `total Transport Means` = 
       `total OtherSpecified Transport Means` = sum(ifelse(!is.na(transportMeans_otherSpecify),1,0)) , 
       `total OtherSpecified Commodities` = sum(ifelse(!is.na(productObserved_otherSpecify),1,0)) ,
       
       `total OtherSpecified Unit of Measures` = sum(ifelse(!is.na(unit_of_measure_otherSpecify),1,0)) ,
       `total Traded Livestocks` = sum(ifelse(commodity_isLivestock=='Yes',1,0)),
       `total Missed Transpondents` = sum(unobservedTraderCount, na.rm = T)
   ) %>%
   mutate(
       across(where(is.numeric), ~replace_na(., 0)),
       `otherSpecified Commodity Percent` =  round((`total OtherSpecified Commodities` /`total Traded Commodities`)*100,2),
   )
 
 
 