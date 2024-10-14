 library(readr)
 icbtdata <- readRDS("C:/2024ICBT/testdata/icbtdata.rds")
 
 ## will need frame of all border places to see if they are in data
 
 enumMeta <- icbtdata %>%   select(regionCode, RegionName, districtCode, districtName, townCity, borderPostName,team_number,
                                   enumerator_name, enumerator_contact ,) %>%
   distinct(regionCode, RegionName, districtCode, districtName, townCity, borderPostName,team_number,
            enumerator_name, enumerator_contact
            )
 
 enumStat_respondent_flow<- icbtdata %>% 
   distinct(regionCode, RegionName, districtCode, districtName, townCity, borderPostName,team_number,
            enumerator_name, enumerator_contact ,
            interview_id, interview_key, transpondent_id, observedRespondentDescription,.keep_all = T) %>%
   group_by (regionCode, RegionName, districtCode, districtName, townCity, borderPostName,team_number,
             enumerator_name, enumerator_contact,
             ) %>%
   summarise(`total Respondents` = n(),
             `total IN_MIGRATION(X)` = sum(ifelse(tradeDirection=='Coming in (Import)',1,0)),
             `total OUT_MIGRATION(M)` = sum(ifelse(tradeDirection=='Going out (Export)',1,0)),
             `Net Migration(X-M)` = `total IN_MIGRATION(X)`  - `total OUT_MIGRATION(M)`,
             `total Male Respondents` =   sum(ifelse(sex=='Male',1,0)),
             `total Female Respondents` = sum(ifelse(sex=='Female',1,0)),
             `total OtherSpecified Transport Means` = sum(ifelse(transportMeans_otherSpecify!="",1,0)) , 
             # `total Missed Transpondents` = sum(unobservedTraderCount, na.rm = T)
            ) %>%
   mutate(
       across(where(is.numeric), ~replace_na(., 0)),
       `other Transport %` =  round((`total OtherSpecified Transport Means` /`total Respondents`)*100,2),
        )

 enumStat_trade <- icbtdata %>%
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
 
 enumStats <- left_join(enumMeta,enumStat_respondent_flow, 
                        by = join_by(regionCode, RegionName, districtCode, districtName, townCity, 
                                     borderPostName, team_number, enumerator_name,enumerator_contact)
                        ) %>%
              left_join(enumStat_trade,
                        by = join_by(regionCode, RegionName, districtCode, districtName, townCity, 
                                     borderPostName, team_number, enumerator_name,enumerator_contact)
                        ) %>% ungroup()
 
 
 borderStats <-enumStats %>%

 
 