# enumErrsMeta <- icbt_dataset()[['icbt_error_dataset']] %>%
#   select(regionCode, RegionName, districtCode, districtName, townCity, borderPostName,team_number,
#             enumerator_name, enumerator_contact ,interview_key
#          ) %>%
#   distinct(regionCode, RegionName, districtCode, districtName, townCity, borderPostName,team_number,
#            enumerator_name, enumerator_contact,interview_key
#   )
#   group_by(
#    team_number,
#     enumerator_name, enumerator_contact , # districtCode, borderPostName,
#   )%>% 
#   summarise(
#     errorCasesCount = n_distinct(interview_key),
#     ErrorsCount = n()
#   )




enumMeta <- icbt_dataset()[['icbt_dataset_final']] %>% filter(month=="November") %>%  select(regionCode, RegionName, districtCode, districtName, townCity, borderPostName,team_number,
                                  enumerator_name, enumerator_contact ,) %>%
  distinct(regionCode, RegionName, districtCode, districtName, townCity, borderPostName,team_number,
           enumerator_name, enumerator_contact
  )

enumStat_respondent_flow<- icbt_dataset()[['icbt_dataset_final']]  %>% filter(month=="November")  %>% 
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
  )

enumStat_trade <- icbt_dataset()[['icbt_dataset_final']]  %>% filter(month=="November")  %>%
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
  )