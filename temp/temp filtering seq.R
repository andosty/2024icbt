user_out_email <- 'terryarthur35@gmail.com'
# user_out_email <- 'andosty@gmail.com'
# user_out_email <- 'alexboateng160@gmail.com'

library(readxl)
user_assigned_data <- read_excel("c:/2024icbt/server/loginDataAccess.xlsx") %>% 
  filter( 
      email ==  'terryarthur35@gmail.com'
      # email ==  signed_in_user_df() 
      )

dataRegCheck = data.frame()

for (i in nrow(user_out_data)) {
  row <- user_out_data[i,]
  
  filteredDataset <-  icbt_data %>%
    filter(
      regionCode %in% (row$startRegionCode:row$endRegionCode)
    ) # %>%
    # filter(
    # parse_number(team_number)>=row$startTeamNumber &   parse_number(team_number)<=row$endTeamNumber
    #   ) %>%
    # arrange(RegionName, districtName, townCity, borderPostName, team_number, enumerator_name ) %>%
    # select(RegionName,team_number) %>% unique(RegionName,team_number)
  
  dataRegCheck <- dplyr::bind_rows(dataRegCheck, filteredDataset)
  
}

table(icbt_data$RegionName) 

## new  ANDY
dataRegCheck = data.frame()
for(i in 1:nrow(user_out_data)) {
  row <- user_out_data[i,]
  
  filteredDataset <-  icbt_data %>%
    filter(
      regionCode %in% (row$startRegionCode:row$endRegionCode)
    ) # %>%
    # filter(
    #   parse_number(team_number)>=row$startTeamNumber &   parse_number(team_number)<=row$endTeamNumber
    # )
    # 
  dataRegCheck <- dplyr::bind_rows(dataRegCheck, filteredDataset)
  
  
  print(row$startRegionCode)
}

table(dataRegCheck$RegionName) 

## end new ANDY


for (item in user_out_data){
  print(item$email)
}

dataRegCheck <- icbt_data %>%
  filter(
    regionCode %in% sapply(list, function) (user_out_data$startRegionCode:user_out_data$endRegionCode)
  ) %>%
  # subset( 
  #   regionCode >= user_out_data$startRegionCode & regionCode <= user_out_data$endRegionCode,
  #         # parse_number(team_number)>=user_out_data$startTeamNumber &   parse_number(team_number)<=user_out_data$endTeamNumber 
  #   ) %>%
  arrange(RegionName, districtName, townCity, borderPostName, team_number, enumerator_name ) %>%
  select(RegionName) %>% unique()

table(dataRegCheck)

range(1:5)

apply(user_out_data,user_out_data$startRegionCode:user_out_data$endRegionCode)


trout <- downloaded_icbt_data %>% filter(str_to_lower(RegionName)=='north east')
table(trout$team_number )


final_error_toLoad <- paste('Data_final/','error_data.RDS' , sep = '')

errdata <- readRDS(final_error_toLoad)

icbt_errors <- data.frame()
for(i in 1:nrow(user_assigned_data)) {
  row <- user_assigned_data[i,]
  filteredDataset <-  errdata %>%
    filter(
      regionCode %in% (row$startRegionCode:row$endRegionCode)
    )  %>%
    filter(
      parse_number(team_number)>=row$startTeamNumber &   parse_number(team_number)<=row$endTeamNumber
    ) %>%
    arrange(RegionName, districtName, townCity, borderPostName, team_number, enumerator_name )
  
  icbt_errors <- dplyr::bind_rows(icbt_errors, filteredDataset)
  rm(filteredDataset)
}




icbt_errors <- data.frame()
for(i in 1:nrow(user_assigned_data)) {
  row <- user_assigned_data[i,]
  filteredErrorDataset <-  errdata %>%
    filter(
      regionCode %in% (row$startRegionCode:row$endRegionCode)
    )  %>%
    filter(
      parse_number(team_number)>=row$startTeamNumber &   parse_number(team_number)<=row$endTeamNumber
    ) %>%
    arrange(RegionName, districtName, townCity, borderPostName, team_number, enumerator_name )
  
  icbt_data <- dplyr::bind_rows(icbt_data, filteredErrorDataset)
  rm(filteredErrorDataset,row)
}


nrow( errdata %>% distinct(interview_key, interview_id))
