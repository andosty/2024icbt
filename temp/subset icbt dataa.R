

trial <- icbt_data %>%
  subset()

user_assigned_data <- loginDataAccess %>% filter(email=="andosty@gmail.com")
icbt_data <- data.frame()
for(i in 1:nrow(user_assigned_data)) {
  row <- user_assigned_data[i,]
  filteredDataset <-  downloaded_icbt_data  %>%
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
  
  icbt_data <- dplyr::bind_rows(icbt_data, filteredDataset)
  rm(filteredDataset,row)
}

region <- icbt_data %>% distinct(RegionName)
teams <- icbt_data %>% distinct(team_number)