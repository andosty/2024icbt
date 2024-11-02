#first  pick the district codes file
icbt_data<- readRDS('Data_final/icbt_data.RDS')  %>%  
  mutate(
    #fix district names
    districtName = case_when( 
                            str_to_lower(districtName)=="bia west" ~ "Bia-West" ,
                            str_to_lower(districtName)=="bawku" ~ "Bawku Municipal" ,
                            TRUE ~ districtName
                            ),
     #conform region Names 
        RegionName= str_squish(trim(str_to_title(RegionName))),
     #conform border Names names
        borderPostName= str_remove(str_to_lower(borderPostName),'border post'),
        borderPostName= str_remove(str_to_lower(borderPostName),'boader'),
        borderPostName= str_remove(str_to_lower(borderPostName),'border'),
     #fix volter border names, remove the V1... causes same border name to duplicate
        borderPostName =  case_when(regionCode == 4 & str_detect(borderPostName," - ") ~ gsub(" - .*","",borderPostName),
                                    TRUE ~ borderPostName
                                    ),
    
        borderPostName= str_squish(trim(trimws(str_to_title(borderPostName)))),
        )

