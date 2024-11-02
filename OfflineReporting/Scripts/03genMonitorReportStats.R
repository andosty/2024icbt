#get regional meta frame
RegionMeta<- icbt_data %>% distinct( regionCode, RegionName)
DistrictMeta<- icbt_data %>% distinct( regionCode, RegionName, districtName) #districtCode
BorderMeta<- icbt_data %>% distinct( regionCode, RegionName, districtName,townCity, borderPostName ) # districtCode %>%

#summary at border
####################


#summary at district
#######################


#summary at regions
#######################



