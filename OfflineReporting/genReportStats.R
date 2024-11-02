#load packages
setwd("C:/2024ICBT/")
source("OfflineReporting/Scripts/01Loadpackages.R")
source("OfflineReporting/Scripts/02LoadDataset.R")
# source("OfflineReporting/Scripts/03genMonitorReportStats.R")

#get regional meta frame
RegionMeta<- icbt_data %>% distinct( regionCode, RegionName)
DistrictMeta<- icbt_data %>% distinct( regionCode, RegionName, districtName) #districtCode
BorderMeta<- icbt_data %>% distinct( regionCode, RegionName, districtName, borderPostName ) # districtCode %>%

#summary at border
####################
source("OfflineReporting/Scripts/04BorderSummary.R")
# source("OfflineReporting/Scripts/05DistrictSummary.R")
source("OfflineReporting/Scripts/06RegionalSummary.R")
source("OfflineReporting/Scripts/07TradingPartnersSummary.R")

# if the value or valume is zero
# it means, interviewe selected other specified and left it blank or zero


#summary at district
#######################

#summary at regions
#######################
