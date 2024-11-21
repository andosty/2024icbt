setwd("C:/2024ICBT/")

#load packages
library(dplyr)

library(lubridate)
library(stringi)
library(stringr)
library(susoapi)
library(susoflows)
library(tidyr)
library(tidytext)
library(tidyverse)
library(haven)
library(readxl)
library(gdata)
sqlSvr <- readRDS("server/credentials/icbt_svr.rds")

#create directory for saving data download if not exist
data_dir <- "Data_Download/"
pricingDownload_dir <- paste(data_dir,'Pricing_download/',sep='')
pricing_extracted_dir <- paste(data_dir,'pricing_extracted/',sep='')

#delete pricingDownload_dir directory if it already exist
ifelse(dir.exists(file.path(pricingDownload_dir)),
       unlink(pricingDownload_dir, recursive = TRUE),
       "Data Directory Exists")

#create Data directory if it does not already exist
ifelse(!dir.exists(file.path(data_dir)),
       dir.create(file.path(data_dir)),
       "Data Directory Exists")

#create pricingDownload_dir Director in Data folder if it does not already exist
ifelse(!dir.exists(file.path(pricingDownload_dir)),
       dir.create(file.path(pricingDownload_dir)),
       "pricingDownload_dir Directory Exists")

#create pricing_extracted_dir Director in Data folder if it does not already exist
ifelse(!dir.exists(file.path(pricing_extracted_dir)),
       dir.create(file.path(pricing_extracted_dir)),
       "pricing_extracted_dir Directory Exists")

# server authentication:
set_credentials(
  server = sqlSvr$server, workspace = sqlSvr$workspace, 
  user =sqlSvr$usr, password = sqlSvr$pswd
)

# get the serve various case versions
server_qnr <- susoapi::get_questionnaires() %>% 
  filter(title == "Supervisor Pricing -FIELD WORK") %>%
  filter(version ==max(version))
  # distinct(title, .keep_all = T) %>%

download_matching(
  matches = server_qnr$title, 
  export_type = "STATA",
  path = pricingDownload_dir,
  include_meta = TRUE ,
  interview_status = "All",
  # c("All", "SupervisorAssigned", "InterviewerAssigned", "Completed", "RejectedBySupervisor", "ApprovedBySupervisor", "RejectedByHeadquarters", "ApprovedByHeadquarters")
  # from = "",
  # to = "",
  # version = server_qnr$version
  # qnr_id= server_qnr_id
)

pricingDataset = data.frame()
#unzip each version of the files
zipppedFiles <- list.files(path = pricingDownload_dir, pattern = "*.zip", full.names = T)

# zipfile<-zipppedFiles[2]y
for (zipfile in zipppedFiles) {
  #take each zip file and extract
  if (file.exists(zipfile)) {
    unzip( zipfile, exdir=pricing_extracted_dir) 
  }
  #take the dataset and process it
  #get user ids and merge with responsible id
  SupPriceMeta <- read_dta(paste(pricing_extracted_dir,"supervisor_pricicing_field_work.dta",sep = ''))
  if (nrow(SupPriceMeta)>0){
  SupPriceMeta <- SupPriceMeta %>% select(
    interview__key,interview__id,enumerator_name,enumerator_contact,team_number ,
    price_selector,id02,id02a,id03,id03b ,
    id04,quarter,month_of_collection,id06,gps__Latitude ,
    gps__Longitude,gps__Accuracy,gps__Altitude,gps__Timestamp 
  ) %>% 
    dplyr::mutate(  #bring in stata factor labels  
      id02a = haven::as_factor(id02a),
      id03b = haven::as_factor(id03b),
      team_number= parse_number(team_number)
    # id04 = haven::as_factor(id04),
    # id06 = haven::as_factor(id06)
    ) %>% rename(
      borderPostName = id06,
      townity = id04,
      districtCode = id03b,
      districtName = id03,
      regionCode = id02a,
      regionName = id02,
      interview_key=interview__key,
      interview_id=interview__id,
    ) %>%
    right_join(
      read_dta(paste(pricing_extracted_dir,"price.dta",sep = '')) %>%
        mutate(
          price__id = haven::as_factor(price__id),
          s1q3 = haven::as_factor(s1q3),
        ) %>%
        rename(
          product_with_UnitMeasure=price__id,
          currency=s1q3,
          price= s1q2,
          interview_key=interview__key,
          interview_id=interview__id,
               ),
      by = c("interview_key", "interview_id")
    )
  
  # pricingDataset <- rbind(pricingDataset,SupPriceMeta)
  pricingDataset <- dplyr::bind_rows(pricingDataset, SupPriceMeta)
  rm(SupPriceMeta)
  }
  
}
  
  ######################
# save final pricing dataset
saveRDS(pricingDataset, "C:/2024ICBT/Data_final/icbt_pricing_data.RDS")

pricingTeamMonitorReport <- pricingDataset %>%
  group_by(regionCode, regionName, districtCode, districtName, team_number) %>%
  summarise(
    uniquePricedItems = n_distinct(product_with_UnitMeasure)
  ) %>% arrange(districtCode, team_number)

PricingMeta <- PricingProductsList_OCT_2024 <- read_excel("pricing/PricingProductsList OCT 2024.xlsx") %>% 
  select(-team_number) %>%
  rename(team_number=team_id) %>%
  group_by(team_number) %>%
  summarise(
    ExpectedPricedItems = n_distinct(unique_product)
  ) 

pricingTeamStats <- merge(PricingMeta, pricingTeamMonitorReport)
## Monitor report for Pricing Data
