

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
library(openxlsx)


sqlSvr <- readRDS("server/credentials/icbt_svr.rds") 

# server authentication:
set_credentials(
  server = sqlSvr$server, workspace = sqlSvr$workspace, 
  user =sqlSvr$usr, password = sqlSvr$pswd
)

# get the serve various case versions
server_qnr <- susoapi::get_questionnaires() %>% 
  filter(title == "ICBT FIELD WORK") %>%
  distinct(title, .keep_all = T)


errordata <- readRDS("C:/2024ICBT/Data_final/error_data.RDS") # %>% filter(enumerator_name=="10-41-12-31" ) %>% distinct(responsibleId,interview_id,interview_key, .keep_all = )

toReject <- errordata %>% filter(str_to_title(enumerator_name)=="Kporvi Michael" ) %>% distinct(responsibleId,interview_id,interview_key, .keep_all = )

interviewerCase_status<- get_interview_stats(
  toReject$interview_id
)

get_tablet_activity <- get_user_action_log(
  user_id = interviewerCase_status$ResponsibleId,
  start =  as.character(today()-1),
  end = as.character(today()+1)
) %>% filter( Message !="Sync started (Online)")

reject_interview_as_hq(
  interview_id= interviewerCase_status$InterviewId,
  comment = "check and correct your errors",
  responsible_id = interviewerCase_status$ResponsibleId,
  verbose = TRUE
)






print("now to reject the case")
if(reject_interview_as_hq(
  interview_id= toReject$interview_id,
  comment = "check and correct your errors",
  responsible_id = toReject$responsibleId,
  verbose = TRUE
)==T){
  print("OKAY DONE")
} else {
  print("IT FAILED")
}

reject_interview_as_hq(
  interview_id= "8c644ffb2e6741719d67290c666cbdad",
  comment = "check and correct your errors",
  responsible_id = "396cba0b-6130-46a5-a818-bc7cfb4b2f7a",
  verbose = TRUE
)




KPORVI MICHAEL

enumUser <- users %>% filter(str_to_upper(FullName)=="KPORVI MICHAEL")
Oti
team 7
Full Name: Kporvi Michael 
District: KADJEBI
Border: Titiaka Outpost 
Case Key: 10-41-12-31



