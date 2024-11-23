
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


errordata <- readRDS("C:/2024ICBT/Data_final/error_data.RDS") # %>% filter(enumerator_name=="ABUBAKAR AMINATU" ) %>% distinct(responsibleId,interview_id,interview_key, .keep_all = )
finalfails <- readRDS("C:/2024ICBT/Data_final/finalFailedRejections.RDS") %>% distinct(responsibleId,interview_id,interview_key, .keep_all = T) # %>% filter(interview_id== errordata$interview_id)

# errordata <- readRDS("C:/2024ICBT/Data_final/error_data.RDS") %>% filter(enumerator_name=="PATIENCE GYABENG")
# finalfails <- readRDS("C:/2024ICBT/Data_final/finalFailedRejections.RDS") %>%
#  filter(responsibleId==errordata$responsibleId)  %>% 
#   distinct(responsibleId,interview_id,interview_key, .keep_all = T)
# userInterviews <- get_interviewers()

failedServerRejects = data.frame()
failedRejectedCases = data.frame()
sucessInterviewerServerRejects = data.frame()
sucessfullyRejectedCases = data.frame()

for(i in 1:nrow(finalfails) ){
  newRejection <- finalfails[i,]
  finalfails <- finalfails[-i,] #remove that row from finalfails dataset
  # newRejection <- finalfails[1,]
  
  rejectedErrorData <- errordata %>% filter(interview_key==newRejection$interview_key)
  errordata <- errordata %>% filter(!(interview_key==newRejection$interview_key)) #remove that error from error dataset
  # intervierUserData <- userInterviews %>% filter(str_squish(trim(str_to_lower(FullName))) == str_squish(trim(str_to_lower(rejectedErrorData$enumerator_name))))
  
  intvwr_status<- get_interview_stats(
    newRejection$interview_id
  )
  
  # get_tablet_activity <- get_user_action_log(
  #   user_id = intvwr_status$ResponsibleId,
  #   start = "2024-01-01",
  #   end = "2025-12-31"
  # )
  
  interviewer_assignment <- get_assignments(
    responsible = intvwr_status$ResponsibleId,
  ) 
  
  for (i in 1:nrow(interviewer_assignment)) {
    rowAssingnment <- interviewer_assignment[i,]
    assignment_id <-  rowAssingnment %>% pull(Id)
    
    print("pull row assignment")
    
    # Get details for a single assignment
    ass_Details<- get_assignment_details(
      id=assignment_id
      # server = Sys.getenv("SUSO_SERVER"),
      # workspace = Sys.getenv("SUSO_WORKSPACE"),
      # user = Sys.getenv("SUSO_USER"),
      # password = Sys.getenv("SUSO_PASSWORD")
    )
    print("to get assingment details")
    
    UserDetails <- get_user_details(user_id= intvwr_status$ResponsibleId)
    
    print("get the error message")
    getCaseIntvwrID <- errordata %>% filter(responsibleId==intvwr_status$ResponsibleId)
    # intvwid = '951d490441c94134876d07387723887e'
    # key='87-69-80-40'
    # assinmentID = '7156'
    # userID = 'f01027'
    
    print("now to reject the case")
    if(reject_interview_as_hq(
      interview_id= newRejection$interview_id,
      comment = "check and correct your errors",
      responsible_id = ass_Details$ResponsibleId,
      verbose = TRUE
    )==F){
      failedServerRejects <- rbind(failedServerRejects,ass_Details)
      failedRejectedCases <- rbind(failedRejectedCases,rejectedErrorData)
    } else {
      sucessInterviewerServerRejects <- rbind(sucessInterviewerServerRejects,ass_Details) 
      sucessfullyRejectedCases <- rbind(sucessfullyRejectedCases,rejectedErrorData) 
    }
    
  }
  }


# ebnd here
failedServerRejects <- failedServerRejects %>% 
  distinct(team_number,enumerator_contact,enumerator_name,ResponsibleId, .keep_all = T) %>%
  select( -any_of(c("Password","WebMode","IsAudioRecordingEnabled","Email","Quantity","Archived")))%>%
  rename(
    regionCode =id02a,
    regionName = id02,
    districtCode=id03b,
    districtName=id03,
    borderName = id06,
    townCity = id04
  ) %>% select(
    regionCode, regionName,districtCode,districtName, townCity,borderName, everything()
  )
write.xlsx(failedRejectedCases, "rejectionfeedbacks/failedServerRejects.xlsx")
saveRDS(failedRejectedCases, "rejectionfeedbacks/failedRejectedCases.RDS")
saveRDS(failedServerRejects, "rejectionfeedbacks/failedServerRejects.RDS")

sucessInterviewerServerRejects <-sucessInterviewerServerRejects %>% distinct(team_number,enumerator_name,enumerator_contact, .keep_all = T) %>%
  select( -any_of(c("Password","WebMode","IsAudioRecordingEnabled","Email","Quantity","Archived")))%>%
  rename(
    regionCode =id02a,
    regionName = id02,
    districtCode=id03b,
    districtName=id03,
    borderName = id06,
    townCity = id04
  ) %>% select(
    regionCode, regionName,districtCode,districtName, townCity,borderName, everything()
  ) %>% arrange(egionCode, regionName,districtCode,districtName, townCity,borderName,enumerator_name)
write.xlsx(sucessInterviewerServerRejects, "rejectionfeedbacks/enumShouldResync.xlsx")

# New Users<- readRDS("C:/2024ICBT/server/users.RDS")