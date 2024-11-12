# cases <- get_interviews_for_questionnaire(
#   chunk_size=9900,
#   qnr_id="a686a8b6-b64b-4411-8b37-7bd5174fe720",
#   qnr_version= 2  # This should be an integer, not a string
# )
# 
# 
# cases <- get_interviews_for_questionnaire(
#   chunk_size=210,
#   qnr_id="49957160-c7e4-4af9-a658-b99111b9104d",
#   qnr_version= 2  # This should be an integer, not a string
# )
# 
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

# server authentication:
set_credentials(
  server = sqlSvr$server, workspace = sqlSvr$workspace, 
  user =sqlSvr$usr, password = sqlSvr$pswd
)

# get the serve various case versions
server_qnr <- susoapi::get_questionnaires() %>% 
  filter(title == "ICBT FIELD WORK") %>%
  distinct(title, .keep_all = T)

intvwr_status<- get_interview_stats(
  "da115dde4e47417fa8e5d95075f8b171"
)

new_users <- get_interviewers()
new_usersAssignments <- get_assignments()
saveRDS(new_usersAssignments, "server/new_users_assignments.RDS")
saveRDS(new_users, "server/users.RDS").

cases1 <- get_interviews(
  nodes = c("id", "key", "assignmentId", "identifyingData", "questionnaireId",
            "questionnaireVersion", "questionnaireVariable", "responsibleName", "responsibleId",
            "responsibleRole", "supervisorName", "status", "actionFlags", "wasCompleted",
            "notAnsweredCount", "errorsCount", "createdDate", "updateDateUtc",
            "receivedByInterviewerAtUtc", "interviewMode"),
  chunk_size = 100,
  server = Sys.getenv("SUSO_SERVER"),
  workspace = Sys.getenv("SUSO_WORKSPACE"),
  user = Sys.getenv("SUSO_USER"),
  password = Sys.getenv("SUSO_PASSWORD")
)

stats <- get_possible_interview_statuses()

get_interviews(
  nodes = c("id", "key", "assignmentId", "identifyingData", "questionnaireId",
            "questionnaireVersion", "questionnaireVariable", "responsibleName", "responsibleId",
            "responsibleRole", "supervisorName", "wasCompleted",
           "createdDate", "updateDateUtc",),
  # chunk_size = 100,
  server = Sys.getenv("SUSO_SERVER"),
  workspace = Sys.getenv("SUSO_WORKSPACE"),
  user = Sys.getenv("SUSO_USER"),
  password = Sys.getenv("SUSO_PASSWORD")
)

saveRDS(all_users, "server/users.RDS")

##to mannually rejet the errors

CaseReject <- icbt_dataset()[['icbt_error_dataset']] %>%
  filter(!is.na(responsibleId)) %>%
  # filter(enumerator_name == "Ibrahim Bukari") %>%
  arrange(regionCode, districtCode, borderPostName,interview_key, interview_id)%>%
  group_by( regionCode ,interview_key,responsibleId,interview_id) %>%
  # group_by(interview_key,responsibleId,interview_id) %>%
  distinct(errorCheck) %>%
  summarize(errorMessage = str_c(errorCheck, collapse = " ; ")) %>%
  ungroup()

#server cred login

sqlSvr <- readRDS("server/credentials/icbt_svr.rds")
set_credentials(
  server = sqlSvr$server, workspace = sqlSvr$workspace, 
  user =sqlSvr$usr, password = sqlSvr$pswd
)
server_qnr <- susoapi::get_questionnaires() %>% 
  filter(title == "ICBT MAIN-Field Practice") %>%
  # filter(title == sqlSvr$title) %>%
  # dplyr::pull(questionnaireId)
  # filter(version==max(version))
  filter(version==1)

server_qnr_id <- server_qnr %>%
  # filter(version==6) %>%
  dplyr::pull(id)



##send cases back
CaseReject <- readRDS('C:/2024ICBT/Data_final/error_data.RDS') %>%
  mutate(
    enumerator_name = str_squish(trimws(trim(str_to_lower(enumerator_name))))
  )

# try Enumerator sandow wansuba jacob
UsersCheck <- new_users %>% mutate(
  FullName = str_squish(trimws(trim(str_to_lower(FullName))))
)

testResjectEnumCase <- CaseReject %>%
  filter(str_detect(str_to_lower(enumerator_name), "sandow wansuba"))

testIntervierviwer <- UsersCheck  %>%
  filter(str_to_lower(UserName)=='f15092')
  # filter(str_detect(str_to_lower(FullName), "sandow wansuba"))

for (i in 1:nrow(CaseReject)  ) {
  row <- CaseReject[i,]  #filter that row
  # print(row$interview_key)
  #send the case back as rejected
  #Rejecting a case as hq
  
  # print(row$interview_id)
  # print(row$responsibleId)
  
  # get_interview_stats(
  #   interview_id=row$interview_id
  # )
  
  # reject_interview_as_hq(
  #     interview_id=row$interview_id,
  #     comment = "",
  #     responsible_id = row$responsibleId,
  #     # verbose = TRUE
  #   )
  
  rejections<- reject_interview_as_hq(
    interview_id=row$interview_id,
    comment = row$errorMessage,
    responsible_id = row$responsibleId,
    verbose = TRUE
  )
}


# check for Ibrahim Bukari 
errorDataFile <- paste('Data_final/','error_data.RDS' , sep = '')
errorData <- readRDS(errorDataFile)

ibrahimStatus <- errorData %>%
  filter(enumerator_name=="Ibrahim Bukari")
