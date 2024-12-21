# library(async)
# library(future)
# plan(multisession)
# plan(multiprocess(workers=2))

## must install SUSO API
# devtools::install_github("michael-cw/SurveySolutionsAPI", build_vignettes = T)
rm(list=ls())  
setwd("C:/2024ICBT/")


#create directory for saving data download if not exist
data_dir <- "Data_Download/"
hqDownload_dir <- paste(data_dir,'HQ_download/',sep='')


#load packages
library(dplyr)
library(SurveySolutionsAPI)

library(susoapi)
library(susoflows)
suso_clear_keys()

sqlSvr <- readRDS("server/credentials/icbt_svr.rds")

set_credentials(
  server = sqlSvr$server, workspace = sqlSvr$workspace,
  user =sqlSvr$usr, password = sqlSvr$pswd
)


suso_set_key(sqlSvr$server,sqlSvr$usr, sqlSvr$pswd)
# suso_set_key("https://xxx.mysurvey.solutions", "xxxxxx", "xxxxxxx")
suso_keys()

questlist <- suso_getQuestDetails(workspace = sqlSvr$workspace) %>%
  filter(Title == "ICBT FIELD WORK") %>% mutate(
    Queue_job_id = NA_integer_
  )

#STEP 1: create export queues first  and add Queue NO# to dataFrame,
#STEP 2: then wait for 3mins for Export Generation to be Completed
#STEP 3: then start the download process

#STEP 1: 
for (i in 1:nrow(questlist)){
  icbtQues <- questlist [i,]
  # # start export process; get job ID
  if( started_job_id <-susoapi::start_export(
      qnr_id= icbtQues$QuestionnaireIdentity,
      export_type = "STATA",
      include_meta = TRUE,
      interview_status = "All" ) 
      ){
    questlist <-questlist %>% mutate(
      Queue_job_id = case_when(
        questlist$QuestionnaireIdentity == icbtQues$QuestionnaireIdentity ~ started_job_id,
        TRUE ~ Queue_job_id)
    )
  }
}
  
#STEP 2: 

# function to download the dataset
dataDownload_function <- function(que_id){
  tryCatch(
    {
      get_export_file(
        job_id = que_id,
        path = hqDownload_dir,
      )
    },
    #if an error occurs, tell me the error
    error=function(e) {
      message('A Download Error Occurred')
      print(e)
    },
    #if a warning occurs, tell me the warning
    warning=function(w) {
      message('Warning, Download Error Occurred')
      print(w)
      # return(NA)
    }
  )
}

#wait for 1.5 minutes
Sys.sleep(90)  # Pause for number is a second in each iteration

#check if Queue Process is COmpleted,
for (i in 1:nrow(questlist)){
  icbtQues <- questlist [i,]
  # # CHECK EXPORT JOB PROGESS, UNTIL COMPLETE, specifying ID of job started in prior step
  exportFeedback <- get_export_job_details(job_id = started_job_id) 
  
  if(exportFeedback$ExportStatus =='Completed' & exportFeedback$HasExportFile == TRUE ){
    que_id = icbtQues$Queue_job_id
    dataDownload_function(que_id)
    rm(que_id, exportFeedback)
  } else{
    #wait for 1 minute and try again
    Sys.sleep(60) # wait for 1 minute, to try again 
    exportFeedback <- get_export_job_details(job_id = started_job_id) 
    que_id = icbtQues$Queue_job_id
    dataDownload_function(que_id)
    rm(que_id, exportFeedback)
  }
}


##### END #####


HasExportFile == TRUE          


questionnaire <- suso_getQuestDetails(operation.type = "structure",
                                      workspace = sqlSvr$workspace, 
                                      quid = questlist[1, QuestionnaireId], 
                                      version = questlist[1, Version]
                                      )

interviewsQuestionnaires <- suso_getQuestDetails(
  operation.type = "interviews",
  workspace = sqlSvr$workspace, 
  quid = questlist[1, QuestionnaireId], 
  version = questlist[1, Version]
  )

interviews <- interviewsQuestionnaires[, .(InterviewId, AssignmentId, ResponsibleId, ErrorsCount, Status)]

library(tm)
newIntvwrID <- interviewsQuestionnaires %>% 
  mutate(
    newID = removePunctuation(InterviewId)
  )

IntData <- suso_getAllAnswerInterview(
  workspace = sqlSvr$workspace, 
  intID = "fc18a399-1b0d-4f94-b86f-7d0db017a67b"
)


# server = sqlSvr$server, workspace = sqlSvr$workspace,
# user =sqlSvr$usr, password =  sqlSvr$pswd


fullData<-  suso_export(
                        server = suso_get_api_key(sqlSvr$server),
                        apiUser = suso_get_api_key(sqlSvr$usr),
                        apiPass = suso_get_api_key( sqlSvr$pswd),
                        workspace = sqlSvr$workspace,  
                        token = NULL,
                        # questID = questlist[1, QuestionnaireId],
                        questID = "49957160-c7e4-4af9-a658-b99111b9104d",
                        version = 1,
                        workStatus = "Completed",
                        # version = questlist[1, Version],
                        # workStatus = NULL,
                        reloadTimeDiff = 1,
                        inShinyApp = F,
                        n_id_vars = 11
                      )

qvData <- suso_export(
  # server = suso_get_api_key("susoServer"),
  # apiUser = suso_get_api_key("susoUser"),
  # apiPass = suso_get_api_key("susoPass"),
  # workspace = NULL,
  # token = NULL,
  questID = questlist[1, QuestionnaireId],
  version = 1,
  workStatus = "Completed",
  reloadTimeDiff = 1,
  inShinyApp = F,
  n_id_vars = 11
)

 
fullData<-  suso_export(
  questID = questlist[1, QuestionnaireId], 
  version = questlist[1, Version], 
  # version = 1,
  workStatus = "Completed",
  reloadTimeDiff = 1,
  inShinyApp = F,
  # n_id_vars = 11
)
  



  questID = questlist[1, QuestionnaireId], 
                        workspace = sqlSvr$workspace,  
                        version = questlist[1, Version], 
                        reloadTimeDiff = 24, 
                        onlyActiveEvents = F, allResponses = T)
              )


stat <- suso_get_stats_interview(
  workspace = sqlSvr$workspace,
  intID = "f77e0681-4fcb-4b09-b638-5da17b827329"
)

stat1 <- suso_get_stats_interview(
  workspace = sqlSvr$workspace,
  intID = "592423b19c864fd4a714eeb3b8c0c9fb"
)


stat33 <-suso_get_assignments(
  questID = "49957160c7e44af9a658b99111b9104d$1",,
  # server = suso_get_api_key("susoServer"),
  # apiUser = suso_get_api_key("susoUser"),
  # apiPass = suso_get_api_key("susoPass"),
  # token = NULL,
  workspace = sqlSvr$workspace,
  # AssId = NULL,
  # version = NULL,
  # responsibleID = NULL,
  # order.by = "",
  # operations.type = c("assignmentQuantitySettings", "history", "recordAudio")
) 

stat44 <- suso_getAllInterviewQuestionnaire(
  # server = suso_get_api_key("susoServer"),
  # apiUser = suso_get_api_key("susoUser"),
  # apiPass = suso_get_api_key("susoPass"),
  workspace = sqlSvr$workspace,
  # token = NULL,
  questID =  "49957160c7e44af9a658b99111b9104d$1",
  version = 1,
  workStatus = "Completed"
)
  
newIntvwrID44 <- stat44 %>% 
  mutate(
    newID = removePunctuation(InterviewId)
  )


getInvterviw <- get_interview_stats(
  interview_id= "f279abd6013744ec83f1665dc3b272e4"
)

getAssignmentInfo <- get_assignment_details(
  id = getInvterviw$AssignmentId
)

paraData <- suso_export_paradata(
              # server = suso_get_api_key("susoServer"),
              # apiUser = suso_get_api_key("susoUser"),
              # apiPass = suso_get_api_key("susoPass"),
              workspace = sqlSvr$workspace,
              token = NULL,
              questID =  "49957160c7e44af9a658b99111b9104d",
              version = 1,
              workStatus = "Completed",
              reloadTimeDiff = 1,
              inShinyApp = FALSE,
              multiCore = NULL,
              onlyActiveEvents = FALSE,
              allResponses = TRUE,
              gpsVarName = NA,
              verbose = T,
              showProgress = F
            )


getDD <- suso_getQuestionsQuestionnaire(
  # server = suso_get_api_key("susoServer"),
  # apiUser = suso_get_api_key("susoUser"),
  # apiPass = suso_get_api_key("susoPass"),
  workspace = sqlSvr$workspace,
  token = NULL,
  questID =  "49957160c7e44af9a658b99111b9104d",
  version = 1,
)

getDDA <- suso_getQuestDetails(
  # server = suso_get_api_key("susoServer"),
  # usr = suso_get_api_key("susoUser"),
  # pass = suso_get_api_key("susoPass"),
  workspace = sqlSvr$workspace,
  token = NULL,
  quid  =  "49957160c7e44af9a658b99111b9104d",
  version = 1,
  operation.type = c("list", "statuses", "structure", "interviews")
)

library(tm)
getDDB <- suso_getQuestDetails(
  # server = suso_get_api_key("susoServer"),
  # usr = suso_get_api_key("susoUser"),
  # pass = suso_get_api_key("susoPass"),
  workspace = sqlSvr$workspace,
  token = NULL,
  quid  =  "49957160c7e44af9a658b99111b9104d",
  version = 1,
  operation.type = "interviews"
) %>% 
  mutate(
    newID = removePunctuation(InterviewId)
  )

library(nplyr)


slashed <- getDDB %>%
  unnest(FeaturedQuestions) %>%
  select(-Id)%>%
  group_by(
    InterviewId,newID, QuestionnaireId,QuestionnaireVersion,  
    AssignmentId,ResponsibleId, ResponsibleName,
    LastEntryDate  
    ) %>%
  summarise(
    
  )

library(dplyr)
library(dbplyr)
longDAta<- unpivot(slashed,
                   InterviewId,newID, QuestionnaireId,QuestionnaireVersion,  
                   AssignmentId,ResponsibleId, ResponsibleName,
                   LastEntryDate )




widerData <- 
  suso_getQuestDetails(
    # server = suso_get_api_key("susoServer"),
    # usr = suso_get_api_key("susoUser"),
    # pass = suso_get_api_key("susoPass"),
    workspace = sqlSvr$workspace,
    token = NULL,
    quid  =  "49957160c7e44af9a658b99111b9104d",
    version = 1,
    operation.type = "interviews"
  ) %>% 
  mutate(
    newID = removePunctuation(InterviewId)
  ) %>%
  unnest(FeaturedQuestions) %>%
  select(-Id)%>%
  pivot_wider(
    names_from = Question, 
    values_from = Answer 
  ) %>%
  select(
    c("InterviewId","QuestionnaireId","QuestionnaireVersion",    
      "AssignmentId","ResponsibleId","ResponsibleName",         
      "newID", "Enumerator Name","Enumerator Contact")
  ) %>%
  rename(
    enumerator_name = "Enumerator Name",
    enumerator_contact="Enumerator Contact"
  ) %>% 
  mutate(
    enumerator_name = str_squish(trim(trimws(str_to_title(enumerator_name))))
  )

