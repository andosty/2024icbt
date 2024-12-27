rm(list=ls())
# setwd("C:/2024ICBT/") #windows
setwd("/home/administrator/myShinyApps/2024icbt/") #unix
#setwd("/srv/shiny-server/2024icbt/") #unix

  #ADDED packages
if(!require(tm)) install.packages("tm")
if(!require(SurveySolutionsAPI)) devtools::install_github("michael-cw/SurveySolutionsAPI", build_vignettes = T)
library(SurveySolutionsAPI)
library(tm)

# install.packages("devtools")
# devtools::install_github("arthur-shaw/susoapi")
# devtools::install_github("arthur-shaw/susoflows")
# remotes::install_gitlab("hrbrmstr/pluralize")


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
  hqDownload_dir <- paste(data_dir,'HQ_download/',sep='')
  hq_extracted_dir <- paste(data_dir,'HQ_extracted/',sep='')

  #delete Data directory if it already exist
  ifelse(dir.exists(file.path(data_dir)),
         unlink(data_dir, recursive = TRUE),
         "Data Directory Exists")

  #create Data directory if it does not already exist
  ifelse(!dir.exists(file.path(data_dir)),
         dir.create(file.path(data_dir)),
         "Data Directory Exists")

  #create HQ_download Director in Data folder if it does not already exist
  ifelse(!dir.exists(file.path(hqDownload_dir)),
         dir.create(file.path(hqDownload_dir)),
         "HQ_download Directory Exists")

  #create HQ_extracted Director in Data folder if it does not already exist
  ifelse(!dir.exists(file.path(hq_extracted_dir)),
         dir.create(file.path(hq_extracted_dir)),
         "HQ_extracted Directory Exists")

  # server authentication:
  set_credentials(
    server = sqlSvr$server, workspace = sqlSvr$workspace,
    user =sqlSvr$usr, password = sqlSvr$pswd
  )



  # newDataMeta <- select(read_rds("server/users.RDS"),UserId, UserName) %>%
  #   rename(responsibleId = UserId) %>% distinct(responsibleId,UserName, .keep_all = T)

  # # get the serve various case versions
  # server_qnr <- susoapi::get_questionnaires() %>%
  #   filter(title == "ICBT FIELD WORK") %>%
  #   distinct(title, .keep_all = T)
  #
  # # cases <- susoapi:::get_interviews_for_questionnaire(
  # #   chunk_size=100,
  # #   qnr_id=server_qnr$questionnaireId,
  # #   qnr_version= server_qnr$version,  # This should be an integer, not a string
  # #   #
  # #   server = Sys.getenv("SUSO_SERVER"),
  # #   workspace = Sys.getenv("SUSO_WORKSPACE"),
  # #   user = Sys.getenv("SUSO_USER"),
  # #   password = Sys.getenv("SUSO_PASSWORD")
  # # )
  # #
  # # count <- get_interviews_count()
  # # count <- interviews_request()
  # # myQ <- get_questionnaires()
  # # gitdata<-get_interviews_for_questionnaire(
  # #
  # #                                           # qnr_id="49957160-c7e4-4af9-a658-b99111b9104d",
  # #                                           qnr_id="49957160-c7e4-4af9-a658-b99111b9104d",
  # #                                           chunk_size = 100,
  # #                                           qnr_version=3,
  # #                                           server = Sys.getenv("SUSO_SERVER"),
  # #                                           workspace = Sys.getenv("SUSO_WORKSPACE"),
  # #                                           user = Sys.getenv("SUSO_USER"),
  # #                                           password = Sys.getenv("SUSO_PASSWORD")
  # #                                           )
  # #
  # # show_credentials()
  # # int_stat<- get_possible_interview_statuses()
  # #
  # # intv <- get_interviews(
  # #   nodes = c("id", "key", "assignmentId", "identifyingData", "questionnaireId",
  # #             "questionnaireVersion", "questionnaireVariable", "responsibleName", "responsibleId",
  # #             "responsibleRole", "supervisorName", "status", "actionFlags", "wasCompleted",
  # #             "notAnsweredCount", "errorsCount", "createdDate", "updateDateUtc",
  # #             "receivedByInterviewerAtUtc", "interviewMode"),
  # #   chunk_size = 100,
  # #   server = Sys.getenv("SUSO_SERVER"),
  # #   workspace = Sys.getenv("SUSO_WORKSPACE"),
  # #   user = Sys.getenv("SUSO_USER"),
  # #   password = Sys.getenv("SUSO_PASSWORD")
  # # )
  # # data0 <- get_interviews(
  # #   nodes = c("id", "key", "assignmentId", "identifyingData", "questionnaireId",
  # #             "questionnaireVersion", "questionnaireVariable", "responsibleName", "responsibleId",
  # #             "responsibleRole", "supervisorName", "status", "actionFlags", "wasCompleted",
  # #             "notAnsweredCount", "errorsCount", "createdDate", "updateDateUtc",
  # #             "receivedByInterviewerAtUtc", "interviewMode"),
  # #   chunk_size = 100,
  # #   server = Sys.getenv("SUSO_SERVER"),
  # #   workspace = Sys.getenv("SUSO_WORKSPACE"),
  # #   user = Sys.getenv("SUSO_USER"),
  # #   password = Sys.getenv("SUSO_PASSWORD")
  # # )
  #
  # # data1 <- get_interviewers()
  # # data2 <- get_interviews_for_questionnaire(
  # #   chunk_size = 100,
  # #   qnr_id="49957160-c7e4-4af9-a658-b99111b9104d",
  # #   qnr_version=1,
  # #   server = Sys.getenv("SUSO_SERVER"),
  # #   workspace = Sys.getenv("SUSO_WORKSPACE"),
  # #   user = Sys.getenv("SUSO_USER"),
  # #   password = Sys.getenv("SUSO_PASSWORD")
  # # )
  # # reGetUser <- get_user_details(user_id="")
  #
  #
  # # FreshUsersAssignments<- get_assignments()# %>% filter(QuestionnaireId=="49957160-c7e4-4af9-a658-b99111b9104d")
  # # FreshUsersAssignments<- new_usersAssignments %>% filter(QuestionnaireId=="49957160c7e44af9a658b99111b9104d$1")
  #
  # #date and time of cuurent last data download run
  #
  #
  # #set download started time
  # dataDownloadStarted <- format(now(), "%a %d %b %Y, %I:%M %p")
  #
  #
  # downloaded_icbt_data <- data.frame()
  #
  # download_matching(
  #   matches = server_qnr$title,
  #   export_type = "STATA",
  #   path = hqDownload_dir,
  #   # qnr_id= server_qnr_id
  # )
  #
  #
  # # manual version 4 files
  # ################################
  # # get Version 4 Export Qeue Job
  # # version_4_ExportJobs <-  get_export_jobs(
  # #   export_type = c("STATA"),
  # #   interview_status = "All",
  # #   qnr_id = "49957160c7e44af9a658b99111b9104d$4",
  # #   export_status = "Completed",
  # #   has_file = NA,
  # #   limit = 100,
  # #   offset = 0,
  # #   server = Sys.getenv("SUSO_SERVER"),
  # #   workspace = Sys.getenv("SUSO_WORKSPACE"),
  # #   user = Sys.getenv("SUSO_USER"),
  # #   password = Sys.getenv("SUSO_PASSWORD")
  # # )
  #
  # job_id_v4 = 2641
  # version4Qeue<- get_export_job_details(
  #   job_id = job_id_v4,
  # )
  # #Get and download the version 4 dataset
  # get_export_file(
  #   job_id=job_id_v4,
  #   path = hqDownload_dir
  # )
  #

  ##### NEW DOWNLOAD SCRIPT START #####

  suso_set_key(sqlSvr$server,sqlSvr$usr, sqlSvr$pswd)

  questlist <- suso_getQuestDetails(workspace = sqlSvr$workspace) %>%
    filter(Title == "ICBT FIELD WORK") %>% mutate(
      Queue_job_id = NA_integer_
    )

  dataDownloadStarted <- format(now(), "%a %d %b %Y, %I:%M %p")

  #STEP 1: create export queues first  and add Queue NO# to dataFrame,
  #STEP 2: then wait for some mins for Export Generation to be Completed
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



  #get Responsible IDS from server whiles waiting for export queue to complete
  #########################################
  icbt_metaData <- data.frame()

  for (i in 1:nrow(questlist)){
    # icbtQues <- questlist [3,]
    icbtQues <- questlist [i,]

    metaData_fetch <-  suso_getQuestDetails(
      workspace = sqlSvr$workspace,
      token = NULL,
      quid  =   icbtQues$QuestionnaireId,
      version = icbtQues$Version,
      operation.type = "interviews"
    ) %>%
      mutate(
        interview__id = removePunctuation(InterviewId)
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
          "interview__id", "Enumerator Name","Enumerator Contact","Month of Data Collection")
      ) %>%
      rename(
        enumerator_name = "Enumerator Name",
        enumerator_contact="Enumerator Contact",
        assignment_id = AssignmentId  ,
        responsibleId = ResponsibleId,
        qnr_createdDateTime = "Month of Data Collection"
      ) %>%
      mutate(
        enumerator_contact=as.character(paste0(enumerator_contact)),
        enumerator_name=as.character(paste0(enumerator_name)),
        qnr_createdDateTime=as.character(paste0(enumerator_name))
      )

    icbt_metaData <- dplyr::bind_rows(icbt_metaData, metaData_fetch)
    rm(metaData_fetch)
  }

  icbt_metaData <- icbt_metaData %>%
    mutate(
      enumerator_name = str_squish(trim(str_to_title(enumerator_name)))
    )

  #check if Queue Process is COmpleted,
  for (i in 1:nrow(questlist)){
    # icbtQues <- questlist [3,]
    icbtQues <- questlist [i,]
    # # CHECK EXPORT JOB PROGESS, UNTIL COMPLETE, specifying ID of job started in prior step
    exportFeedback <- get_export_job_details(job_id = started_job_id)

    if(exportFeedback$ExportStatus =='Completed' & exportFeedback$HasExportFile == TRUE ){
      que_id = icbtQues$Queue_job_id
      dataDownload_function(que_id)
      rm(que_id, exportFeedback)
    } else{
      #wait for 1 minute and try again
      Sys.sleep(120) # wait for 2 minute, to try again
      exportFeedback <- get_export_job_details(job_id = started_job_id)
      que_id = icbtQues$Queue_job_id
      dataDownload_function(que_id)
      rm(que_id, exportFeedback)
    }
  }
  ##### NEW DOWNLOAD SCRIPT END #####

  #unzip each version of the files
  zipppedFiles <- list.files(path = hqDownload_dir, pattern = "*.zip", full.names = T)

  # https://arthur-shaw.github.io/susoflows/
  # https://www.appsilon.com/post/forget-about-excel-use-r-shiny-packages-instead
  # https://stackoverflow.com/questions/35834322/how-to-save-edits-made-using-rhandsontable-r-package
  # take the database from firebase or fidesworcx
  #corrections dataframe would need “created_at”, “created_by”, “modified_at”
  #add download corrections dataframe files as r,stata and csv

  #future scripting task
  # split each work by month
  # and save each monthly data set with its errors
  # then, merge them as final file

  downloaded_icbt_data <- data.frame()


  # zipfile<-zipppedFiles[3]
  for (zipfile in zipppedFiles) {
    #take each zip file and extract
    if (file.exists(zipfile)) {
      unzip( zipfile, exdir=hq_extracted_dir)
    }
    #take the dataset and process it
    # #get user ids and merge with responsible id
    # userID_in_Data <-  read_dta(paste(hq_extracted_dir,"interview__actions.dta",sep = '')) %>% select(
    #   interview__key, interview__id, action, originator, date, time
    # ) %>%filter(action==12) %>% #filter case with created action, for the one who created the case
    #   rename(UserName =  originator,
    #          qnr_createdDate =date,
    #          qnr_createdTime =time
    #   )  %>%
    #   mutate(
    #     qnr_createdDateTime= paste(qnr_createdDate, 'T' , qnr_createdTime, sep = ''),
    #   ) %>%
    #   select(-c('action','qnr_createdDate','qnr_createdTime')) %>%
    #   distinct( UserName,interview__key,interview__id , .keep_all = T) %>%
    #   arrange(UserName, interview__key,interview__id )
    #
    # MajorMeta <- left_join(userID_in_Data,newDataMeta, by=c("UserName"))


    # take cases meta data
    library(haven)

    metaColNames <- c("interview__key", "interview__id","enumerator_name" ,"enumerator_contact", "team_number" ,
                      "id02","id02a","id03","id03b","id04",
                      "id06","gps__Latitude","gps__Longitude","gps__Accuracy","gps__Altitude",
                      "gps__Timestamp","assignment__id", "quarter" , 'date_and_time_collection',"quarter"
    )


    transpondentNames <- c("interview__key","interview__id","transportercharacteristics__id",
                           "s2q1","s2q2","s2q3",
                           "s2q3ll","s2q3llo","s2q10a","s2q10b"
    )

    #take the stata   files for processing
    icbt_data_version <- read_dta(paste(hq_extracted_dir,questlist$Variable[1],".dta", sep = ''))

    if (nrow(icbt_data_version)>0){
      icbt_data_version <- icbt_data_version %>%
        select(all_of(metaColNames),any_of(c("quarter","month","date_and_time_collection"))) %>%
        dplyr::mutate(  #bring in stata factor labels
          id02 = haven::as_factor(id02),
          id03 = haven::as_factor(id03),
          id04 = haven::as_factor(id04),
          id06 = haven::as_factor(id06)
        ) %>%
        left_join(
          read_dta(paste(hq_extracted_dir,"transportercharacteristics.dta",sep = '')) %>%
            select(all_of(transpondentNames)) %>%
            dplyr::mutate(  #bring in stata factor labels
              s2q2 = haven::as_factor(s2q2),
              s2q3 = haven::as_factor(s2q3),
              s2q10a = haven::as_factor(s2q10a),
              # s2q10b = haven::as_factor(s2q10b),
              s2q3ll = haven::as_factor(s2q3ll)
            )
          , by=c("interview__key"="interview__key", "interview__id" = "interview__id")
        ) %>%
        left_join(
          read_dta(paste(hq_extracted_dir,"Commodity.dta",sep = '')) %>%
            dplyr::mutate(  #bring in stata factor labels
              s2q4 = haven::as_factor(s2q4),
              s2b1 = haven::as_factor(s2b1),
              s2q5 = haven::as_factor(s2q5),
              s2l1 = haven::as_factor(s2l1),
              s2l2 = haven::as_factor(s2l2),
              s2q6oo = haven::as_factor(s2q6oo),

              # s2q10a = haven::as_factor(s2q10a)
            ),
          by=c("interview__key"="interview__key",
               "interview__id"="interview__id",
               "transportercharacteristics__id"="transportercharacteristics__id"
          )
        ) %>% rename(
          assignment_id = assignment__id
        ) %>%
        mutate(
          enumerator_name = str_squish(trim(trimws(str_to_title(enumerator_name)))),
          enumerator_contact=as.character(paste0(enumerator_contact))
        ) %>%
        left_join(icbt_metaData)

      downloaded_icbt_data <- dplyr::bind_rows(downloaded_icbt_data, icbt_data_version)
      rm(icbt_data_version)
      ## end zip loop
    }
  }

  if("s2q6ao" %in% names(downloaded_icbt_data) ){
    downloaded_icbt_data <- downloaded_icbt_data %>% mutate(

      s2q6ao = haven::as_factor(s2q6ao)
    )
  }

  #Pull HH level var renames file
  renameData <- read_excel("server/dictionary/varNames.xlsx",sheet = "icbtVarRenames")

  for (i in 1:nrow(renameData)  ) {
    row <- renameData[i,]  #filter that row
    if(row$oldVarName %in% names(downloaded_icbt_data)){ #if name var exists, then rename the var
      downloaded_icbt_data <- rename.vars(downloaded_icbt_data, from =row$oldVarName, to = row$newVarName)
    }
  }


  # rm(renameData, metaColNames, transpondentNames, row, server_qnr,sqlSvr)
  colnames(downloaded_icbt_data) = gsub("__", "_", colnames(downloaded_icbt_data))


  downloaded_icbt_data <- downloaded_icbt_data %>%
    filter(!is.na(transpondent_id)) %>%
    mutate(
      borderPostName=  str_remove_all(borderPostName, '"'),
      gps_Timestamp = format(ymd_hms(gps_Timestamp,tz=Sys.timezone()), "%Y-%m-%d %I:%M %p"),
      createdDate = format(ymd_hms(createdDate,tz=Sys.timezone()), "%Y-%m-%d %I:%M %p"),
      qnr_createdDateTime = format(ymd_hms(qnr_createdDateTime,tz=Sys.timezone()), "%Y-%m-%d %I:%M %p"),
      # gpsCapture_QnrCreated_dayGap= difftime(gps_Timestamp,qnr_createdDateTime, units = "days"), # gps_timestamp - createdDateTimeStamp
      # gpsCapture_QnrCreated_HourGap= difftime(gps_Timestamp,qnr_createdDateTime, units = "hours") # gps_timestamp - createdDateTimeStamp
    ) %>%
    mutate(  #fix wrong regionCode & regionName & team numbering assignments
      regionCode = case_when(
        regionCode==16 & str_to_lower(RegionName)=='savanna'~ 13, #fix savanna with wrong region codes
        str_to_lower(districtName)== 'bole' & regionCode==16 ~ 13,
        str_to_lower(RegionName)=='northern'~ 12, # fix northern region cases with wrong region codes
        TRUE ~ regionCode
      ),
      team_number = case_when(
        regionCode==1 & team_number=='WR Team 2' ~ 'WR 2',
        regionCode==13 & team_number=='UW Team 2' ~ 'Savannah Team 2',
        regionCode==13 & team_number=='UW Team 1' ~ 'Savannah Team 1',
        TRUE ~ team_number),
      RegionName = case_when(str_to_lower(RegionName)=='savanna'~ 'SAVANNAH',
                             TRUE ~ RegionName),
      RegionName= str_to_title(RegionName),
      districtName= str_to_title(districtName),
      borderPostName= str_replace_all(borderPostName,'"',''),
      borderPostName= str_to_title(gsub("/", "-", borderPostName) ),
      borderPostName =  gsub("Boader","Border", (borderPostName) ) ,
      borderPostName =  gsub("boader","Border", (borderPostName) ) ,
      borderPostName =  gsub("BOADER","Border", (borderPostName) )
    ) %>%
    arrange(RegionName, districtName, townCity, borderPostName, team_number, enumerator_name ) %>%
    mutate( #fix dataset issues

      #fix blank month and qtr
      month = case_when(
        is.na(month) ~ as.character(month(gps_Timestamp, label = TRUE, abbr = FALSE)),
        TRUE ~ month
      ),
      quarter = case_when(
        is.na(quarter) & str_to_lower(month)=="october" ~ as.character(1),
        TRUE ~ quarter
      ),

      team_number= parse_number(team_number),
      productObserved=  gsub("colanut", "cola nut", productObserved, ignore.case = TRUE) ,
      # fix for 'coming on' and 'going on', phrases to 'comin in on' and 'going out on'
      #afterwards, remove it from error check
      observedRespondentDescription = str_squish(trim(trimws(observedRespondentDescription))),
      observedRespondentDescription = case_when(
        str_detect(str_to_lower(observedRespondentDescription),'coming on') ~  gsub('coming on', 'coming in on', observedRespondentDescription, ignore.case = TRUE),
        str_detect(str_to_lower(observedRespondentDescription),'coning in') ~  gsub('coning in', 'coming in', observedRespondentDescription, ignore.case = TRUE),
        str_detect(str_to_lower(observedRespondentDescription),'entering in') ~  gsub('entering in', 'coming in', observedRespondentDescription, ignore.case = TRUE),
        str_detect(str_to_lower(observedRespondentDescription),'cominng in') ~  gsub('cominng in', 'coming in', observedRespondentDescription, ignore.case = TRUE),
        str_detect(str_to_lower(observedRespondentDescription),'coming with') ~  gsub('coming with', 'coming in on', observedRespondentDescription, ignore.case = TRUE),
        str_detect(str_to_lower(observedRespondentDescription),'going on') ~ gsub('going on', 'going out on', observedRespondentDescription, ignore.case = TRUE),
        str_detect(str_to_lower(observedRespondentDescription),'gong out') ~ gsub('gong out', 'going out', observedRespondentDescription, ignore.case = TRUE),
        str_detect(str_to_lower(observedRespondentDescription),'going with') ~ gsub('going with', 'going out with', observedRespondentDescription, ignore.case = TRUE),

        str_detect(str_to_lower(observedRespondentDescription),'motobike') ~ gsub('motobike', 'Motorbike', observedRespondentDescription, ignore.case = TRUE),

        TRUE ~ observedRespondentDescription
      ),
      #fixing tranport-mode spelling mistakes
      transportMeans= case_when(
        str_detect(str_to_lower(transportMeans),'motobike') ~ gsub('motobike', 'Motorbike', transportMeans, ignore.case = TRUE),
        TRUE ~ transportMeans

      ),


      #spelling fixes
      commodityObervedDescription=  gsub("under wear","underwear", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("under wears","underwear", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("okra","okro", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("alefo","alefu", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("water melon","watermelon", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub(" tea ","lipton (teabag)", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("local tea","lipton (teabag)", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("kitchen ware","Kitchenware", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("kitchen wares","Kitchenware", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("alcohol in gallons","akpeteshie", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("alcohol in gallon","akpeteshie", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("cyclinder","cylinder", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("barber machine","hair accessories", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("ayilo","ayilor", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("ladels","ladles", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("ladel","ladle", str_to_lower(commodityObervedDescription) ) ,

      commodityObervedDescription = ifelse(
        str_to_lower(as.character(month(gps_Timestamp, label = TRUE, abbr = FALSE)))=="october" ,
        gsub("pomade","body lotion", str_to_lower(commodityObervedDescription) ),
        commodityObervedDescription
      ),

      commodityObervedDescription= case_when(
        str_to_lower(month)=="october" ~  gsub("pomade","body lotion", str_to_lower(commodityObervedDescription) ),
                                              TRUE ~ commodityObervedDescription # correction for Month1 OCTOBER only, here month was blank
      ) ,


      # commodityObervedDescription=  gsub("pomade","body lotion", str_to_lower(commodityObervedDescription) ) ,
      # commodityObervedDescription=  gsub("body cream","body lotion", str_to_lower(commodityObervedDescription) ) ,
      # # commodityObervedDescription=  gsub("body lotion","pomade", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("busket","basket ", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("creates ","crates ", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("frying pan","saucepan", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("sacepan","saucepan", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("sauce pans","saucepan", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("sauce pan","saucepan", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("aucepans","saucepan", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("grandnuts","groundnuts", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("groudnut","groundnut", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("groundnu","groundnut", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("ground nut","groundnut", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("weedcide","weedicide", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("weedicid","weedicide", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("0ranges","orange", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("indoime","indomie ", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("morta","mortar", str_to_lower(commodityObervedDescription) ) ,
      # commodityObervedDescription=  gsub("body lotion (body cream)","pomade", str_to_lower(commodityObervedDescription) ) ,
      # commodityObervedDescription=  gsub("body cream","pomade", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("beens","beans", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("chacoal","charcoal", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("ofmcharcoal","of charcoal", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("door mats","doormats", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("door mat","doormat", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("dipper","diaper", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("dippers","diapers", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("akpeteshi","akpeteshie", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("akpeteshiee","akpeteshie", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("akpteshie","akpeteshie", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("akpetshie","akpeteshie", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("apketeshie","akpeteshie", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("akpeteshei","akpeteshie", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("apketshie","akpeteshie", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("akakpeteshie","akpeteshie", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("soup","soap", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("saop","soap", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("millo","milo", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("patrol","petrol", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("fire wood","firewood", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("five wood","firewood", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("frewood","firewood", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("sphaghetti","spaghetti", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("cucumba","cucumber", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("cucumbas","cucumbers", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("pastel","pestle", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("tiger nuts","tigernuts", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("tiger nut","tigernut", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("konkonte","kokonte", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("kokontee","kokonte", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("mortarr","mortar", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("grice","rice", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("pantain","plantain", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("weedicideec","weedicide", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("egg8","egg", str_to_lower(commodityObervedDescription) ) ,
      commodityObervedDescription=  gsub("ofonion","of onion", str_to_lower(commodityObervedDescription) ) ,
      # commodityObervedDescription=  gsub("toothpast","toothpaste", str_to_lower(commodityObervedDescription) ) ,

      #fix gps date issue but case is valid and within time frame
      gps_Timestamp = case_when(
                                  interview_key=="38-99-06-28" & interview_id=="9f106e6358c24d2aad54510a813dc4ce" ~ '2024-11-11T23:30:08.923919Z',
                                  interview_key=="59-82-19-59" & interview_id=="592423b19c864fd4a714eeb3b8c0c9fb" ~ "2024-11-11T23:58:02.661015Z",
                                  interview_key=="50-47-81-71" & interview_id=="6a2bc43595fd4df0b98b2088eca88dd3" ~ '2024-10-23T11:35:14.905758Z',
                                  interview_key=="19-97-73-94" & interview_id=="de6bb15e5e5e4e2ebb971a8270fa16be" ~ '2024-10-23T09:33:30.21676Z',
                                  interview_key=="27-97-38-25" & interview_id=="ce7ff989c68b412cb7e89c20c2186cb3" ~ '2024-11-11T00:13:37.768065Z',
                                  interview_key=="06-08-58-38" & interview_id=="69d39e8bd2ae404ebd244536a0d336e8" ~ '2024-10-22T12:26:38.595491Z',
                                  interview_key=="19-97-73-94" & interview_id=="de6bb15e5e5e4e2ebb971a8270fa16be" ~ '2024-10-23T09:33:30.21676Z',
                                  interview_key=="53-95-09-19" & interview_id=="290a572e712f40d98451f1835e8fbb54" ~ '2024-10-22T12:26:14.226129Z',
                                  interview_key=="48-15-77-88" & interview_id=="f279abd6013744ec83f1665dc3b272e4" ~ '2024-11-11T08:25:27.46638Z',
                                  interview_key=="11-26-03-51" & interview_id=="4f243553ab9045819bce277334e71b85" ~ as.character("2024-10-31T10:39:20"),
                                  interview_key=="97-03-54-45" & interview_id=="3d34684ea9d0411eb1cddaca3ac0f6b5" ~ as.character("2024-11-24T08:36:51"),
                                  interview_key=="94-61-35-06" & interview_id=="603103e8689b4cc3ad023ce05b0726ad" ~ as.character("2024-11-24T08:36:51"),
                                  interview_key=="39-71-25-23" & interview_id=="bf5f063f886747e0bea95284eaf20dc2" ~ '2024-12-09T07:51:54.830042Z',
                                  TRUE ~ gps_Timestamp
                                ),
      qnr_createdDateTime = case_when(
                                  interview_key=="38-99-06-28" & interview_id=="9f106e6358c24d2aad54510a813dc4ce" ~ gps_Timestamp,
                                  interview_key=="59-82-19-59" & interview_id=="592423b19c864fd4a714eeb3b8c0c9fb" ~ gps_Timestamp,
                                  interview_key=="50-47-81-71" & interview_id=="6a2bc43595fd4df0b98b2088eca88dd3" ~ gps_Timestamp,
                                  interview_key=="19-97-73-94" & interview_id=="de6bb15e5e5e4e2ebb971a8270fa16be" ~ gps_Timestamp,
                                  interview_key=="27-97-38-25" & interview_id=="ce7ff989c68b412cb7e89c20c2186cb3" ~ gps_Timestamp,
                                  interview_key=="06-08-58-38" & interview_id=="69d39e8bd2ae404ebd244536a0d336e8" ~ gps_Timestamp,
                                  interview_key=="19-97-73-94" & interview_id=="de6bb15e5e5e4e2ebb971a8270fa16be" ~ gps_Timestamp,
                                  interview_key=="48-15-77-88" & interview_id=="f279abd6013744ec83f1665dc3b272e4" ~ gps_Timestamp,
                                  interview_key=="53-95-09-19" & interview_id=="290a572e712f40d98451f1835e8fbb54" ~ gps_Timestamp,
                                  interview_key=="11-26-03-51" & interview_id=="4f243553ab9045819bce277334e71b85" ~ gps_Timestamp,
                                  interview_key=="97-03-54-45" & interview_id=="3d34684ea9d0411eb1cddaca3ac0f6b5" ~ gps_Timestamp,
                                  interview_key=="94-61-35-06" & interview_id=="603103e8689b4cc3ad023ce05b0726ad" ~ gps_Timestamp,
                                  interview_key=="39-71-25-23" & interview_id=="bf5f063f886747e0bea95284eaf20dc2" ~ gps_Timestamp,
                                  TRUE ~ createdDate
                                ),
      createdDate = case_when(
                                  interview_key=="38-99-06-28" & interview_id=="9f106e6358c24d2aad54510a813dc4ce" ~ gps_Timestamp,
                                  interview_key=="59-82-19-59" & interview_id=="592423b19c864fd4a714eeb3b8c0c9fb" ~ gps_Timestamp,
                                  interview_key=="50-47-81-71" & interview_id=="6a2bc43595fd4df0b98b2088eca88dd3" ~ gps_Timestamp,
                                  interview_key=="19-97-73-94" & interview_id=="de6bb15e5e5e4e2ebb971a8270fa16be" ~ gps_Timestamp,
                                  interview_key=="27-97-38-25" & interview_id=="ce7ff989c68b412cb7e89c20c2186cb3" ~ gps_Timestamp,
                                  interview_key=="06-08-58-38" & interview_id=="69d39e8bd2ae404ebd244536a0d336e8" ~ gps_Timestamp,
                                  interview_key=="53-95-09-19" & interview_id=="290a572e712f40d98451f1835e8fbb54" ~ gps_Timestamp,
                                  interview_key=="48-15-77-88" & interview_id=="f279abd6013744ec83f1665dc3b272e4" ~ gps_Timestamp,
                                  interview_key=="19-97-73-94" & interview_id=="de6bb15e5e5e4e2ebb971a8270fa16be" ~ gps_Timestamp,
                                  interview_key=="11-26-03-51" & interview_id=="4f243553ab9045819bce277334e71b85" ~ gps_Timestamp,
                                  interview_key=="97-03-54-45" & interview_id=="3d34684ea9d0411eb1cddaca3ac0f6b5" ~ gps_Timestamp,
                                  interview_key=="94-61-35-06" & interview_id=="603103e8689b4cc3ad023ce05b0726ad" ~ gps_Timestamp,
                                  interview_key=="39-71-25-23" & interview_id=="bf5f063f886747e0bea95284eaf20dc2" ~ gps_Timestamp,
                                  TRUE ~ createdDate
                                ),
      dates = as.Date(gps_Timestamp),
    )

  #removal of invalid cases
  downloaded_icbt_data <- downloaded_icbt_data %>%
    filter(
    #TO LOOK INTO ! (str_to_upper(str_squish(trim(enumerator_name)))=="SYLVIA AGYEMANG" & interview_key=="94-61-35-06" & interview_id== "603103e8689b4cc3ad023ce05b0726ad" )

    #not yet ! (str_to_upper(str_squish(trim(enumerator_name)))=="YACHAMBE KUPORKPA MOSES" & interview_key=="06-08-58-38" & interview_id== "69d39e8bd2ae404ebd244536a0d336e8" )
    #not yet ! (str_to_upper(str_squish(trim(enumerator_name)))=="SYLVIA AGYEMANG" & interview_key=="50-47-81-71" & interview_id== "6a2bc43595fd4df0b98b2088eca88dd3" )
    #not yet ! (str_to_upper(str_squish(trim(enumerator_name)))=="LOTSU KWAME BLESS" & interview_key=="48-15-77-88" & interview_id== "f279abd6013744ec83f1665dc3b272e4" )
    #not yet ! (str_to_upper(str_squish(trim(enumerator_name)))=="ABABAGRE NOAH ASSIBI" & interview_key=="38-50-57-79" & interview_id== "51dca45bf867441e8d17f5e5ea9bbb03" )
    #not yet ! (str_to_upper(str_squish(trim(enumerator_name)))=="PATIENCE GYABENG" & interview_key=="87-78-60-56" & interview_id== "f8da6d33caa64700883d96fa47cf26e4" )

   # yes delete below
    ! (
        (str_to_upper(str_squish(trim(enumerator_name)))=="OPPONG SIMON" & interview_key=="48-21-24-13" & interview_id== "5eae3f4bf3c64527b311375d0e857182" )  |
        (str_to_upper(str_squish(trim(enumerator_name)))=="OPPONG SIMON" & interview_key=="52-21-39-28" & interview_id== "3dd71d59fd2c43169701b5550b4e6bba" )  |
        (str_to_upper(str_squish(trim(enumerator_name)))=="AGYEIWAA ADUSAH MARY" & interview_key=="17-68-34-17" & interview_id== "b8f22ddaa5614cdc9d3cbc7fa0f3e39e" ) |
        (str_to_upper(str_squish(trim(enumerator_name)))=="ANYAMASA MARTINA" & interview_key=="06-29-78-16" & interview_id== "ed2c994f742648d7af73a8f98b05a741" )
      )



      # !(enumerator_name=="Juliana Sekyiraa" & interview_key=="14-40-62-43") |  #
      # !(enumerator_name=="Juliana Sekyiraa" & interview_key=="71-14-18-31") |  #
    )

  library(tm)

  #Stuck Manual Error Corrections
  ###############################
  downloaded_icbt_data <- downloaded_icbt_data %>%
    mutate(
      commodityObervedDescription= case_when(
                str_to_upper(str_squish(trim(enumerator_name)))=="ALHASSAN ABDUL-MUMIN" &
                   interview_key    =="59-96-29-29" &
                  month == 'October' &
                   interview_id     == "43ded5c278de4f22b8b8728b639d359e" &
                    transpondent_id == 6 &
                    Commodity_id    == 1  ~ "36 packs of alcoholic drink" ,

                str_to_upper(str_squish(trim(enumerator_name)))=="TAHIRU SAMSUDEEN" &
                   interview_key    =="64-87-10-11" &
                  month == 'October' &
                   interview_id     == "c48ae95255694e9fafd358aa5bc6e593" &
                    transpondent_id == 4 &
                    Commodity_id    == 1  ~ "1 pan of corn dough" ,

                  str_to_upper(str_squish(trim(enumerator_name)))=="TAHIRU SAMSUDEEN" &
                   interview_key    =="64-87-10-11" &
                  month == 'October' &
                   interview_id     == "c48ae95255694e9fafd358aa5bc6e593" &
                    transpondent_id == 6 &
                    Commodity_id    == 2  ~ "1 barrel of diesel" ,

                str_to_upper(str_squish(trim(enumerator_name)))=="ASAMOAH FLORENCE" &
                   interview_key    =="00-57-21-41" &
                  month == 'October' &
                   interview_id     == "adfce20eaff940f483cebd579c957977" &
                    transpondent_id == 17 &
                    Commodity_id    == 1  ~ "4 330ml pack of alcoholic drink" ,

                str_to_upper(str_squish(trim(enumerator_name)))=="PATRICK MENSAH DZIDZORNU" &
                   interview_key    =="53-71-68-82" &
                  month == 'November' &
                   interview_id     == "f5b72c525ff747dab058f8399b5037f5" &
                    transpondent_id == 5 &
                    Commodity_id    == 1  ~ "3 (5ltr) gallons of cooking oil " ,

                #20241223
                str_to_upper(str_squish(trim(enumerator_name)))=="ABBEY EUNICE CORNEA" &
                   interview_key    =="09-43-99-80" &
                  month == 'November' &
                   interview_id     == "461fe00ff4fb4136af207e674eee8a8f"  &
                    transpondent_id == 22 &
                    Commodity_id    == 1  ~ "20 pair of bathroom slippers" ,

                str_to_upper(str_squish(trim(enumerator_name)))=="JOHNSON KOFI MENSAH KUVEDU" &
                   interview_key    =="57-25-68-97" &
                  month == 'November' &
                   interview_id     == "da7d36e4f4ea443d89fba6e9ae53f33b"  &
                    transpondent_id == 7 &
                    Commodity_id    == 1  ~ "15 pieces of sticks" ,

                str_to_upper(str_squish(trim(enumerator_name)))=="JOHNSON KOFI MENSAH KUVEDU" &
                   interview_key    =="59-39-48-58" &
                  month == 'November' &
                   interview_id     == "a9bfb4f8665a4ebf9ce5e85b3d38cb09"  &
                    transpondent_id == 4 &
                    Commodity_id    == 1  ~ "180 pieces of body creams" ,

                str_to_upper(str_squish(trim(enumerator_name)))=="JOHNSON KOFI MENSAH KUVEDU" &
                   interview_key    =="64-85-55-00" &
                    month == 'November' &
                   interview_id     == "7d384713a08c4efebe43f2ee948565e8"  &
                    transpondent_id == 21 &
                    Commodity_id    == 1  ~ "35 pieces of fowls" ,

                #20241224
                str_to_upper(str_squish(trim(enumerator_name)))=="ABUSKA FELICITY" &
                   interview_key    =="55-17-17-12" &
                    month == 'December' &
                   interview_id     == removePunctuation("9d8cfb52-552c-4611-b702-6a2873d78eef")  &
                    transpondent_id == 9 &
                    Commodity_id    == 1  ~ "28 sack of fresh pepper" ,

                str_to_upper(str_squish(trim(enumerator_name)))=="ASUMASIM SANDO EDWARD" &
                   interview_key    =="52-20-08-58" &
                    month == 'December' &
                   interview_id     == removePunctuation("df14afd3-2661-46a5-9ebc-f388b6d2acc6")  &
                    transpondent_id == 3 &
                    Commodity_id    == 3  ~ "5 tuber of yam (cocoyam,water yam)" ,

                str_to_upper(str_squish(trim(enumerator_name)))=="ASUMASIM SANDO EDWARD" &
                   interview_key    =="52-20-08-58" &
                    month == 'December' &
                   interview_id     == removePunctuation("df14afd3-2661-46a5-9ebc-f388b6d2acc6")  &
                    transpondent_id == 16 &
                    Commodity_id    == 2  ~ "1 piece of traveling bag medium (ecolac medium)" ,

                str_to_upper(str_squish(trim(enumerator_name)))=="FRANSICA ANDOH" &
                   interview_key    =="07-19-33-33" &
                    month == 'December' &
                   interview_id     == removePunctuation("8c79ea5b-4971-46a0-a0cb-d145810afbf8")  &
                    transpondent_id == 19 &
                    Commodity_id    == 2  ~ "1 piece(s) of barrel (plastic container) 250l" ,

                str_to_upper(str_squish(trim(enumerator_name)))=="FRANSICA ANDOH" &
                   interview_key    =="84-70-49-42" &
                    month == 'December' &
                   interview_id     == removePunctuation("34c710a0-8c79-419b-bac4-8c6d7417fc6c")  &
                    transpondent_id == 10 &
                    Commodity_id    == 4  ~ "4 carton (24) of canned soft drink (330ml)" ,

                str_to_upper(str_squish(trim(enumerator_name)))=="AKASISI ERNESTINA WEWOLI" &
                   interview_key    =="67-09-16-46" &
                    month == 'December' &
                   interview_id     == "fc2f10b7-8238-4046-9c33-5ed734472c40"  &
                    transpondent_id == 20 &
                    Commodity_id    == 1  ~ "2 pieces of puppies" ,

                str_to_upper(str_squish(trim(enumerator_name)))=="KWOTUA VICTORIA" &
                   interview_key    =="39-83-18-34" &
                    month == 'December' &
                   interview_id     == removePunctuation("282cf099-34f8-4019-92d0-9139f6fee078")  &
                    transpondent_id == 5 &
                    Commodity_id    == 6  ~ "1 sack of kokonte(cassava flour)" ,

                str_to_upper(str_squish(trim(enumerator_name)))=="KWOTUA VICTORIA" &
                   interview_key    =="66-85-53-09" &
                    month == 'December' &
                   interview_id     == removePunctuation("52e5719c-c55f-4c62-8392-db60948816ed")  &
                    transpondent_id == 16 &
                    Commodity_id    == 1  ~ "20 sack of kokonte(cassava flour)" ,

                str_to_upper(str_squish(trim(enumerator_name)))=="MIRIAM AYIVOR" &
                   interview_key    =="92-12-14-94" &
                    month == 'December' &
                   interview_id     == removePunctuation("3f40b6d4-bcd1-49a4-84df-01144ac59ed9")  &
                    transpondent_id == 2 &
                    Commodity_id    == 1  ~ "5 sacks of charcoal" ,

                str_to_upper(str_squish(trim(enumerator_name)))=="MIRIAM AYIVOR" &
                   interview_key    =="92-12-14-94" &
                    month == 'December' &
                   interview_id     == removePunctuation("3f40b6d4-bcd1-49a4-84df-01144ac59ed9")  &
                    transpondent_id == 11 &
                    Commodity_id    == 1  ~ "2 box of floor tiles" ,

                str_to_upper(str_squish(trim(enumerator_name)))=="JOHNSON KOFI MENSAH KUVEDU" &
                   interview_key    =="27-14-16-84" &
                    month == 'December' &
                   interview_id     == removePunctuation("b8d63aea-78ef-4116-8769-338bc2642f3f")  &
                    transpondent_id == 12 &
                    Commodity_id    == 1  ~ "10 bottles of alcoholic beverages" ,

                str_to_upper(str_squish(trim(enumerator_name)))=="JOHNSON KOFI MENSAH KUVEDU" &
                   interview_key    =="54-44-26-12" &
                    month == 'December' &
                   interview_id     == removePunctuation("eb8bb5b4-7bd9-45aa-82bc-e9df9db72604")  &
                    transpondent_id == 14 &
                    Commodity_id    == 1  ~ "1 sack of mango" ,

                str_to_upper(str_squish(trim(enumerator_name)))=="JOHNSON KOFI MENSAH KUVEDU" &
                   interview_key    =="54-44-26-12" &
                    month == 'December' &
                   interview_id     == removePunctuation("eb8bb5b4-7bd9-45aa-82bc-e9df9db72604")  &
                    transpondent_id == 15 &
                    Commodity_id    == 1  ~ "2 sachets of washing powder" ,

                str_to_upper(str_squish(trim(enumerator_name)))=="JOHNSON KOFI MENSAH KUVEDU" &
                   interview_key    =="54-44-26-12" &
                    month == 'December' &
                   interview_id     == removePunctuation("eb8bb5b4-7bd9-45aa-82bc-e9df9db72604")  &
                    transpondent_id == 22 &
                    Commodity_id    == 1  ~ "1 sack of second hand clothing" ,

                str_to_upper(str_squish(trim(enumerator_name)))=="JOHNSON KOFI MENSAH KUVEDU" &
                   interview_key    =="54-44-26-12" &
                    month == 'December' &
                   interview_id     == removePunctuation("eb8bb5b4-7bd9-45aa-82bc-e9df9db72604")  &
                    transpondent_id == 26 &
                    Commodity_id    == 2  ~ "1 pack of baby diapers" ,

                str_to_upper(str_squish(trim(enumerator_name)))=="JOHNSON KOFI MENSAH KUVEDU" &
                   interview_key    =="54-44-26-12" &
                    month == 'December' &
                   interview_id     == removePunctuation("eb8bb5b4-7bd9-45aa-82bc-e9df9db72604")  &
                    transpondent_id == 30 &
                    Commodity_id    == 2  ~ "1 sack of onion" ,

                str_to_upper(str_squish(trim(enumerator_name)))=="AGYEI THEOPHILUS TWENE" &
                   interview_key    =="03-70-01-86" &
                    month == 'December' &
                   interview_id     == removePunctuation("3ea3d259-e35f-443e-804c-537f27a9a27f")  &
                    transpondent_id == 15 &
                    Commodity_id    == 1  ~ "1 basket of plantain" ,

                str_to_upper(str_squish(trim(enumerator_name)))=="EVANS BOATENG BOAMPONG" &
                   interview_key    =="00-37-15-30" &
                    month == 'December' &
                   interview_id     == removePunctuation("1d9db2b1-25d5-49e8-95f1-999f0d4e1109")  &
                    transpondent_id == 2 &
                    Commodity_id    == 1  ~ "1 half sack of konkonte" ,

             TRUE ~ commodityObervedDescription
              ),

      commodityQuantity = case_when(
              str_to_upper(str_squish(trim(enumerator_name)))=="ALICE OFOSU" &
                interview_key    =="60-88-53-60" &
                month == 'November' &
                interview_id     == removePunctuation('d7e8a0f5-2225-4b5e-9d14-e64d3a2a59b4') &
                transpondent_id == 3 &
                Commodity_id    == 1  ~ 6,

              str_to_upper(str_squish(trim(enumerator_name)))=="ALICE OFOSU" &
                interview_key    =="75-62-66-48" &
                month == 'November' &
                interview_id     == removePunctuation('d1589eab-14d7-47c2-8527-a08d19a4298e') &
                transpondent_id == 18 &
                Commodity_id    == 1  ~ 30,

              str_to_upper(str_squish(trim(enumerator_name)))=="JOHNSON KOFI MENSAH KUVEDU" &
                interview_key    =="10-51-63-73" &
                month == 'November' &
                interview_id     == removePunctuation('021fb534-2e6b-4dff-b084-d77a95aeaeb3') &
                transpondent_id == 22 &
                Commodity_id    == 1  ~ 4,

              str_to_upper(str_squish(trim(enumerator_name)))=="JOHNSON KOFI MENSAH KUVEDU" &
                interview_key    =="16-64-57-83" &
                interview_id     == removePunctuation('22f58115-bd8c-458e-b9cc-92abb8bcffa2') &
                transpondent_id == 16 &
                Commodity_id    == 1  ~ 2,

              str_to_upper(str_squish(trim(enumerator_name)))=="JOHNSON KOFI MENSAH KUVEDU" &
                interview_key    =="44-92-79-20" &
                month == 'November' &
                interview_id     == removePunctuation('f57bc57e-7d1a-4b44-9b7a-d767a643394d') &
                transpondent_id == 13 &
                Commodity_id    == 1  ~ 10,

              str_to_upper(str_squish(trim(enumerator_name)))=="JOHNSON KOFI MENSAH KUVEDU" &
                interview_key    =="44-92-79-20" &
                month == 'November' &
                interview_id     == removePunctuation('6b0366bc-718b-4b26-a9c8-4eb6da4b55d4') &
                transpondent_id == 30 &
                Commodity_id    == 1  ~ 2,

              str_to_upper(str_squish(trim(enumerator_name)))=="ASAMOAH FLORENCE" &
                interview_key    =="00-57-21-41" &
                month == 'October' &
                interview_id     == removePunctuation('adfce20e-aff9-40f4-83ce-bd579c957977') &
                transpondent_id == 17 &
                Commodity_id    == 1  ~ 4,

            #20241224
              str_to_upper(str_squish(trim(enumerator_name)))=="ASUMASIM SANDO EDWARD" &
                interview_key    =="34-65-80-71" &
                month == 'December' &
                interview_id     == removePunctuation('29002998-1e3b-4cc4-a0dd-ff735017730d') &
                transpondent_id == 25 &
                Commodity_id    == 1  ~ 2,

              str_to_upper(str_squish(trim(enumerator_name)))=="ASUMASIM SANDO EDWARD" &
                interview_key    =="52-20-08-58" &
                month == 'December' &
                interview_id     == removePunctuation('df14afd3-2661-46a5-9ebc-f388b6d2acc6') &
                transpondent_id == 7 &
                Commodity_id    == 1  ~ 3,

              str_to_upper(str_squish(trim(enumerator_name)))=="ASUMASIM SANDO EDWARD" &
                interview_key    =="52-20-08-58" &
                month == 'December' &
                interview_id     == removePunctuation('df14afd3-2661-46a5-9ebc-f388b6d2acc6') &
                transpondent_id == 10 &
                Commodity_id    == 1  ~ 6,

              str_to_upper(str_squish(trim(enumerator_name)))=="ABBEY EUNICE CORNEA" &
                interview_key    =="09-43-99-80" &
                month == 'November' &
                interview_id     == removePunctuation('461fe00f-f4fb-4136-af20-7e674eee8a8f') &
                transpondent_id == 22 &
                Commodity_id    == 1  ~ 20,

              str_to_upper(str_squish(trim(enumerator_name)))=="JOHNSON KOFI MENSAH KUVEDU" &
                interview_key    =="61-99-97-20" &
                month == 'November' &
                interview_id     == removePunctuation('6b0366bc-718b-4b26-a9c8-4eb6da4b55d4') &
                transpondent_id == 30 &
                Commodity_id    == 1  ~ 2,

              str_to_upper(str_squish(trim(enumerator_name)))=="MIRIAM AYIVOR" &
                interview_key    =="68-12-13-79" &
                month == 'December' &
                interview_id     == removePunctuation('82b6ed57-5d29-41b1-9652-93d63ccfc5cc') &
                transpondent_id == 16 &
                Commodity_id    == 1  ~ 3,

            str_to_upper(str_squish(trim(enumerator_name)))=="COURAGE AGORTSE" &
              interview_key    =="70-71-41-58" &
              month == 'November' &
              interview_id     == removePunctuation("fed8af86-4676-4464-b00d-50b574938aa9")  &
              transpondent_id == 1  &
              Commodity_id    == 1  ~ 10 ,

            str_to_upper(str_squish(trim(enumerator_name)))=="COURAGE AGORTSE" &
              interview_key    =="70-71-41-58" &
              month == 'November' &
              interview_id     == removePunctuation("fed8af86-4676-4464-b00d-50b574938aa9")  &
              transpondent_id == 7  &
              Commodity_id    == 1  ~ 5 ,

            str_to_upper(str_squish(trim(enumerator_name)))=="COURAGE AGORTSE" &
              interview_key    =="96-46-85-16" &
              month == 'November' &
              interview_id     == "ab969ce4-a913-43e1-9d87-d2cf729a47f7"  &
              transpondent_id == 5  &
              Commodity_id    == 1  ~ 6 ,

            str_to_upper(str_squish(trim(enumerator_name)))=="ADUHENE JOSEPH" &
              interview_key    =="24-87-52-09" &
              month == 'November' &
              interview_id     == removePunctuation("674f4214-9cd1-429a-a4bf-fb906015bf97")  &
              transpondent_id == 17  &
              Commodity_id    == 1  ~ 2 ,

            str_squish(trim(str_to_upper(str_squish(trim(enumerator_name)))))=="COURAGE AGORTSE" &
              interview_key    =="96-46-85-16" &
              month == 'November' &
              interview_id     == removePunctuation("ab969ce4-a913-43e1-9d87-d2cf729a47f7")  &
              transpondent_id == 5  &
              Commodity_id    == 1  ~ 6 ,

        TRUE ~ commodityQuantity
      ),

      observedRespondentDescription= case_when(
                str_to_upper(str_squish(trim(enumerator_name)))=="DANIEL GELI" &
                  interview_key    =="71-02-42-24" &
                  month == 'November' &
                  interview_id     == "f5b06bd5a18e4c8297df8fed9b2850de" &
                  transpondent_id == 16  ~ "a man going out on foot" ,

                str_to_upper(str_squish(trim(enumerator_name)))=="DANIEL GELI" &
                  interview_key    =="71-02-42-24" &
                  month == 'November' &
                  interview_id     == "f5b06bd5a18e4c8297df8fed9b2850de" &
                  transpondent_id ==  30  ~ "a man coming in with Push Truck" ,

                #20241224
                str_to_upper(str_squish(trim(enumerator_name)))=="FRANSICA ANDOH" &
                  interview_key    =="84-70-49-42" &
                  month == 'December' &
                  interview_id     == removePunctuation("34c710a0-8c79-419b-bac4-8c6d7417fc6c") &
                  transpondent_id ==  8  ~ "A man going out of the country on a tricycle" ,

                str_to_upper(str_squish(trim(enumerator_name)))=="JOSHUA AKUTEY KUMAH" &
                  interview_key    =="52-75-58-57" &
                  month == 'December' &
                  interview_id     == "bd65ef7d-51dd-4072-8941-6c7f40e8b285" &
                  transpondent_id ==  30  ~ "A woman coming in on foot" ,

                str_to_upper(str_squish(trim(enumerator_name)))=="ABIGAIL BOADI" &
                  interview_key    =="03-39-52-32" &
                  month == 'December' &
                  interview_id     == "0f7caa27-be7a-48b0-b2e3-e813eefe4b72" &
                  transpondent_id ==  10  ~ "a woman coming in with goods on foot" ,

                str_to_upper(str_squish(trim(enumerator_name)))=="ABDULAI NAZIRU" &
                  interview_key    =="58-80-33-78" &
                  month == 'December' &
                  interview_id     == removePunctuation("04b831a1-1928-4871-bc56-d69415d73c18") &
                  transpondent_id ==  2  ~ "A woman going out on foot" ,
                
                str_to_upper(str_squish(trim(enumerator_name)))=="EVANS BOATENG BOAMPONG" &
                  interview_key    =="06-26-98-98" &
                  month == 'December' &
                  interview_id     == "4e355e8b-b6fd-44c4-97b9-71eca61cfc9a" &
                  transpondent_id == 3  ~ "a man going out on Motorbike" ,

                  TRUE ~ observedRespondentDescription
                  ),

      transportMeans= case_when(
               str_to_upper(str_squish(trim(enumerator_name)))=="MUMEN HAMDDAN" &
                 interview_key    =="97-71-23-78" &
                 month == 'November' &
                 interview_id     == "9bc564e68aaa4e449ad2830b10ad214c" &
                 transpondent_id == 9  ~ "Motorbike" ,

               #20241224
               str_to_upper(str_squish(trim(enumerator_name)))=="ABUSKA FELICITY" &
                 interview_key    =="55-17-17-12" &
                 month == 'December' &
                 interview_id     == "9d8cfb52-552c-4611-b702-6a2873d78eef" &
                 transpondent_id == 6  ~ "Bicycle" ,

               str_to_upper(str_squish(trim(enumerator_name)))=="ABUSKA FELICITY" &
                 interview_key    =="96-81-51-88" &
                 month == 'December' &
                 interview_id     == "ee6a89d6-68ff-43d4-a113-c38fd62a82ec" &
                 transpondent_id == 12  ~ "Tricycle" ,


                 TRUE ~ transportMeans
                    ),

       tradeDirection= case_when(
             str_to_upper(str_squish(trim(enumerator_name)))=="ASUMASIM SANDO EDWARD" &
               interview_key    =="23-57-66-81" &
               month == 'December' &
               interview_id     == "6ab30a33-c2cb-44cd-9c65-cbad8c44da21" &
               transpondent_id == 12  ~ "Coming in (Import)" ,

             str_to_upper(str_squish(trim(enumerator_name)))=="ZOR SAVIOUR AKORSUA" &
               interview_key    =="08-46-56-30" &
               month == 'December' &
               interview_id     == "4b708be0-dad3-4056-b466-04d129148854" &
               transpondent_id == 9  ~ "Going out (Export)" ,

             str_to_upper(str_squish(trim(enumerator_name)))=="ABDULAI NAZIRU" &
               interview_key    =="58-80-33-78" &
               month == 'December' &
               interview_id     == "04b831a1-1928-4871-bc56-d69415d73c18" &
               transpondent_id == 2  ~ "Going out (Export)" ,

             str_to_upper(str_squish(trim(enumerator_name)))=="ALHASSAN RAKIB" &
               interview_key    =="46-21-78-66" &
               month == 'December' &
               interview_id     == "2359e629-7b73-475a-b539-ddf57dd2be5a" &
               transpondent_id == 4  ~ "Coming in (Import)" ,

             str_to_upper(str_squish(trim(enumerator_name)))=="DATTAH SYLVIA" &
               interview_key    =="01-64-24-68" &
               month == 'December' &
               interview_id     == "38b81753-6ff4-4032-8ca4-8304d6160aaa" &
               transpondent_id == 1  ~ "Coming in (Import)" ,

               TRUE ~ tradeDirection
                  ),

      unit_of_measure = case_when(
            str_to_upper(str_squish(trim(enumerator_name)))=="MIRIAM AYIVOR" &
              interview_key    =="08-00-32-15" &
              month == 'December' &
              interview_id     == removePunctuation('777a97d7-fe21-4e6b-a0ec-9dff58f973b2') &
              transpondent_id == 20 &
              Commodity_id    == 1  ~ 'bag 5kg',

            str_to_upper(str_squish(trim(enumerator_name)))=="PATRICK MENSAH DZIDZORNU" &
              interview_key    =="96-28-51-75" &
              month == 'December' &
              interview_id     == removePunctuation('7835f336-6896-4895-bc57-9d8212ac5700') &
              transpondent_id == 12 &
              Commodity_id    == 1  ~ 'bottle (200ml)',

            TRUE ~ unit_of_measure
      ),

      productObserved = case_when(
            str_to_upper(str_squish(trim(enumerator_name)))=="JOHNSON KOFI MENSAH KUVEDU" &
              interview_key    =="54-44-26-12" &
              month == 'December' &
              interview_id     == 'eb8bb5b4-7bd9-45aa-82bc-e9df9db72604' &
              transpondent_id == 5 &
              Commodity_id    == 1  ~ 'sponge',

            TRUE ~ productObserved
      )
    
    ) %>%
    select(-interview_id, -QuestionnaireId) %>%
    rename(
      interview_id = InterviewId ,
      UserName = ResponsibleName)

  #-------------------------
  #Corrections using excel
  #-------------------------
  library(readxl)
  manual_corrections <- read_excel("correction/icbt errors manual corrections DEC 2024.xlsx")
  
  for (i in 1:nrow(manual_corrections)) {
    # row <- manual_corrections[1,]
    row <- manual_corrections[i,]
    
    downloaded_icbt_data <- downloaded_icbt_data %>%
      mutate(
        observedRespondentDescription= case_when(
                                      str_to_upper(str_squish(trim(enumerator_name)))==str_to_upper(str_squish(trim(row$enumerator_name))) &
                                      interview_key    ==row$interview_key &
                                      month == row$month &
                                      interview_id     == row$interview_id &
                                      transpondent_id == row$transpondent_id &
                                      !is.na(row$correctTranspondentDescription) ~ as.character(str_squish(trim(row$correctTranspondentDescription))) ,
                                      TRUE ~ observedRespondentDescription
                                      ),
        
        tradeDirection= case_when(
                        str_to_upper(str_squish(trim(enumerator_name)))==str_to_upper(str_squish(trim(row$enumerator_name)))&
                        interview_key    == row$interview_key &
                        month == row$month &
                        interview_id     == row$interview_id &
                        transpondent_id == row$transpondent_id &
                        !is.na(row$correctTradeDirection)  ~ as.character(str_squish(trim(row$correctTradeDirection))),
                        TRUE ~ tradeDirection),
        
        
        transportMeans= case_when(
                            str_to_upper(str_squish(trim(enumerator_name)))==str_to_upper(str_squish(trim(row$enumerator_name)))&
                            interview_key    == row$interview_key &
                            month == row$month &
                            interview_id     == row$interview_id &
                            transpondent_id == row$transpondent_id &
                            !is.na(row$correctMeansOfTransport)  ~ as.character(str_squish(trim(row$correctMeansOfTransport))),
                            TRUE ~ transportMeans),
        
        commodityObervedDescription= case_when(
          str_to_upper(str_squish(trim(enumerator_name)))==str_to_upper(str_squish(trim(row$enumerator_name)))&
            interview_key    == row$interview_key &
            month == row$month &
            interview_id     == row$interview_id &
            transpondent_id == row$transpondent_id &
            Commodity_id    == row$Commodity_id  &
            !is.na(row$correctProductDescription) ~ as.character(str_squish(trim(row$correctProductDescription))) ,
          TRUE ~ commodityObervedDescription),
        
        productObserved= case_when(
                          str_to_upper(str_squish(trim(enumerator_name)))==str_to_upper(str_squish(trim(row$enumerator_name)))&
                          interview_key    == row$interview_key &
                          month == row$month &
                          interview_id     == row$interview_id &
                          transpondent_id == row$transpondent_id &
                          Commodity_id    == row$Commodity_id  &
                          !is.na(row$CorrectSelectedProduct) ~ as.character(str_squish(trim(row$CorrectSelectedProduct))) ,
                          TRUE ~ productObserved),
        
        commodityQuantity= case_when(
                          str_to_upper(str_squish(trim(enumerator_name)))==str_to_upper(str_squish(trim(row$enumerator_name)))&
                          interview_key    == row$interview_key &
                          month == row$month &
                          interview_id     == row$interview_id &
                          transpondent_id == row$transpondent_id &
                          Commodity_id    == row$Commodity_id  &
                          !is.na(row$correctCommodityQty) ~ as.integer(str_squish(trim(row$correctCommodityQty))) ,
                          TRUE ~ commodityQuantity),
        
        unit_of_measure= case_when(
                          str_to_upper(str_squish(trim(enumerator_name)))==str_to_upper(str_squish(trim(row$enumerator_name)))&
                          interview_key    == row$interview_key &
                          month == row$month &
                          interview_id     == row$interview_id &
                          transpondent_id == row$transpondent_id &
                          Commodity_id    == row$Commodity_id  &
                          !is.na(row$correctUnitOfMeasure) ~ as.character(str_squish(trim(row$correctUnitOfMeasure))) ,
                          TRUE ~ unit_of_measure),
        
      )
  }
  
  
 saveFinalData <-  downloaded_icbt_data

  #--- change created to GPS timestamp

  # #fix blank month and qtr
  # downloaded_icbt_data <- downloaded_icbt_data %>%
  #   mutate(
  # #fix blank month and qtr
  #     month = case_when(
  #       is.na(month) ~ as.character(month(gps_Timestamp, label = TRUE, abbr = FALSE)),
  #       TRUE ~ month
  #     ),
  #     quarter = case_when(
  #       is.na(quarter) & str_to_lower(month)=="october" ~ as.character(1),
  #       TRUE ~ quarter
  #     ),
  #     dates = as.Date(gps_Timestamp),

      # commodityObervedDescription= case_when(
      #                           str_to_lower(month)=="october" ~  gsub("pomade","body lotion", str_to_lower(commodityObervedDescription) ),
      #                           TRUE ~ commodityObervedDescription # correction for Month1 OCTOBER only, here month was blank
      # ) ,
#
    # )


  #save final dataset
  final_data_dir <- "Data_final/"
  #create Data directory if it does not already exist
  ifelse(!dir.exists(file.path(final_data_dir)),
         dir.create(file.path(final_data_dir)),
         "fina_data_dir Directory Exists")

  # remove_val_labels(hq_icbt_data)

  #check errors
  # icbt_data <- downloaded_icbt_data
  source(file.path("server/hqdata/03_errorchecks.R"),  local = TRUE)$value
  saveRDS(errorChecks,paste(final_data_dir,'error_data.RDS',sep=''))

  #save final dataset
  saveRDS(saveFinalData,paste(final_data_dir,'icbt_data.RDS',sep=''))
  saveRDS(downloaded_icbt_data %>% distinct(month,quarter),paste(final_data_dir,'dataCollectionPeriod.RDS',sep=''))
  print("hq download and merge okay")

  saveRDS(dataDownloadStarted, "Data_final/lastDataAccessedDateTime.RDS")

  # new_interviwer_users <- get_interviewers()
  # saveRDS(new_interviwer_users,paste(final_data_dir,'users.RDS',sep=''))

  #need to script trade direction error text correction
  #ok
