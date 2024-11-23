rm(list=ls())  
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
  
  
  
  newDataMeta <- select(read_rds("server/users.RDS"),UserId, UserName) %>%  
    rename(responsibleId = UserId) %>% distinct(responsibleId,UserName, .keep_all = T)
  
  # get the serve various case versions
  server_qnr <- susoapi::get_questionnaires() %>% 
    filter(title == "ICBT FIELD WORK") %>%
    distinct(title, .keep_all = T)
  
  # cases <- susoapi:::get_interviews_for_questionnaire(
  #   chunk_size=100,
  #   qnr_id=server_qnr$questionnaireId,
  #   qnr_version= server_qnr$version,  # This should be an integer, not a string
  #   # 
  #   server = Sys.getenv("SUSO_SERVER"),
  #   workspace = Sys.getenv("SUSO_WORKSPACE"),
  #   user = Sys.getenv("SUSO_USER"),
  #   password = Sys.getenv("SUSO_PASSWORD")
  # )
  # 
  # count <- get_interviews_count()
  # count <- interviews_request()
  # myQ <- get_questionnaires()
  # gitdata<-get_interviews_for_questionnaire(
  #   
  #                                           # qnr_id="49957160-c7e4-4af9-a658-b99111b9104d",
  #                                           qnr_id="49957160-c7e4-4af9-a658-b99111b9104d",
  #                                           chunk_size = 100,
  #                                           qnr_version=3,
  #                                           server = Sys.getenv("SUSO_SERVER"),
  #                                           workspace = Sys.getenv("SUSO_WORKSPACE"),
  #                                           user = Sys.getenv("SUSO_USER"),
  #                                           password = Sys.getenv("SUSO_PASSWORD")
  #                                           )
  # 
  # show_credentials()
  # int_stat<- get_possible_interview_statuses()
  # 
  # intv <- get_interviews(
  #   nodes = c("id", "key", "assignmentId", "identifyingData", "questionnaireId",
  #             "questionnaireVersion", "questionnaireVariable", "responsibleName", "responsibleId",
  #             "responsibleRole", "supervisorName", "status", "actionFlags", "wasCompleted",
  #             "notAnsweredCount", "errorsCount", "createdDate", "updateDateUtc",
  #             "receivedByInterviewerAtUtc", "interviewMode"),
  #   chunk_size = 100,
  #   server = Sys.getenv("SUSO_SERVER"),
  #   workspace = Sys.getenv("SUSO_WORKSPACE"),
  #   user = Sys.getenv("SUSO_USER"),
  #   password = Sys.getenv("SUSO_PASSWORD")
  # )
  # data0 <- get_interviews(
  #   nodes = c("id", "key", "assignmentId", "identifyingData", "questionnaireId",
  #             "questionnaireVersion", "questionnaireVariable", "responsibleName", "responsibleId",
  #             "responsibleRole", "supervisorName", "status", "actionFlags", "wasCompleted",
  #             "notAnsweredCount", "errorsCount", "createdDate", "updateDateUtc",
  #             "receivedByInterviewerAtUtc", "interviewMode"),
  #   chunk_size = 100,
  #   server = Sys.getenv("SUSO_SERVER"),
  #   workspace = Sys.getenv("SUSO_WORKSPACE"),
  #   user = Sys.getenv("SUSO_USER"),
  #   password = Sys.getenv("SUSO_PASSWORD")
  # )
  
  # data1 <- get_interviewers()
  # data2 <- get_interviews_for_questionnaire(
  #   chunk_size = 100,
  #   qnr_id="49957160-c7e4-4af9-a658-b99111b9104d",
  #   qnr_version=1,
  #   server = Sys.getenv("SUSO_SERVER"),
  #   workspace = Sys.getenv("SUSO_WORKSPACE"),
  #   user = Sys.getenv("SUSO_USER"),
  #   password = Sys.getenv("SUSO_PASSWORD")
  # )
  # reGetUser <- get_user_details(user_id="")
  
  
  # FreshUsersAssignments<- get_assignments()# %>% filter(QuestionnaireId=="49957160-c7e4-4af9-a658-b99111b9104d")
  # FreshUsersAssignments<- new_usersAssignments %>% filter(QuestionnaireId=="49957160c7e44af9a658b99111b9104d$1")
    
  downloaded_icbt_data <- data.frame()
  
  download_matching(
    matches = server_qnr$title, 
    export_type = "STATA",
    path = hqDownload_dir,
    # qnr_id= server_qnr_id
  )
  
  #unzip each version of the files
  zipppedFiles <- list.files(path = hqDownload_dir, pattern = "*.zip", full.names = T)
  # zipfile<-zipppedFiles[2]
  for (zipfile in zipppedFiles) {
    #take each zip file and extract
    if (file.exists(zipfile)) {
      unzip( zipfile, exdir=hq_extracted_dir) 
    }
    #take the dataset and process it
    #get user ids and merge with responsible id
    userID_in_Data <-  read_dta(paste(hq_extracted_dir,"interview__actions.dta",sep = '')) %>% select(
      interview__key, interview__id, action, originator, date, time
    ) %>%filter(action==12) %>% #filter case with created action, for the one who created the case
      rename(UserName =  originator,
             qnr_createdDate =date,
             qnr_createdTime =time
      )  %>%
      mutate(
        qnr_createdDateTime= paste(qnr_createdDate, 'T' , qnr_createdTime, sep = ''),
      ) %>%
      select(-c('action','qnr_createdDate','qnr_createdTime')) %>%
      distinct( UserName,interview__key,interview__id , .keep_all = T) %>% 
      arrange(UserName, interview__key,interview__id )
    
    MajorMeta <- left_join(userID_in_Data,newDataMeta, by=c("UserName"))
    
    
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
    icbt_data_version <- read_dta(paste(hq_extracted_dir,server_qnr$variable,".dta", sep = ''))
    
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
        ) %>% left_join(MajorMeta) 
      
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
      dates = as.Date(gps_Timestamp),

      
      team_number= parse_number(team_number),
      productObserved=  gsub("colanut", "cola nut", productObserved, ignore.case = TRUE) ,
      # fix for 'coming on' and 'going on', phrases to 'comin in on' and 'going out on' 
      #afterwards, remove it from error check
      observedRespondentDescription = str_squish(trim(trimws(observedRespondentDescription))),
      observedRespondentDescription = case_when(
        str_detect(str_to_lower(observedRespondentDescription),'coming on') ~  gsub('coming on', 'coming in on', observedRespondentDescription, ignore.case = TRUE),
        str_detect(str_to_lower(observedRespondentDescription),'coning in') ~  gsub('coning in', 'coming in', observedRespondentDescription, ignore.case = TRUE),
        str_detect(str_to_lower(observedRespondentDescription),'cominng in') ~  gsub('cominng in', 'coming in', observedRespondentDescription, ignore.case = TRUE),
        str_detect(str_to_lower(observedRespondentDescription),'coming with') ~  gsub('coming with', 'coming in on', observedRespondentDescription, ignore.case = TRUE),
        str_detect(str_to_lower(observedRespondentDescription),'going on') ~ gsub('going on', 'going out on', observedRespondentDescription, ignore.case = TRUE),
        str_detect(str_to_lower(observedRespondentDescription),'gong out') ~ gsub('gong out', 'going out', observedRespondentDescription, ignore.case = TRUE),
        str_detect(str_to_lower(observedRespondentDescription),'going with') ~ gsub('going with', 'going out with', observedRespondentDescription, ignore.case = TRUE),
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
                                  interview_key=="59-82-19-59" & interview_id=="592423b19c864fd4a714eeb3b8c0c9fb" ~ qnr_createdDateTime,
                                  interview_key=="50-47-81-71" & interview_id=="6a2bc43595fd4df0b98b2088eca88dd3" ~ qnr_createdDateTime,
                                  TRUE ~ gps_Timestamp
                                )
    )
  
  #removal of invalid cases
  downloaded_icbt_data <- downloaded_icbt_data %>%
    filter(
      !(str_squish(trim(str_to_lower(enumerator_name)))=="yachambe kuporkpa moses" & interview_key=="06-08-58-38") |  ## he said he wanted to sync and he was finding difficulty with it so he entered a any random information to be able to help him sync.
      !(enumerator_name=="Juliana Sekyiraa" & interview_key=="14-40-62-43") |  # 
      !(enumerator_name=="Juliana Sekyiraa" & interview_key=="71-14-18-31") |  # 
      !(str_squish(trim(str_to_lower(enumerator_name)))=="patience gyabeng" & interview_key=="87-78-60-56") |  #
      !(str_squish(trim(str_to_lower(enumerator_name)))=="ababagre noah assibi" & interview_key=="38-50-57-79")   #
    )
  
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
  
  #save final dataset
  saveRDS(downloaded_icbt_data,paste(final_data_dir,'icbt_data.RDS',sep=''))
  saveRDS(downloaded_icbt_data %>% distinct(month,quarter),paste(final_data_dir,'dataCollectionPeriod.RDS',sep=''))
  print("hq download and merge okay")
  
  
  #check errors
  # icbt_data <- downloaded_icbt_data
  source(file.path("server/hqdata/03_errorchecks.R"),  local = TRUE)$value
  saveRDS(errorChecks,paste(final_data_dir,'error_data.RDS',sep=''))
  
  # new_interviwer_users <- get_interviewers()
  # saveRDS(new_interviwer_users,paste(final_data_dir,'users.RDS',sep=''))
  
  #need to script trade direction error text correction
  #ok