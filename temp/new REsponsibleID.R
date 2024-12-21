
#get Responsible IDS from server
#########################################
icbt_metaData <- data.frame()

for (i in 1:nrow(questlist)){
  icbtQues <- questlist [i,]
    
    metaData_fetch <-  suso_getQuestDetails(
        workspace = sqlSvr$workspace,
        token = NULL,
        quid  =   icbtQues$QuestionnaireId,
        version = icbtQues$Version,
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
        enumerator_name = str_squish(trim(trimws(str_to_title(enumerator_name)))),
        enumerator_contact=as.character(paste0(enumerator_contact))
      )
    
    icbt_metaData <- dplyr::bind_rows(icbt_metaData, metaData_fetch)
    rm(metaData_fetch)
}





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
      ) %>% left_join(MajorMeta) 
    
    downloaded_icbt_data <- dplyr::bind_rows(downloaded_icbt_data, icbt_data_version)
    rm(icbt_data_version)
    ## end zip loop
  }
}