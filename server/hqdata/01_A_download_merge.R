user_assigned_data <- read_excel("server/loginDataAccess.xlsx") %>% 
  filter( 
    email ==  user_out_email()
    # email ==  'terryarthur35@gmail.com'
  )

# sqlSvr <- readRDS("server/credentials/icbt_svr.rds")
# 
# #create directory for saving data download if not exist
# data_dir <- "Data/"
# hqDownload_dir <- paste(data_dir,'HQ_download/',sep='')
# hq_extracted_dir <- paste(data_dir,'HQ_extracted/',sep='')
# 
# #delete Data directory if it already exist
# ifelse(dir.exists(file.path(data_dir)),
#        unlink(data_dir, recursive = TRUE),
#        "Data Directory Exists")
# 
# #create Data directory if it does not already exist
# ifelse(!dir.exists(file.path(data_dir)),
#        dir.create(file.path(data_dir)),
#        "Data Directory Exists")
# 
# #create HQ_download Director in Data folder if it does not already exist
# ifelse(!dir.exists(file.path(hqDownload_dir)),
#        dir.create(file.path(hqDownload_dir)),
#        "HQ_download Directory Exists")
# 
# #create HQ_extracted Director in Data folder if it does not already exist
# ifelse(!dir.exists(file.path(hq_extracted_dir)),
#        dir.create(file.path(hq_extracted_dir)),
#        "HQ_extracted Directory Exists")
# 
# # server authentication:
# set_credentials(
#   server = sqlSvr$server, workspace = sqlSvr$workspace, 
#   user =sqlSvr$usr, password = sqlSvr$pswd
# )
# 
# newDataMeta <- select(read_rds("server/users.RDS"),UserId, UserName) %>%  
#   rename(responsibleId = UserId) %>% distinct(responsibleId,UserName, .keep_all = T)
# 
# # get the serve various case versions
# server_qnr <- susoapi::get_questionnaires() %>% 
#   filter(title == "ICBT FIELD WORK") %>%
#   distinct(title, .keep_all = T)
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
# #unzip each version of the files
# # my_zip_dir <- hqDownload_dir
# 
# zipppedFiles <- list.files(path = hqDownload_dir, pattern = "*.zip", full.names = T)
# # zipfile<-zipppedFiles[2]
# for (zipfile in zipppedFiles) {
#   #take each zip file and extract
#   if (file.exists(zipfile)) {
#     unzip( zipfile, exdir=hq_extracted_dir) 
#   }
#   #take the dataset and process it
#   #get user ids and merge with responsible id
#   # userID_in_Data <-  read_dta(paste(hq_extracted_dir,"assignment__actions.dta",sep = '')) %>% select(
#   #   assignment__id, responsible__name
#   # ) %>% rename(
#   #   UserName =  responsible__name
#   # # )  %>% distinct(UserName,  .keep_all = T)
#   # )  %>% distinct( assignment__id, .keep_all = T)
#   
#   
#   userID_in_Data <-  read_dta(paste(hq_extracted_dir,"interview__actions.dta",sep = '')) %>% select(
#     interview__key, interview__id, action, originator, date, time
#   ) %>%filter(action==12) %>% #filter case with created action, for the one who created the case
#     rename(UserName =  originator,
#            qnr_createdDate =date,
#            qnr_createdTime =time
#            )  %>%
#     mutate(
#       qnr_createdDateTime= paste(qnr_createdDate, 'T' , qnr_createdTime, sep = ''),
#     ) %>%
#     select(-c('action','qnr_createdDate','qnr_createdTime')) %>%
#     distinct( UserName,interview__key,interview__id , .keep_all = T) %>% 
#     arrange(UserName, interview__key,interview__id )
#   
#   MajorMeta <- left_join(userID_in_Data,newDataMeta, by=c("UserName"))
#   
#   
#   # take cases meta data
#   library(haven)
#   
#   metaColNames <- c("interview__key", "interview__id","enumerator_name" ,"enumerator_contact", "team_number" ,      
#                     "id02","id02a","id03","id03b","id04",
#                     "id06","gps__Latitude","gps__Longitude","gps__Accuracy","gps__Altitude",
#                     "gps__Timestamp","assignment__id", "quarter" , 'date_and_time_collection'
#   )
#   
#     
#   transpondentNames <- c("interview__key","interview__id","transportercharacteristics__id",
#                          "s2q1","s2q2","s2q3",
#                          "s2q3ll","s2q3llo","s2q10a","s2q10b"
#   )
#   
#   #take the stata   files for processing
#   icbt_data_version <- read_dta(paste(hq_extracted_dir,server_qnr$variable,".dta", sep = ''))
#   
#   if (nrow(icbt_data_version)>0){
#     icbt_data_version <- icbt_data_version %>%
#       select(all_of(metaColNames)) %>%
#       dplyr::mutate(  #bring in stata factor labels
#         id02 = haven::as_factor(id02),
#         id03 = haven::as_factor(id03),
#         id04 = haven::as_factor(id04),
#         id06 = haven::as_factor(id06)
#       ) %>% 
#       left_join(
#         read_dta(paste(hq_extracted_dir,"transportercharacteristics.dta",sep = '')) %>% 
#           select(all_of(transpondentNames)) %>%
#           dplyr::mutate(  #bring in stata factor labels
#             s2q2 = haven::as_factor(s2q2),
#             s2q3 = haven::as_factor(s2q3),
#             s2q10a = haven::as_factor(s2q10a),
#             # s2q10b = haven::as_factor(s2q10b),
#             s2q3ll = haven::as_factor(s2q3ll)
#           )
#         , by=c("interview__key"="interview__key", "interview__id" = "interview__id")
#       ) %>%
#       left_join(
#         read_dta(paste(hq_extracted_dir,"Commodity.dta",sep = '')) %>%
#           # mutate(across(matches(s2q6ao), haven::as_factor(s2q6ao))) %>%
#           # mutate_at(
#           #   vars(quietly(one_of)("s2q6ao",  .vars = haven::as_factor(s2q6ao)))
#           #   # s2q6ao = haven::as_factor(s2q6ao)
#           #           ) %>%
#           dplyr::mutate(  #bring in stata factor labels
#             s2q4 = haven::as_factor(s2q4),
#             s2b1 = haven::as_factor(s2b1),
#             s2q5 = haven::as_factor(s2q5),
#             s2l1 = haven::as_factor(s2l1),
#             s2l2 = haven::as_factor(s2l2),
#             s2q6oo = haven::as_factor(s2q6oo),
#             
#             # s2q10a = haven::as_factor(s2q10a)
#           ),
#         by=c("interview__key"="interview__key",
#              "interview__id"="interview__id",
#              "transportercharacteristics__id"="transportercharacteristics__id"
#         )
#       ) %>% rename(
#         assignment_id = assignment__id
#       ) %>% left_join(MajorMeta) 
#     
#     # if("s2q6ao" %in% names(icbt_data) ){
#     #   icbt_data <- icbt_data %>% mutate(
#     #     
#     #   s2q6ao = haven::as_factor(s2q6ao)
#     #   )
#     # }
#    
#     
#     downloaded_icbt_data <- dplyr::bind_rows(downloaded_icbt_data, icbt_data_version)
#     rm(icbt_data_version)
#     ## end zip loop
#   }
#     
#   
# }
# 
# if("s2q6ao" %in% names(downloaded_icbt_data) ){
#   downloaded_icbt_data <- downloaded_icbt_data %>% mutate(
# 
#   s2q6ao = haven::as_factor(s2q6ao)
#   )
# }
# 
# #Pull HH level var renames file
# renameData <- read_excel("server/dictionary/varNames.xlsx",sheet = "icbtVarRenames")
# 
# for (i in 1:nrow(renameData)  ) {
#   row <- renameData[i,]  #filter that row
#   if(row$oldVarName %in% names(downloaded_icbt_data)){ #if name var exists, then rename the var
#     downloaded_icbt_data <- rename.vars(downloaded_icbt_data, from =row$oldVarName, to = row$newVarName)
#   }
# }
# 
# #filtering for assigned dataset 
# icbt_data <- data.frame()
# # filteredDataset = data.frame()
# 
# for(i in 1:nrow(user_assigned_data)) {
#   row <- user_assigned_data[i,]
#   
#   filteredDataset <-  downloaded_icbt_data %>%
#     filter(
#       regionCode %in% (row$startRegionCode:row$endRegionCode)
#     )  %>%
#   filter(
#     parse_number(team_number)>=row$startTeamNumber &   parse_number(team_number)<=row$endTeamNumber
#   ) %>%
#     arrange(RegionName, districtName, townCity, borderPostName, team_number, enumerator_name )
# 
#   icbt_data <- dplyr::bind_rows(icbt_data, filteredDataset)
#   rm(filteredDataset)
#   
#   # print(row$startRegionCode)
# }
# 
# rm(downloaded_icbt_data)
# 
# 
# # rm(renameData, metaColNames, transpondentNames, row, server_qnr,sqlSvr)
# colnames(icbt_data) = gsub("__", "_", colnames(icbt_data))
# 
# # caseIdentifier <-cases %>% rename(interview_id = id) %>%
# #   select(
# # c('key', 'interview_id', 'createdDate','updateDateUtc',
# #   'enumerator_name', 'enumerator_contact','responsibleId',
# #   'questionnaireVersion','wasCompleted'),
# #   )
# 
# icbt_data <- icbt_data %>% 
#   filter(!is.na(transpondent_id)) %>%
#   mutate(
#     borderPostName=  str_remove_all(borderPostName, '"'),
#     gps_Timestamp = format(ymd_hms(gps_Timestamp,tz=Sys.timezone()), "%Y-%m-%d %I:%M %p"),
#     createdDate = format(ymd_hms(createdDate,tz=Sys.timezone()), "%Y-%m-%d %I:%M %p"),
#     qnr_createdDateTime = format(ymd_hms(qnr_createdDateTime,tz=Sys.timezone()), "%Y-%m-%d %I:%M %p"),
#     # gpsCapture_QnrCreated_dayGap= difftime(gps_Timestamp,qnr_createdDateTime, units = "days"), # gps_timestamp - createdDateTimeStamp
#     # gpsCapture_QnrCreated_HourGap= difftime(gps_Timestamp,qnr_createdDateTime, units = "hours") # gps_timestamp - createdDateTimeStamp
#   ) %>% 
#   mutate(  #fix wrong regionCode & regionName & team numbering assignments
#     regionCode = case_when(
#                             regionCode==16 & str_to_lower(RegionName)=='savanna'~ 13, #fix savanna with wrong region codes
#                             str_to_lower(districtName)== 'bole' & regionCode==16 ~ 13,
#                             str_to_lower(RegionName)=='northern'~ 12, # fix northern region cases with wrong region codes
#                             TRUE ~ regionCode
#                            ),
#     team_number = case_when(
#                             regionCode==1 & team_number=='WR Team 2' ~ 'WR 2',
#                             regionCode==13 & team_number=='UW Team 2' ~ 'Savannah Team 2',
#                             regionCode==13 & team_number=='UW Team 1' ~ 'Savannah Team 1',
#                             TRUE ~ team_number),
#     RegionName = case_when(str_to_lower(RegionName)=='savanna'~ 'SAVANNAH',
#                            TRUE ~ RegionName),
#     RegionName= str_to_title(RegionName),
#     districtName= str_to_title(districtName),
#     borderPostName= str_replace_all(borderPostName,'"',''), 
#     borderPostName= str_to_title(gsub("/", "-", borderPostName) ), 
#   ) %>%
#   # subset( regionCode >= user_out_data()$startRegionCode & regionCode <= user_out_data()$endRegionCode
#   #         & parse_number(team_number)>=user_out_data()$startTeamNumber &   parse_number(team_number)<=user_out_data()$endTeamNumber ) %>%
#   arrange(RegionName, districtName, townCity, borderPostName, team_number, enumerator_name ) %>%
#   mutate( #fix dataset issues
#     productObserved=  gsub("colanut", "cola nut", productObserved, ignore.case = TRUE) ,
#   )
# 
# # left_join(caseIdentifier, by = c('interview_id', 'enumerator_name', 'enumerator_contact') ) %>%
# # bind_rows(icbt_data,icbt_data_version)
# 
# #save final merged dataset
# 
# 
# # hq_icbt_data <- paste(data_dir,'ICBT_data/',sep='')
# # 
# # ifelse(!dir.exists(file.path(hq_icbt_data)),
# #        dir.create(file.path(hq_icbt_data)),
# #        "ICBT_data Directory Exists")
# # 
# # saveRDS(icbt_data, paste(hq_icbt_data,"finalData.RDS", sep = '') )
# # 
# # saveStataFileName <- paste(hq_icbt_data,'finalData.dta',sep = '')
# # readstata13::save.dta13(icbt_data, saveStataFileName)
# 
# # readstata13::save.dta13(icbt_data %>% as.data.frame(), saveStataFileName, version="15mp")
# # 
# # library(foreign)
# # write.dta(icbt_data, saveStataFileName)
# # 
# 
# # remove_val_labels(hq_icbt_data)
# 
# print("hq download and merge okay")



#####################################
##### Take data from data final folder
######################################
final_data_dir <- "Data_final/"
final_data_toLoad <- paste(fina_data_dir,'icbt_data.RDS')

load_icbt_data <- readRDS(final_data_toLoad)

daily_data <- reactiveFileReader(
  intervalMillis = 100,
  filePath       = 'path/to/shared/data',
  readFunc       = readr::read_cs
)


icbt_data <- data.frame()
for(i in 1:nrow(user_assigned_data)) {
  row <- user_assigned_data[i,]
  
  filteredDataset <-  load_icbt_data %>%
    filter(
      regionCode %in% (row$startRegionCode:row$endRegionCode)
    )  %>%
    filter(
      parse_number(team_number)>=row$startTeamNumber &   parse_number(team_number)<=row$endTeamNumber
    ) %>%
    arrange(RegionName, districtName, townCity, borderPostName, team_number, enumerator_name )
  
  icbt_data <- dplyr::bind_rows(icbt_data, filteredDataset)
  rm(filteredDataset)
  
  # print(row$startRegionCode)
}
rm(load_icbt_data)
