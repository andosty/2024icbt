newDataMeta <- select(read_rds("server/users.RDS"),UserId, UserName) %>%  
  rename(responsibleId = UserId) %>% distinct(responsibleId, .keep_all = T)

userID_in_Data <-  read_dta(paste(hq_extracted_dir,"assignment__actions.dta",sep = '')) %>% select(
  assignment__id, responsible__name
) %>% rename(
  UserName =  responsible__name
)  %>% distinct(UserName, .keep_all = T)

MajorMeta <- left_join(userID_in_Data,newDataMeta) %>% rename(assignment_id = assignment__id)   



#create directory for Extracting data download, unzipped and prepped if not exist


#create ICBT_data Director in Data folder if it does not already exist
hq_icbt_data <- paste(data_dir,'ICBT_data/',sep='')

ifelse(!dir.exists(file.path(hq_icbt_data)),
       dir.create(file.path(hq_icbt_data)),
       "ICBT_data Directory Exists")


# take case with meta data
library(haven)

metaColNames <- c("interview__key", "interview__id","enumerator_name" ,"enumerator_contact", "team_number" ,      
                   "id02","id02a","id03","id03b","id04",
                   "id06","gps__Latitude","gps__Longitude","gps__Accuracy","gps__Altitude",
                   "gps__Timestamp","assignment__id"
                  )

transpondentNames <- c("interview__key","interview__id","transportercharacteristics__id",
                       "s2q1","s2q2","s2q3",
                       "s2q3ll","s2q3llo"
                      )

icbt_data <- read_dta(paste(hq_extracted_dir,server_qnr$variable,".dta", sep = '')) %>%
    select(all_of(metaColNames)) %>%
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
        s2q10a = haven::as_factor(s2q10a)
      ),
    by=c("interview__key"="interview__key",
         "interview__id"="interview__id",
         "transportercharacteristics__id"="transportercharacteristics__id"
         )
  ) %>% rename(
    assignment_id = assignment__id
  ) %>% left_join(MajorMeta) 

#Pull HH level var renames file
renameData <- read_excel("server/dictionary/varNames.xlsx",sheet = "icbtVarRenames")

for (i in 1:nrow(renameData)  ) {
  row <- renameData[i,]  #filter that row
  if(row$oldVarName %in% names(icbt_data)){ #if name var exists, then rename the var
    icbt_data <- rename.vars(icbt_data, from =row$oldVarName, to = row$newVarName)
  }
}

rm(renameData, metaColNames, transpondentNames, row, server_qnr,sqlSvr)


colnames(icbt_data) = gsub("__", "_", colnames(icbt_data))

# caseIdentifier <-cases %>% rename(interview_id = id) %>%
#   select(
# c('key', 'interview_id', 'createdDate','updateDateUtc',
#   'enumerator_name', 'enumerator_contact','responsibleId',
#   'questionnaireVersion','wasCompleted'),
#   )


  

icbt_data <- icbt_data %>%
  # left_join(caseIdentifier, by = c('interview_id', 'enumerator_name', 'enumerator_contact') ) %>%
  subset( regionCode >= user_out_data()$startRegionCode & regionCode <= user_out_data()$endRegionCode )

# # export your dataset:
# library(foreign)
# saveRDS(icbt_data , paste(hq_icbt_data,'finalData.rds',sep = '')) 
# saveStataFileName <- paste(hq_icbt_data,'finalData.dta',sep = '')
# readstata13::save.dta13(icbt_data, saveStataFileName, version="15mp")
# 
# #save backup of Dataset and cases
# backup_dir <- "../Other/backup/"
# dataset_backup_dir <- "../Other/backup/dataset/"
# dataset_backup_fileName <- paste(dataset_backup_dir,"finalData " ,current_date, ".dta", sep="")
# 
# #create Backup directory in other folder if it does not already exist
# ifelse(!dir.exists(file.path(backup_dir)),
#        dir.create(file.path(backup_dir)),
#        "Backup Directory Exists")
# 
# #create Dataset Director in Backup folder if it does not already exist
# ifelse(!dir.exists(file.path(dataset_backup_dir)),
#        dir.create(file.path(dataset_backup_dir)),
#        "Dataset Backup Backup Directory Exists")
# 
# readstata13::save.dta13(icbt_data, dataset_backup_fileName , version="15mp")
# 
# # save cases to rds and stata
# ##################################
