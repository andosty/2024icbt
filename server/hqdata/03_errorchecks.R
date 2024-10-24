#icbt_data

# Check unit of measure, quantity and selected product
# #####################################################



icbt_meta <- icbt_data %>%
  select(.,
         regionCode, RegionName,districtCode,districtName,
         townCity,borderPostName,team_number,interview_key,interview_id,enumerator_name,enumerator_contact, 
         gps_Timestamp,responsibleId,createdDate # , , 
  ) %>%
  distinct(interview_key,interview_id,responsibleId, .keep_all = T) %>%  
  arrange(districtCode,borderPostName,team_number,enumerator_name) 

errorChecks = data.frame()


blankCommdtyDesctip <- icbt_data %>% 
  filter(is.na(commodityObervedDescription)) %>%
  mutate(
    errorCheck = 'blank product description',
    errorMessage = paste("commodity/product description cannot be blank", sep = '')
  ) %>% select(.,interview_key,interview_id,observedRespondentDescription, transpondent_id,commodityObervedDescription,Commodity_id, errorCheck,errorMessage)
errorChecks <- dplyr::bind_rows(errorChecks, blankCommdtyDesctip) #add to the errorData frame file
rm(blankCommdtyDesctip)


UoM_Check <- icbt_data %>% 
  filter(!is.na(commodityObervedDescription)) %>%
  mutate(newUOM =unit_of_measure,
         cmdty_descrip = singularize(commodityObervedDescription)
  ) %>%
  separate(newUOM, c('uom1', 'uom2')) %>%
  mutate(
    uom_unit = case_when(
      is.na(uom2) & grepl("^[A-Za-z]+$",uom1)~uom1, # if uom2 is NA and uom1 contains only alpha
      !is.na(uom2) & (grepl("[A-Za-z]", uom2) & grepl("[0-9]", uom2)) & grepl("^[A-Za-z]+$",uom1)  ~uom1, # if uom2 is not NA and is either alphaNumeric or NUmeric and uom1 contains only alpha
      !is.na(uom2) & (!grepl("[A-Za-z]", uom2) & grepl("[0-9]", uom2)) & grepl("^[A-Za-z]+$",uom1)  ~ uom1, # if uom2 is not NA and is ONLY NUmeric BUT not Apla, and uom1 contains only alpha
      TRUE ~ NA_character_
    ),
    uom_size_num = case_when(
      as.numeric(gsub("\\D", "", uom1))>0 ~as.numeric(gsub("\\D", "", uom1)),
      as.numeric(gsub("\\D", "", uom2))>0 ~as.numeric(gsub("\\D", "", uom2)),
      TRUE ~ NA
    ),
    uom_deno = case_when(
      as.numeric(gsub("\\D", "", uom1))>0 & grepl("[A-Za-z]", uom1) ~ str_to_lower(sub("[^[:alpha:]]+", "", uom1)),
      as.numeric(gsub("\\D", "", uom2))>0 & grepl("[A-Za-z]", uom2) ~  str_to_lower(sub("[^[:alpha:]]+", "", uom2)),
      TRUE ~ NA
    ),
    # uom_deno=singularize(uom_deno)
  ) %>% filter(
    unit_of_measure !='Other (Specify)'
  )%>%
  select(
    # -c("uom1","uom2"),
    # enumerator_name, enumerator_contact,
    # team_number, regionCode, RegionName, districtCode, districtName, townCity,
    # borderPostName, observedRespondentDescription,
    interview_key , interview_id , 
    observedRespondentDescription,
    transpondent_id,Commodity_id, commodityObervedDescription,
    productObserved,unit_of_measure,commodityQuantity,
    uom_unit,uom_size_num,uom_deno,cmdty_descrip
  )  %>%
  mutate(	
    cmdty_descrip=singularize(cmdty_descrip),
    commodityObervedDescription=singularize(commodityObervedDescription)
  ) %>%
  cSplit(splitCols="cmdty_descrip", sep = " ")

pivotedDF <- UoM_Check %>% 
  gather(varDescription, val, -c(
    interview_key , interview_id , observedRespondentDescription,
    transpondent_id,Commodity_id, commodityObervedDescription,
    productObserved,unit_of_measure,commodityQuantity,
    uom_unit,uom_size_num,uom_deno
  ), 
  na.rm = T) %>%
  mutate(
    commodity_qty = case_when(
      as.numeric(gsub("\\D", "", val))>0 & !grepl("[A-Za-z]", val) ~ as.numeric(gsub("\\D", "", val)) ,
      TRUE ~ 0
    ),
    commodity_uom_size=case_when(
      as.numeric(gsub("\\D", "", val))>0 & grepl("[A-Za-z]", val) ~ as.numeric(gsub("\\D", "", val)),
      TRUE ~ 0
    ),
    commodity_uom_deno = case_when(
      as.numeric(gsub("\\D", "", val))>0 & grepl("[A-Za-z]", val) ~ sub("[^[:alpha:]]+", "", val),
      TRUE ~ NA_character_ 
    ),
    commodity_uom_deno=str_to_lower(commodity_uom_deno)
  )

summaryPivotedDF <- pivotedDF %>% 
  group_by(interview_key , interview_id , observedRespondentDescription,
           transpondent_id,Commodity_id, commodityObervedDescription,
           productObserved,unit_of_measure,commodityQuantity,
           uom_unit,uom_size_num,uom_deno
  ) %>%
  summarise(
    commodity_description_uom_qty = sum(commodity_qty),
    commodity_description_uom_size = sum(commodity_uom_size),
    commodity_description_uom_deno = first(commodity_uom_deno,na_rm = T)
  ) %>% mutate(
    commodity_description_uom_size = case_when(commodity_description_uom_size==0 ~ NA_real_,
                                               TRUE ~ as.numeric(commodity_description_uom_size)),
    commodity_description_uom_qty = case_when(commodity_description_uom_qty==0 ~ NA_real_,
                                              TRUE ~ commodity_description_uom_qty)
  )

getData <- summaryPivotedDF %>%
  # mutate(commodityObervedDescription0=commodityObervedDescription) %>%
  group_by(interview_key, interview_id, transpondent_id, Commodity_id) %>%
  unnest_tokens(word, commodityObervedDescription) %>%
  mutate(word=singularize(str_to_lower(word))) %>%
  summarize(text = str_c(word, collapse = " ")) %>%
  ungroup() %>%
  rename(commodityObervedDescription_singularised = text)

summaryPivotedDF <-summaryPivotedDF %>%
  left_join(getData, by=c("interview_key"="interview_key", "interview_id"="interview_id", 
                          "transpondent_id"="transpondent_id", "Commodity_id"="Commodity_id"))

commodityQtyMismatchError <- summaryPivotedDF %>% 
  ungroup() %>%
  filter(!is.na(commodity_description_uom_qty)) %>%
  filter(commodity_description_uom_qty != commodityQuantity) %>%
  mutate(
    errorCheck = 'product Qty mismatch',
    errorMessage = paste("product description implies the quantity ='",commodity_description_uom_qty, "'. observed product quantity entered = '", commodityQuantity, "'. production description = '",commodityObervedDescription,"'",sep = '')
  ) %>%
  select(.,interview_key,interview_id,observedRespondentDescription,transpondent_id,commodityObervedDescription,Commodity_id, errorCheck,errorMessage)

errorChecks <- dplyr::bind_rows(errorChecks, commodityQtyMismatchError) #add to the errorData frame file
rm(commodityQtyMismatchError)

# check commodity description not having qty in there
commodityQty_Described_missing <- summaryPivotedDF %>% 
  ungroup() %>%
  filter(is.na(commodity_description_uom_qty)) %>%
  mutate(
    errorCheck = 'missing qty description',
    errorMessage = paste("described commodity quantity was not specified. Commodity quantity observed = '", commodityQuantity,"'. product description = '",commodityObervedDescription,"'", sep = '')
  ) %>% select(.,interview_key,interview_id,observedRespondentDescription, transpondent_id,commodityObervedDescription,Commodity_id, errorCheck,errorMessage)

errorChecks <- dplyr::bind_rows(errorChecks, commodityQty_Described_missing) #add to the errorData frame file
rm(commodityQty_Described_missing)

# described a 25kg bag of rice, but selected 5kg bag of rice
UOM_not_in_description <- summaryPivotedDF %>% 
  ungroup() %>%
  select(.,interview_key,interview_id,observedRespondentDescription, transpondent_id,commodityObervedDescription,Commodity_id, uom_unit,commodityObervedDescription_singularised) %>%
  filter(!is.na(uom_unit) &  !str_detect(commodityObervedDescription_singularised,uom_unit)) %>% # unit of measure not in product description. if text in col1, exist in col2
  mutate(
    errorCheck = 'unit of measure error',
    errorMessage = paste("selected unit of measurement = '",uom_unit, "' , but commodity description is '",commodityObervedDescription,"'")
  ) %>% select(.,interview_key,interview_id,observedRespondentDescription, transpondent_id,commodityObervedDescription,Commodity_id, errorCheck,errorMessage)
errorChecks <- dplyr::bind_rows(errorChecks, UOM_not_in_description) #add to the errorData frame file
rm(UOM_not_in_description)

unit_ofMeasure_detailedDescription <- summaryPivotedDF %>% 
  ungroup() %>%
  select(.,interview_key,interview_id,observedRespondentDescription, transpondent_id,commodityObervedDescription,Commodity_id,unit_of_measure, uom_size_num,uom_deno,commodity_description_uom_size,commodity_description_uom_deno) %>%
  filter(!is.na(commodity_description_uom_deno) )  %>%
  filter(commodity_description_uom_size !=uom_size_num | commodity_description_uom_deno != uom_deno) %>%
  mutate(
    errorCheck = 'unit of measure sub-detail error',
    errorMessage = paste("The '",paste( uom_size_num,uom_deno,sep = ''),"' in the selected unit of measurement '",unit_of_measure, "' is not consistent with '",paste(commodity_description_uom_size,commodity_description_uom_deno,sep=''),"' in the commodity description of '",commodityObervedDescription,"'", sep = '')
  ) %>% select(.,interview_key,interview_id,observedRespondentDescription, transpondent_id,commodityObervedDescription,Commodity_id, errorCheck,errorMessage)
errorChecks <- dplyr::bind_rows(errorChecks, unit_ofMeasure_detailedDescription) #add to the errorData frame file
rm(unit_ofMeasure_detailedDescription)

#interviewer start date not in range
surveyDatePeriod <- read_excel("server/dictionary/dataCollectionPeriod.xlsx") %>%
  mutate(
    data_startDate =as.Date(paste(Year,'-',monthNumber,'-',startDate, sep=''), format= "%Y-%m-%d"), 
    data_endDate =as.Date(paste(Year,'-',monthNumber,'-',endDate, sep=''), format= "%Y-%m-%d") 
  )

wrongDate <- icbt_data %>% 
  mutate( gps_Timestamp= as.Date(gps_Timestamp, format= "%Y-%m-%d")  ) %>%
  subset(!(gps_Timestamp >= surveyDatePeriod$data_startDate & gps_Timestamp <= surveyDatePeriod$data_endDate)) %>% arrange(interview_key,transpondent_id)%>%
  distinct(interview_key, .keep_all = T) %>%
  mutate(
    errorCheck = 'invalid Date',
    errorMessage = paste("invalid start date of case. Gps Date taken ='",gps_Timestamp,"'",'. Error is reported on the 1st transpondent for the Case', sep = '')
  ) %>% select(.,interview_key,interview_id,observedRespondentDescription, transpondent_id,commodityObervedDescription,Commodity_id, errorCheck,errorMessage)
errorChecks <- dplyr::bind_rows(errorChecks, wrongDate) #add to the errorData frame file
rm(wrongDate)

# transpondent description less than 4 words "a man on foot"
tronspondentDescription <- icbt_data %>% 
  mutate( wordCount= stri_count_words(observedRespondentDescription)  ) %>%
  filter(wordCount<4) %>%
  mutate(
    errorCheck = 'invalid transporter description',
    errorMessage = paste("The description of transpondent must describe the person, and means of transport", sep = '')
  ) %>% select(.,interview_key,interview_id,observedRespondentDescription, transpondent_id,commodityObervedDescription,Commodity_id, errorCheck,errorMessage)
errorChecks <- dplyr::bind_rows(errorChecks, tronspondentDescription) #add to the errorData frame file
rm(wrongDate)

#transpondent sex error
genderWords <- read_excel("server/dictionary/Dictionary.xlsx", sheet='genderSynonyms') %>% 
  mutate(synonyms = trim(str_to_lower(synonyms))) %>%
  arrange(gender,synonyms) %>% 
  group_by(gender) %>%
  mutate(
    genderWords = paste(synonyms, collapse='|' )  
  ) %>% distinct(gender, .keep_all = T)

sexCheck <- icbt_data %>%
  select(.,interview_key,interview_id,observedRespondentDescription, transpondent_id,commodityObervedDescription,Commodity_id, sex) %>%
  distinct(interview_key,interview_id,observedRespondentDescription,transpondent_id, .keep_all = T) %>%
  mutate(
    checkGender = case_when(
      str_detect(sex, genderWords$gender[1]) ~ 'Female', #genderWords$gender[1] = Female
      str_detect(sex, genderWords$gender[2]) ~ 'Male',
      TRUE ~ NA_character_ )
  ) %>%
  filter(checkGender != sex) %>%
  mutate(
    errorCheck = 'gender inconsistency',
    errorMessage = paste("selected gender of respondent = '",sex, "' , but transpondent description = '",observedRespondentDescription,"'", sep = '')
  ) %>% select(.,interview_key,interview_id,observedRespondentDescription, transpondent_id,commodityObervedDescription,Commodity_id, errorCheck,errorMessage)
errorChecks <- dplyr::bind_rows(errorChecks, sexCheck) #add to the errorData frame file
rm(sexCheck)

# other specified means of transport already as a select option
transportOptions <- read_excel("server/dictionary/Dictionary.xlsx", sheet='meansOfTransport') 

otherSpecMeansTransport <-  icbt_data %>%
  select(.,interview_key,interview_id,observedRespondentDescription, transpondent_id,commodityObervedDescription,Commodity_id, transportMeans, transportMeans_otherSpecify) %>%
  filter(transportMeans =='Other (Specify)') %>%
  mutate(transportMeans_otherSpecify = trimws(trim(str_to_lower(transportMeans_otherSpecify)))) %>%
  filter(str_detect(transportMeans_otherSpecify,paste(transportOptions$means_of_transport,collapse = '|'))) %>% #  str_detect(payload, "create|drop|select"
  mutate(
    errorCheck = 'other_specified transport means error',
    errorMessage = paste("The 'other specified' transport means = '",transportMeans_otherSpecify, "', already exists as a select option", sep = '')
  ) %>% select(.,interview_key,interview_id,observedRespondentDescription, transpondent_id,commodityObervedDescription,Commodity_id, errorCheck,errorMessage)
errorChecks <- dplyr::bind_rows(errorChecks, otherSpecMeansTransport) #add to the errorData frame file
rm(otherSpecMeansTransport)

# transporter description and the means of transport not consistent
describedTransportDescriptionTransport <-  icbt_data %>%
  select(.,interview_key,interview_id,observedRespondentDescription, transpondent_id,commodityObervedDescription,Commodity_id, transportMeans, transportMeans_otherSpecify) %>%
  mutate(
    observedRespondentDescription=  gsub("motorcycle","motorbike", str_to_lower(observedRespondentDescription) ) ,
    observedRespondentDescription=  gsub("motor bike","motorbike", str_to_lower(observedRespondentDescription) ) ,
    observedRespondentDescription=  gsub("in foot","on foot", str_to_lower(observedRespondentDescription) ) ,
  )%>%
  filter( transportMeans!='Other (Specify)' & !str_detect(trimws(trim(str_to_lower(observedRespondentDescription))),as.character(str_to_lower(trim(transportMeans)))) )%>%
  mutate(
    errorCheck = 'means of transport error',
    errorMessage = paste("selected means transport  = '",transportMeans, "', is inconsistent with respondent description of '" ,observedRespondentDescription,"'. re-Check respondent description with selected means of transport", sep = '')
  ) %>% select(.,interview_key,interview_id,observedRespondentDescription, transpondent_id,commodityObervedDescription,Commodity_id, errorCheck,errorMessage)
errorChecks <- dplyr::bind_rows(errorChecks, describedTransportDescriptionTransport) #add to the errorData frame file
rm(describedTransportDescriptionTransport)

# river transport and border check
riverTransport <-  icbt_data %>%
  select(.,interview_key,interview_id,observedRespondentDescription, transpondent_id,commodityObervedDescription,Commodity_id, transportMeans, transportMeans_otherSpecify) %>%
  filter( transportMeans=='canoe' | transportMeans=='boat') %>%
  mutate(
    errorCheck = 'river transport validation',
    errorMessage = paste("Are you sure there is a river transport in crossing this border? This needs validation.", sep = '')
  ) %>% select(.,interview_key,interview_id,observedRespondentDescription, transpondent_id,commodityObervedDescription,Commodity_id, errorCheck,errorMessage)
errorChecks <- dplyr::bind_rows(errorChecks, riverTransport) #add to the errorData frame file
rm(riverTransport)

#other specified transport error
otherSpecifiedTransports <-  icbt_data %>%
  select(.,interview_key,interview_id,observedRespondentDescription, transpondent_id,commodityObervedDescription,Commodity_id, transportMeans, transportMeans_otherSpecify) %>%
  filter( transportMeans=='Other (Specify)' & !str_detect(transportMeans_otherSpecify,paste(transportOptions$means_of_transport,collapse = '|'))) %>%
  filter( grepl('barge|ship|plane|marine|yacht|helicopter|air|glider|train|rocked',transportMeans_otherSpecify, ignore.case=T)) %>%
  mutate(
    errorCheck = 'questionable transport means',
    errorMessage = paste("Are you sure there respondents used '",transportMeans_otherSpecify,"' to cross Ghana's border at your post? This needs validation.", sep = '')
  ) %>% select(.,interview_key,interview_id,observedRespondentDescription, transpondent_id,commodityObervedDescription,Commodity_id, errorCheck,errorMessage)
errorChecks <- dplyr::bind_rows(errorChecks, otherSpecifiedTransports) #add to the errorData frame file
rm(otherSpecifiedTransports)
rm(transportOptions)


#described commodity and the selected product Error
wrongSelectedProductDescription <-  icbt_data %>%
  select(.,interview_key,interview_id,observedRespondentDescription, transpondent_id,Commodity_id,commodityObervedDescription, productObserved) %>%
  filter( str_to_lower(productObserved) !='other (specify)' ) %>% mutate(productObserved0= productObserved) %>%
  cSplit(splitCols="productObserved", sep = " ") %>% #drop =F
  gather(varDescription, val, 
         -c(interview_key,interview_id,observedRespondentDescription, transpondent_id,Commodity_id,commodityObervedDescription,productObserved0
         ), 
         na.rm = T) %>%
  filter(str_length(val)>2 ) %>%  #remove two words or 1 word such as a, to , of , in, etc
  mutate(
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
    commodityObervedDescription=  gsub("ladels","ladle", str_to_lower(commodityObervedDescription) ) ,
    commodityObervedDescription=  gsub("body lotion","pomade", str_to_lower(commodityObervedDescription) ) ,
    commodityObervedDescription=  gsub("frying pan","saucepan", str_to_lower(commodityObervedDescription) ) ,
    commodityObervedDescription=  gsub("sacepan","saucepan", str_to_lower(commodityObervedDescription) ) ,
    commodityObervedDescription=  gsub("sauce pans","saucepan", str_to_lower(commodityObervedDescription) ) ,
    commodityObervedDescription=  gsub("sauce pan","saucepan", str_to_lower(commodityObervedDescription) ) ,
    commodityObervedDescription=  gsub("groudnut","groundnut", str_to_lower(commodityObervedDescription) ) ,
    commodityObervedDescription=  gsub("ground nut","groundnut", str_to_lower(commodityObervedDescription) ) ,
    commodityObervedDescription =str_replace(str_to_lower(commodityObervedDescription),"Chicken|chicken|Chick|chick", "fowl"),  #replace chicken or chick with fowl
    valRecode =gsub("[^[:alpha:]]","",val), #keep only alphanumeric characters and space in a string
    valRecode = case_when(!is.na(valRecode) & str_to_lower(valRecode)=='yrs' ~ NA, TRUE ~ valRecode),
    rootWord =str_to_lower( SnowballC::wordStem(valRecode, language = "english")),
  ) %>%
  filter(str_length(rootWord)>2 ) %>%
  arrange(interview_key,interview_id,observedRespondentDescription, transpondent_id,Commodity_id, rootWord) %>%
  mutate(
    totalCount = case_when(
      str_detect(str_to_lower(trim(commodityObervedDescription)),str_to_lower(trim(rootWord))) ~ 1, TRUE ~ 0
    )
  ) %>%
  group_by(interview_key,interview_id,observedRespondentDescription, transpondent_id,Commodity_id,commodityObervedDescription) %>%
  summarise(
    totCount = sum(totalCount),
    # rootWord= first(rootWord),
    productObserved = first(productObserved0),
    # totCount =   sum(ifelse(str_detect(str_to_lower(commodityObervedDescription),str_to_lower(rootWord)),1,0) ),
    # totalCGRE =   sum(ifelse(grepl(rootWord,commodityObervedDescription,ignore.case=T),1,0) )
  ) %>% filter(totCount==0) %>%
  mutate(
    errorCheck = 'selected product Error',
    errorMessage = paste("The selected product of '",productObserved,"' is not described in the product description of '",commodityObervedDescription,"'", sep = '')
  ) %>% select(.,interview_key,interview_id,observedRespondentDescription, transpondent_id,commodityObervedDescription,Commodity_id, errorCheck,errorMessage)
errorChecks <- dplyr::bind_rows(errorChecks, wrongSelectedProductDescription) #add to the errorData frame file
rm(wrongSelectedProductDescription)

# other specified product description
otherCommdtyDescription <- icbt_data %>%
  filter(productObserved=='Other (Specify)')



## merge error messages to meta data CASES HTTP ERROR
# 
# ICBT_metaData  <- cases %>%
#   select(id,key,assignmentId,createdDate,responsibleId ) %>%
#   rename(interview_id=id, interview_key = key)


errorChecks <- errorChecks %>%
  select(.,interview_key, interview_id, 
         transpondent_id,observedRespondentDescription,
         Commodity_id, commodityObervedDescription,
         errorCheck , errorMessage) %>%
  arrange(
    interview_key,transpondent_id, Commodity_id 
  ) %>% left_join(icbt_meta, by=c("interview_key"="interview_key","interview_id"="interview_id")) %>%
  select(.,
         regionCode, RegionName,districtCode,districtName,
         townCity,borderPostName,team_number,interview_key,interview_id,enumerator_name,enumerator_contact, everything()
  ) %>% 
  arrange(districtCode,borderPostName,team_number,enumerator_name) #  %>%
   # left_join(ICBT_metaData)


#rm(ICBT_metaData)
# 
# ############ SAVE REPORTS in Excel
# ####################################################
# 
# 
# #####################################
# #save ALL AT NATIONAL LEVEL IN ONE FILE
# # filename <-paste("../report/Error_Report.xlsx", sep="")  
# filename <-paste("../report/Error_Report.xlsx", sep="")  
# checkname <- paste("contentErrors")
# 
# #create reports directory if it does not already exist
# reports_dir <- "../report/"
# errorReports_dir <- "../report/errorReports/"
# ifelse(!dir.exists(file.path(reports_dir)),
#        dir.create(file.path(reports_dir)),
#        "REPORTS Directory Exists")
# ifelse(!dir.exists(file.path(errorReports_dir)),
#        dir.create(file.path(errorReports_dir)),
#        "ERROR REPORT Directory Exists")
# 
# 
# errorDataSave <- select(errorChecks, -c("assignmentId","responsibleId"))
# if (file.exists(filename)) {
#   wb <- loadWorkbook(filename) #load the workbook
#   #check sheet existence name(wb) gives name of sheet in wb
#   for (sheetnames in names(wb)){
#     if (sheetnames == checkname) { removeWorksheet(wb, checkname) } 
#   }
#   # if (nrow(errorDataSave)>0) { #only save for regions with the errors}
# }else{
#   wb <- createWorkbook(filename) #create the workbook if file does not exist
# }
# addWorksheet(wb,checkname)
# ## create and add a style to the column headers
# ## Freeze Panes
# freezePane(wb, checkname, firstActiveRow = 2, firstActiveCol = 7)
# 
# writeData(wb, sheet = checkname, errorDataSave, withFilter=TRUE, headerStyle = createStyle(textRotation = 45))
# saveWorkbook(wb,filename,overwrite = TRUE)
# 
# 
# #save backup for National
# national_backup_dir <- "../Other/backup/national/"
# 
# #create directory if it does not already exist
# ifelse(!dir.exists(file.path(national_backup_dir)),
#        dir.create(file.path(national_backup_dir)),
#        "National Backup Directory Exists")
# 
# nationalError_backup_dir <- "../Other/backup/national/icbt_errors_xls/"
# 
# #create directory if it does not already exist
# ifelse(!dir.exists(file.path(nationalError_backup_dir)),
#        dir.create(file.path(nationalError_backup_dir)),
#        "National Backup Directory Exists")
# national_backup_fileName <- paste(nationalError_backup_dir,"Error_Report ",current_date, ".xlsx", sep="")
# saveWorkbook(wb,national_backup_fileName,overwrite = TRUE)
# 
# 
# ##################################
# #save for each REGIONAL LEVEL
# #################################
# regionErrors <- errorChecks %>% 
#   select(RegionName,regionCode,districtCode,districtName,townCity, borderPostName,team_number) %>%
#   distinct(RegionName,districtName,townCity, borderPostName,team_number)
# 
# for (i in 1:nrow(regionErrors)  ) {
#   row <- regionErrors[i,]  #filter that region row
#   
#   saveDataSet <- errorDataSave %>% #filter errors for that region
#     filter(RegionName==row$RegionName) %>% 
#     select(., -c("regionCode","districtCode","interview_id"))
#   
#   #generate the filename for saving
#   filename <- paste("../report/errorReports/",row$RegionName," - ",row$borderPostName," - errorsReport.xlsx", sep="")
#   checkname <- paste("contentErrors")
#   if (file.exists(filename)) {
#     wb <- loadWorkbook(filename) #load the workbook
#     #check sheet existence name(wb) gives name of sheet in wb
#     for (sheetnames in names(wb)){
#       if (sheetnames == checkname) { removeWorksheet(wb, checkname) } 
#     }
#     # if (nrow(errorDataSave)>0) { #only save for regions with the errors}
#   }else{
#     wb <- createWorkbook(filename) #create the workbook if file does not exist
#   }
#   addWorksheet(wb,checkname)
#   ## create and add a style to the column headers
#   ## Freeze Panes
#   freezePane(wb, checkname, firstActiveRow = 2, firstActiveCol = 7)
#   
#   writeData(wb, sheet = checkname, saveDataSet, withFilter=TRUE, headerStyle = createStyle(textRotation = 45))
#   saveWorkbook(wb,filename,overwrite = TRUE)
#   
#   #REGIONAL LEVEL BACKUP
#   #save backup for each regional_District border
#   regional_backup_dir <- "../Other/backup/regional/"
#   regionalError_backup_dir <- "../Other/backup/regional/icbt_errors/"
#   regionalErrorDate_backup_dir <- paste("../Other/backup/regional/icbt_errors_xls/",current_dateOnly ,"/", sep = '')  
#   regional_backup_fileName <- paste(regionalErrorDate_backup_dir,row$RegionName," - ",row$borderPostName," - Error_Report " ,current_date, ".xlsx", sep="")
#   
#   #create REgional Backup directory if it does not already exist
#   ifelse(!dir.exists(file.path(regional_backup_dir)),
#          dir.create(file.path(regional_backup_dir)),
#          "Regional Backup Directory Exists")
#   
#   #create REgional Error Backup directory if it does not already exist
#   ifelse(!dir.exists(file.path(regionalError_backup_dir)),
#          dir.create(file.path(regionalError_backup_dir)),
#          "Regional Backup Backup Directory Exists")
#   
#   #create Dated REgional Error Backup directory if it does not already exist
#   ifelse(!dir.exists(file.path(regionalErrorDate_backup_dir)),
#          dir.create(file.path(regionalErrorDate_backup_dir)),
#          "Regional Backup Backup Directory Exists")
#   
#   saveWorkbook(wb,regional_backup_fileName,overwrite = TRUE)
# }
# 
# 
# 
