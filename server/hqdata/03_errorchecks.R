#downloaded_icbt_data
#install some packages
# if(!require(labelled )) install.packages("labelled ")
# if(!require(shinyalert)) install.packages("shinyalert")
# Load the package
# library(shinyalert)
# library(bs4Dash)
library(dplyr)
library(DT)
library(foreign)
library(fresh)
library(gdata)
library(haven)
library(hcandersenr)
library(htmltools)
library(leaflet)
library(lubridate)
library(openxlsx)
library(plotly)
library(pluralize)
library(readr)
library(readxl)
# library(shiny)
# library(shinyjs)
library(SnowballC)
library(splitstackshape)
library(stopwords)
library(stringi)
library(stringr)
# library(susoapi)
# library(susoflows)
library(tidyr)
library(tidytext)
library(tidyverse)
library(tm)


# Check unit of measure, quantity and selected product
# #####################################################

icbt_meta <- downloaded_icbt_data %>%
  select(.,
         regionCode, RegionName,districtCode,districtName,
         townCity,borderPostName,team_number,interview_key,interview_id,enumerator_name,enumerator_contact, 
         gps_Timestamp,responsibleId,createdDate # , , 
  ) %>%
  distinct(interview_key,interview_id,responsibleId, .keep_all = T) %>%  
  arrange(districtCode,borderPostName,team_number,enumerator_name) 

errorChecks = data.frame()


blankCommdtyDesctip <- downloaded_icbt_data %>% 
  filter(is.na(commodityObervedDescription)) %>%
  mutate(
    errorCheck = 'blank product description',
    errorMessage = paste("commodity/product description cannot be blank", sep = '')
  ) %>% select(.,interview_key,interview_id,observedRespondentDescription, transpondent_id,commodityObervedDescription,Commodity_id, errorCheck,errorMessage)
errorChecks <- dplyr::bind_rows(errorChecks, blankCommdtyDesctip) #add to the errorData frame file
rm(blankCommdtyDesctip)

#Direction of Trade error
directionCheck <- downloaded_icbt_data %>% 
  mutate(
    observedRespondentDescription = str_squish(trim(trimws(observedRespondentDescription))),
    observedRespondentDescription = case_when(
      str_detect(str_to_lower(observedRespondentDescription),'coming on') ~  gsub('coming on', 'coming in on', observedRespondentDescription, ignore.case = TRUE),
      str_detect(str_to_lower(observedRespondentDescription),'going on') ~ gsub('going on', 'going out on', observedRespondentDescription, ignore.case = TRUE),
      TRUE ~ observedRespondentDescription
                                            )
  )
  
directionError <- directionCheck  %>%
  filter( 
    str_detect(str_to_lower(observedRespondentDescription),'coming in|going out')
  ) %>%
  distinct(interview_key, interview_id, transpondent_id, .keep_all = T) %>%
  mutate(
    direction = as.character(paste(tradeDirection)),
    # tradeDirection<- gsub('[[:punct:] ]+',' ',tradeDirection),
    direction = str_remove_all(direction,"(Export)"),
    direction = str_remove_all(direction,"(Import)"),
    direction= gsub('[[:punct:] ]+',' ',direction),
    direction = str_squish(trim(trimws(str_to_lower(direction)))),
    observedRespondentDescription= str_squish(trim(trimws(str_to_lower(observedRespondentDescription))))
  ) %>%
  mutate(
    badDir = case_when(
      # str_detect(string, pattern, negate = FALSE)
      str_detect(observedRespondentDescription,direction)  ~ 0,
      # stri_detect_fixed(observedRespondentDescription ,direction) 
      TRUE ~ 1
    )
  ) %>%
  filter(badDir==1) %>%
  mutate(
    errorCheck = 'Trade Direction Error',
    errorMessage = paste("trade direction selected ='",tradeDirection, "', but description of transpondent ='",observedRespondentDescription,"'", sep = '')
  ) %>% select(.,interview_key,interview_id,observedRespondentDescription, transpondent_id,commodityObervedDescription,Commodity_id, errorCheck,errorMessage)
errorChecks <- dplyr::bind_rows(errorChecks, directionError) #add to the errorData frame file
rm(directionError)

tradeDirectionNotSpecified <- directionCheck %>% 
  filter( 
    !str_detect(str_squish(trim(trimws(str_to_lower(observedRespondentDescription)))),'coming in|going out')
  ) %>%
  distinct(interview_key, interview_id, transpondent_id, .keep_all = T) %>%
  mutate(
    errorCheck = 'Trade Direction Missing',
    errorMessage = paste("Direction of respondent not specified in description of transpondent ='",observedRespondentDescription,"'", sep = '')
  ) %>% select(.,interview_key,interview_id,observedRespondentDescription, transpondent_id,commodityObervedDescription,Commodity_id, errorCheck,errorMessage)
errorChecks <- dplyr::bind_rows(errorChecks, tradeDirectionNotSpecified) #add to the errorData frame file
rm(tradeDirectionNotSpecified, directionCheck)

UoM_Check <- downloaded_icbt_data %>% 
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


# commodityQtyMismatchError <- downloaded_icbt_data %>%
#   select(.,interview_key,interview_id,observedRespondentDescription, transpondent_id,Commodity_id,commodityObervedDescription, productObserved,commodityQuantity) %>%
#   # filter(
#   #   (interview_key=='43-78-93-19' & transpondent_id==20) | 
#   #     (interview_key=="23-01-64-19" & transpondent_id==3) |
#   #     (interview_key=="24-15-76-11" & transpondent_id==2) |
#   #     (interview_key=="82-69-48-34" & transpondent_id==2 )
#   #   ) %>% 
#   # filter(transpondent_id==20) %>%
#   #remove brackets from commodity discriptions
#   mutate(
#     commodityObervedDescription =  gsub("\\s*\\([^\\)]+\\)", "", commodityObervedDescription), #remove parenthesis
#     cmdityDesc = str_squish(trim(trimws(str_to_lower(commodityObervedDescription)))) #trim out all unnecessary spaces
#   ) %>%
#   # convert each work to column and then gather
#   cSplit(splitCols="cmdityDesc", sep = " ") %>%
#   gather(varDescription, val, 
#          -c(interview_key,interview_id,observedRespondentDescription, transpondent_id,Commodity_id,commodityObervedDescription,productObserved,commodityQuantity
#          ), 
#          na.rm = T) %>%
#   #keep columns vals that are only numeric quantityes 
#   mutate(val=removePunctuation(val)) %>%
#   mutate_at(c('val'), ~na_if(., '')) %>%
#   filter(!grepl("[A-Za-z]", val) & !is.na(val)) %>%
#   filter(
#     commodityQuantity !=val
#   ) %>%
#   mutate(
#     errorCheck = 'product Qty mismatch',
#     errorMessage = paste("product description implies the quantity ='",val, "'. commodityQuantity entered = '", commodityQuantity, "'. production description = '",commodityObervedDescription,"'",sep = '')
#   ) %>%
#   select(.,interview_key,interview_id,observedRespondentDescription,transpondent_id,commodityObervedDescription,Commodity_id, errorCheck,errorMessage)

commodityQtyMismatchError <- downloaded_icbt_data %>%
  select(.,interview_key,interview_id,observedRespondentDescription, transpondent_id,Commodity_id,commodityObervedDescription, productObserved,commodityQuantity) %>%
  filter(
    (interview_key=='74-36-02-42' )
    # |
    #   (interview_key=="23-01-64-19" & transpondent_id==3) |
    #   (interview_key=="24-15-76-11" & transpondent_id==2) |
    #   (interview_key=="82-69-48-34" & transpondent_id==2 )
  ) %>%
  # filter(transpondent_id==20) %>%
  #remove brackets from commodity discriptions
  mutate(
    commodityObervedDescription =  gsub("\\s*\\([^\\)]+\\)", "", commodityObervedDescription), #remove parenthesis
    cmdityDesc = str_squish(trim(trimws(str_to_lower(commodityObervedDescription)))) #trim out all unnecessary spaces
  ) %>%
  # convert each work to column and then gather
  cSplit(splitCols="cmdityDesc", sep = " ") %>%
  gather(varDescription, val, 
         -c(interview_key,interview_id,observedRespondentDescription, transpondent_id,Commodity_id,commodityObervedDescription,productObserved,commodityQuantity
         ), 
         na.rm = T) %>%
  #keep columns vals that are only numeric quantityes 
  mutate(val=removePunctuation(val)) %>%
  mutate_at(c('val'), ~na_if(., '')) %>%
  filter(!grepl("[A-Za-z]", val) & !is.na(val)) %>%
  group_by(interview_key,interview_id,observedRespondentDescription,transpondent_id,commodityObervedDescription,Commodity_id,commodityQuantity) %>%
  summarise(
    qtyMismatch = sum(ifelse(commodityQuantity ==val,1,0)),
    val=val
  ) %>% 
  ungroup() %>%
  filter(qtyMismatch==0  ) %>%
  mutate(
    errorCheck = 'product Qty mismatch',
    errorMessage = paste("product description implies the quantity ='",val, "'. commodityQuantity entered = '", commodityQuantity, "'. production description = '",commodityObervedDescription,"'",sep = '')
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

dateCheck <- downloaded_icbt_data %>% 
  select(interview_key,interview_id, gps_Timestamp) %>%
  distinct(interview_key, .keep_all = T) %>%
  mutate( gps_Timestamp= ymd(as.Date(gps_Timestamp, format= "%Y-%m-%d"))  ) 
  
  surveyDatePeriod <- read_excel("server/dictionary/dataCollectionPeriod.xlsx") %>%
    mutate(
      data_startDate =ymd(paste(Year,'-',monthNumber,'-',startDay, sep='')), 
      data_endDate =ymd(paste(Year,'-',monthNumber,'-',endDay, sep='')) ,
      # ps=as.character(paste(
      #                       seq(
      #                           as.Date(as.character(data_startDate)),as.Date(as.character(data_endDate)),
      #                           by = "day")
      #                         , collapse = " ")
      #                 )
      # ps = paste(seq(as.Date(data_startDate,format= "%Y-%m-%d"),as.Date(data_endDate,format= "%Y-%m-%d"),by = "day"))
      # numStart = as.numeric(data_startDate),
      # numEnd = as.numeric(data_endDate),
    )
  
ValidDatedCase = data.frame()  
  
# casesInValidDate
for (i in 1:nrow(surveyDatePeriod)) {
    period <- surveyDatePeriod[i,]
    startDate <- as.character(period$data_startDate)
    endDate  <- as.character(period$data_endDate)
    dateVals = seq(as.Date(startDate,format= "%Y-%m-%d"), as.Date(endDate,format= "%Y-%m-%d"), by = "day")
    
    okayDatedCase <- dateCheck %>%
      subset(
        (gps_Timestamp %in% dateVals )
      )
    ValidDatedCase = rbind(ValidDatedCase,okayDatedCase)
}

#invalidDatedCases
wrongDate <- downloaded_icbt_data %>% 
  filter(
    !(interview_key %in% ValidDatedCase$interview_key)
  ) %>% distinct(interview_key, interview_id , .keep_all = T) %>%
  mutate(
    errorCheck = 'invalid Date',
    errorMessage = paste("invalid start date of case. Gps Date taken ='",gps_Timestamp,"'",'. Error is reported on the 1st transpondent for the Case', sep = '')
  ) %>% select(.,interview_key,interview_id,observedRespondentDescription, transpondent_id,commodityObervedDescription,Commodity_id, errorCheck,errorMessage)
errorChecks <- dplyr::bind_rows(errorChecks, wrongDate) #add to the errorData frame file
rm(wrongDate)

# transpondent description less than 4 words "a man on foot"
tronspondentDescription <- downloaded_icbt_data %>% 
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

sexCheck <- downloaded_icbt_data %>%
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

otherSpecMeansTransport <-  downloaded_icbt_data %>%
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
describedTransportDescriptionTransport <-  downloaded_icbt_data %>%
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
# riverTransport <-  downloaded_icbt_data %>%
#   select(.,interview_key,interview_id,observedRespondentDescription, transpondent_id,commodityObervedDescription,Commodity_id, transportMeans, transportMeans_otherSpecify) %>%
#   filter( transportMeans=='canoe' | transportMeans=='boat') %>%
#   mutate(
#     errorCheck = 'river transport validation',
#     errorMessage = paste("Are you sure there is a river transport in crossing this border? This needs validation.", sep = '')
#   ) %>% select(.,interview_key,interview_id,observedRespondentDescription, transpondent_id,commodityObervedDescription,Commodity_id, errorCheck,errorMessage)
# errorChecks <- dplyr::bind_rows(errorChecks, riverTransport) #add to the errorData frame file
# rm(riverTransport)

#other specified transport error
otherSpecifiedTransports <-  downloaded_icbt_data %>%
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
# wrongSelectedProductDescription <-  downloaded_icbt_data %>%
#   filter( str_to_lower(productObserved) !='other (specify)' ) %>% 
#   mutate(
#     productObserved = case_when( productObserved=="lining (linen)" ~  "lining",
#                                 TRUE ~  productObserved
#                          ),
#     productObserved = str_to_lower(productObserved),
#     commodityObervedDescription = str_squish(trim(trimws(str_to_lower(commodityObervedDescription)))),
#     # productObserved = str_squish(trim(trimws(gsub("\\s*\\([^\\)]+\\)", "",singularize(productObserved) ))))
#   ) %>%
#   select(.,interview_key,interview_id,observedRespondentDescription, transpondent_id,Commodity_id,commodityObervedDescription, productObserved) %>%
#   mutate(productObserved0= productObserved) %>%
#   cSplit(splitCols="productObserved", sep = " ") %>% #drop =F
#   gather(varDescription, val, 
#          -c(interview_key,interview_id,observedRespondentDescription, transpondent_id,Commodity_id,commodityObervedDescription,productObserved0
#          ), 
#          na.rm = T) %>%
#   filter(str_length(val)>2 ) %>%  #remove two words or 1 word such as a, to , of , in, etc
#   mutate(
#     # commodityObervedDescription=  gsub("under wear","underwear", str_to_lower(commodityObervedDescription) ) ,
#     # commodityObervedDescription=  gsub("under wears","underwear", str_to_lower(commodityObervedDescription) ) ,
#     # commodityObervedDescription=  gsub("okra","okro", str_to_lower(commodityObervedDescription) ) ,
#     # commodityObervedDescription=  gsub("alefo","alefu", str_to_lower(commodityObervedDescription) ) ,
#     # commodityObervedDescription=  gsub("water melon","watermelon", str_to_lower(commodityObervedDescription) ) ,
#     # commodityObervedDescription=  gsub(" tea ","lipton (teabag)", str_to_lower(commodityObervedDescription) ) ,
#     # commodityObervedDescription=  gsub("local tea","lipton (teabag)", str_to_lower(commodityObervedDescription) ) ,
#     # commodityObervedDescription=  gsub("kitchen ware","Kitchenware", str_to_lower(commodityObervedDescription) ) ,
#     # commodityObervedDescription=  gsub("kitchen wares","Kitchenware", str_to_lower(commodityObervedDescription) ) ,
#     # commodityObervedDescription=  gsub("alcohol in gallons","akpeteshie", str_to_lower(commodityObervedDescription) ) ,
#     # commodityObervedDescription=  gsub("alcohol in gallon","akpeteshie", str_to_lower(commodityObervedDescription) ) ,
#     # commodityObervedDescription=  gsub("cyclinder","cylinder", str_to_lower(commodityObervedDescription) ) ,
#     # commodityObervedDescription=  gsub("barber machine","hair accessories", str_to_lower(commodityObervedDescription) ) ,
#     # commodityObervedDescription=  gsub("ayilo","ayilor", str_to_lower(commodityObervedDescription) ) ,
#     # commodityObervedDescription=  gsub("ladels","ladle", str_to_lower(commodityObervedDescription) ) ,
#     # commodityObervedDescription=  gsub("body lotion","pomade", str_to_lower(commodityObervedDescription) ) ,
#     # commodityObervedDescription=  gsub("frying pan","saucepan", str_to_lower(commodityObervedDescription) ) ,
#     # commodityObervedDescription=  gsub("sacepan","saucepan", str_to_lower(commodityObervedDescription) ) ,
#     # commodityObervedDescription=  gsub("sauce pans","saucepan", str_to_lower(commodityObervedDescription) ) ,
#     # commodityObervedDescription=  gsub("sauce pan","saucepan", str_to_lower(commodityObervedDescription) ) ,
#     # commodityObervedDescription=  gsub("groudnut","groundnut", str_to_lower(commodityObervedDescription) ) ,
#     # commodityObervedDescription=  gsub("ground nut","groundnut", str_to_lower(commodityObervedDescription) ) ,
#     # commodityObervedDescription =str_replace(str_to_lower(commodityObervedDescription),"chicken|chick", "fowl"),  #replace chicken or chick with fowl
#     valRecode =gsub("[^[:alpha:]]","",val), #keep only alphanumeric characters and space in a string
#     valRecode = case_when(!is.na(valRecode) & str_to_lower(valRecode)=='yrs' ~ NA, TRUE ~ valRecode),
#     # rootWordA = singularize(valRecode),
#     rootWord =str_to_lower( SnowballC::wordStem(valRecode, language = "english")),
#   ) %>%
#   filter(str_length(rootWord)>2 ) %>% filter(!is.na(valRecode)) %>%
#   arrange(interview_key,interview_id,observedRespondentDescription, transpondent_id,Commodity_id, rootWord) %>%
#   mutate(
#     totalCount = case_when(
#       str_detect(str_to_lower(trim(commodityObervedDescription)),singularize(str_to_lower(trim(valRecode)))) ~ 1,
#       str_detect(str_to_lower(trim(commodityObervedDescription)),(trim(SnowballC::wordStem(rootWord, language = "english") ))) ~ 1,
#       TRUE ~ 0
#     )
#   ) %>%
#   group_by(interview_key,interview_id,observedRespondentDescription, transpondent_id,Commodity_id,commodityObervedDescription) %>%
#   summarise(
#     totCount = sum(totalCount),
#     # rootWord= first(rootWord),
#     productObserved = first(productObserved0),
#     # totCount =   sum(ifelse(str_detect(str_to_lower(commodityObervedDescription),str_to_lower(rootWord)),1,0) ),
#     # totalCGRE =   sum(ifelse(grepl(rootWord,commodityObervedDescription,ignore.case=T),1,0) )
#   ) %>% filter(totCount==0) %>%
#   mutate(
#     errorCheck = 'selected product Error',
#     errorMessage = paste("The selected product of '",productObserved,"' is not described in the product description of '",commodityObervedDescription,"'", sep = '')
#   ) %>% select(.,interview_key,interview_id,observedRespondentDescription, transpondent_id,commodityObervedDescription,Commodity_id, errorCheck,errorMessage)
# errorChecks <- dplyr::bind_rows(errorChecks, wrongSelectedProductDescription) #add to the errorData frame file
# rm(wrongSelectedProductDescription)

wrongSelectedProductDescription <-  downloaded_icbt_data %>%
  filter( str_to_lower(productObserved) !='other (specify)' ) %>% 
  mutate(productObserved = str_to_lower(productObserved)) %>%
  select(.,interview_key,interview_id,observedRespondentDescription, transpondent_id,Commodity_id,commodityObervedDescription, productObserved)

wrgProdSel1 <- wrongSelectedProductDescription %>% #  filter(interview_key=='01-49-75-84' & transpondent_id==21 ) %>%
  select(.,interview_key,interview_id,observedRespondentDescription, transpondent_id,Commodity_id,commodityObervedDescription, productObserved) %>%
  filter( #take oneword products
    str_count(productObserved, '\\w+') == 1
  ) %>%
  mutate(
    productObserved = singularize(productObserved),
    cmsdesc =  sub("\\(", " (", commodityObervedDescription),
    cmsdesc =  sub("\\)", ") ", cmsdesc),
    cmsdesc = str_squish(cmsdesc)
  ) %>%
  #now take the product description and also singularise it
  cSplit(splitCols="cmsdesc", sep = " ") %>%
  gather(varDescription, val, 
         -c(interview_key,interview_id,observedRespondentDescription, transpondent_id,Commodity_id,commodityObervedDescription,productObserved
         ), 
         na.rm = T) %>%
  filter(str_length(val)>2 & grepl("[A-Za-z]", val)) %>% #remove vals with only numbers out
  mutate(
    valNew= gsub("\\s*\\([^\\)]+\\)", "", str_to_lower(val)), #remove parenthesis
    valNew = removePunctuation(valNew),
    
    val = (gsub("\\(|\\)", "", str_to_lower(removePunctuation(val)))),
    productObserved = (gsub("\\(|\\)", "", str_to_lower(productObserved))),
    # val = (gsub("\\(|\\)", "", str_to_lower(val))),
    
    
    # productObserved = singularize(productObserved),
    # val = ,
    totCountCheck = case_when(
      #valNew
      #############
      str_detect(productObserved, valNew )  ~ 1,
      str_detect(productObserved,  singularize(valNew) )  ~ 1,
      str_detect( singularize(productObserved), valNew )  ~ 1 ,
      str_detect( singularize(productObserved), singularize(valNew) )  ~ 1,
      
      # str_detect(productObserved, singularize(str_to_lower(valNew)) )  ~ 1,
      str_detect(productObserved, SnowballC::wordStem((valNew), language = "english"))  ~ 1,
      str_detect(SnowballC::wordStem((productObserved), language = "english"), valNew)  ~ 1,
      str_detect(SnowballC::wordStem((productObserved), language = "english"), SnowballC::wordStem((valNew), language = "english"))  ~ 1,
      
      #val
      ########
      str_detect(productObserved, val )  ~ 1,
      str_detect(productObserved,  singularize(val) )  ~ 1,
      str_detect( singularize(productObserved), val )  ~ 1,
      str_detect( singularize(productObserved), singularize(val) )  ~ 1,
      
      # str_detect(productObserved, singularize(str_to_lower(val)) )  ~ 1,
      str_detect(productObserved, SnowballC::wordStem((val), language = "english"))  ~ 1,
      str_detect(SnowballC::wordStem((productObserved), language = "english"), val)  ~ 1,
      str_detect(SnowballC::wordStem((productObserved), language = "english"), SnowballC::wordStem((val), language = "english"))  ~ 1,
      
      TRUE ~ 0
    )
  )  %>%
  group_by(interview_key,interview_id,observedRespondentDescription, transpondent_id,Commodity_id,commodityObervedDescription) %>%
  summarise(
    productObserved = first(productObserved),
    totCount = sum(totCountCheck)
  ) %>%
  filter(totCount==0) %>%
  mutate(
    errorCheck = 'selected product Error',
    errorMessage = paste("The selected product of '",productObserved,"' is not described in the product description of '",commodityObervedDescription,"'", sep = '')
  ) %>% 
  select(.,interview_key,interview_id,observedRespondentDescription, transpondent_id,commodityObervedDescription,Commodity_id, errorCheck,errorMessage)
errorChecks <- dplyr::bind_rows(errorChecks, wrgProdSel1) #add to the errorData frame file
rm(wrgProdSel1)


wrgProdSel2 <- wrongSelectedProductDescription  %>% #filter(interview_key=='76-21-67-79') %>%
  select(.,interview_key,interview_id,observedRespondentDescription, transpondent_id,Commodity_id,commodityObervedDescription, productObserved) %>%
  filter( #take more than one-word products
    str_count(productObserved, '\\w+') > 1
  ) %>%
  # slice(1:100) %>% # keep or use top 100
  mutate(
    cmdyDesc=  gsub(","," , ", str_to_lower(commodityObervedDescription) ) ,
    productObserved=  gsub(","," , ", str_to_lower(productObserved) ) ,
  ) %>% #># split and collect commodity description into singular, then join it back
  cSplit(splitCols="cmdyDesc", sep = " ") %>%
  gather(varDescription, val, 
         -c(interview_key,interview_id,observedRespondentDescription, transpondent_id,Commodity_id,commodityObervedDescription,productObserved
         ), 
         na.rm = T) %>%
  mutate(
    val = singularize(str_to_lower(val)),
  ) %>%
  group_by(interview_key,interview_id,observedRespondentDescription, transpondent_id,Commodity_id,commodityObervedDescription,productObserved) %>%
  summarise(
    cmdtyDescription = paste(val, collapse = " "),
    prodSel = first(productObserved)
  ) %>% #now to split the product
  cSplit(splitCols="prodSel", sep = " ") %>%
  gather(varDescription, val, 
         -c(interview_key,interview_id,observedRespondentDescription, transpondent_id,Commodity_id,commodityObervedDescription,productObserved,cmdtyDescription
         ), 
         na.rm = T) %>%
  filter(str_length(val)>2 & grepl("[A-Za-z]", val)) %>% #remove vals with only numbers out
  mutate(
    val = (gsub("\\(|\\)", "", str_to_lower(val))),
    totCountCheckok = case_when( 
      str_detect(cmdtyDescription,val) ~ 1,
      str_detect(cmdtyDescription,singularize(val)) ~ 1,
      str_detect(cmdtyDescription,SnowballC::wordStem(val, language = "english")) ~ 1,
      TRUE ~ 0  
    )
  )  %>%
  group_by(interview_key,interview_id,observedRespondentDescription, transpondent_id,Commodity_id,commodityObervedDescription,productObserved) %>%
  summarise(
    # productObserved = first(productObserved),
    totCount = sum(totCountCheckok) #sum(ifelse(str_detect(cmdtyDescription, gsub("\\(|\\)", "", val)  ), 1,0))
  ) %>%
  filter(totCount==0) %>%
  mutate(
    errorCheck = 'selected product Error',
    errorMessage = paste("The selected product of '",productObserved,"' is not described in the product description of '",commodityObervedDescription,"'", sep = '')
  ) %>% 
  select(.,interview_key,interview_id,observedRespondentDescription, transpondent_id,commodityObervedDescription,Commodity_id, errorCheck,errorMessage)
errorChecks <- dplyr::bind_rows(errorChecks, wrgProdSel2) #add to the errorData frame file
rm(wrgProdSel2, wrongSelectedProductDescription)


# other specified product description
otherCommdtyDescriptionKK <- downloaded_icbt_data %>%
  filter(productObserved=='Other (specify)')

otherSPecifiedError1 <-otherCommdtyDescriptionKK %>%
  filter( #is not alpha but only numeric productObserved_otherSpecify
    !str_detect(productObserved_otherSpecify, "^[:alpha:]+$")  & # product is not alphabets
    str_detect(productObserved_otherSpecify, "^[:digit:]+$") # product is numbers
    # !grepl("^[A-Za-z]+$", productObserved_otherSpecify)  & # product is not alphabets
    # grepl("\\D", productObserved_otherSpecify)   # product is numbers
  ) %>%
mutate(
  errorCheck = 'other Specified Error',
  errorMessage = paste("other specified commodity cannot be numeric", sep = '')
) %>% 
  select(.,interview_key,interview_id,observedRespondentDescription, transpondent_id,commodityObervedDescription,Commodity_id, errorCheck,errorMessage)
errorChecks <- dplyr::bind_rows(errorChecks, otherSPecifiedError1) #add to the errorData frame file
rm(otherSPecifiedError1)

otherSPecifiedError2 <-otherCommdtyDescriptionKK %>%
  filter( #strings already existing as select option
    str_detect(str_to_lower(productObserved_otherSpecify),
               "alcohol|fertiliser|fertilizer|chicken|chicks|cock|gas cylinder|pick axe|axe|coconut oil|battery|milk|motor"
               ) |  #excluded wood
     str_to_lower(productObserved_otherSpecify)=="water"
  ) %>%
mutate(
  errorCheck = 'other Specified Error',
  errorMessage = paste("other specified commodity ='",productObserved_otherSpecify,"', already exists as a select option", sep = '')
) %>% 
  select(.,interview_key,interview_id,observedRespondentDescription, transpondent_id,commodityObervedDescription,Commodity_id, errorCheck,errorMessage)
errorChecks <- dplyr::bind_rows(errorChecks, otherSPecifiedError2) #add to the errorData frame file
rm(otherSPecifiedError2)
  

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


rm(UoM_Check, tronspondentDescription,surveyDatePeriod,pivotedDF,otherCommdtyDescription,icbt_meta,getData,genderWords,summaryPivotedDF)

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