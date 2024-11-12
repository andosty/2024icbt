# product Qty mismatch
 setwd("C:/2024ICBT/")
 
 
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
 
errorDataFile <- paste('Data_final/','icbt_data.RDS' , sep = '')


downloaded_icbt_data <- readRDS(errorDataFile)
  
  
commodityQtyMismatchError <- downloaded_icbt_data %>%
  select(.,interview_key,interview_id,observedRespondentDescription, transpondent_id,Commodity_id,commodityObervedDescription, productObserved,commodityQuantity) %>%
  filter(
    (interview_key=='61-69-17-20' & transpondent_id==27) 
  #   (interview_key=='43-78-93-19' & transpondent_id==20) | 
  #   (interview_key=='43-78-93-19' & transpondent_id==20) | 
  #     (interview_key=="23-01-64-19" & transpondent_id==3) |
  #     (interview_key=="24-15-76-11" & transpondent_id==2) |
  #     (interview_key=="82-69-48-34" & transpondent_id==2 )
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
  filter(
    commodityQuantity !=val
  ) %>%
  mutate(
    errorCheck = 'product Qty mismatch',
    errorMessage = paste("product description implies the quantity ='",val, "'. commodityQuantity entered = '", commodityQuantity, "'. production description = '",commodityObervedDescription,"'",sep = '')
  ) %>%
  select(.,interview_key,interview_id,observedRespondentDescription,transpondent_id,commodityObervedDescription,Commodity_id, errorCheck,errorMessage)

errorChecks <- dplyr::bind_rows(errorChecks, commodityQtyMismatchError) #add to the errorData frame file
rm(commodityQtyMismatchError)


