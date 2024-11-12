# selected product error

downloaded_icbt_data <- read_rds("Data_final/icbt_data.RDS")

## creating the other spec list
#########################################
otherspec_non_OtherSpecUoM <-  downloaded_icbt_data %>%
  filter( str_to_lower(productObserved) =='other (specify)' ) %>%
  filter(unit_of_measure != "Other (specify)" )   %>% 
    distinct(productObserved, productObserved_otherSpecify, unit_of_measure) 
  

otherspec_uom_otherSpec <-  downloaded_icbt_data %>%
  filter( str_to_lower(productObserved) =='other (specify)' ) %>%
  filter(unit_of_measure == "Other (specify)" )   %>% 
    distinct(productObserved, productObserved_otherSpecify, unit_of_measure_otherSpecify) 
  
otherSpecList <- bind_rows(otherspec_non_OtherSpecUoM,otherspec_uom_otherSpec ) %>%
  mutate_if(is.character, ~replace_na(.,"")) %>%
  mutate(
    unit_of_measures=str_to_lower(str_squish(trim( paste(unit_of_measure,unit_of_measure_otherSpecify )))),
    productObserved_otherSpecify =str_to_lower(productObserved_otherSpecify),
    
    unit_of_measures=  gsub("na ","", str_to_lower(unit_of_measures) ) ,
    
    productObserved_otherSpecify=  gsub("bittersalt","bitter salt", str_to_lower(productObserved_otherSpecify) ) ,
    productObserved_otherSpecify=  gsub(" adorde","adorde", str_to_lower(productObserved_otherSpecify) ) ,
    productObserved_otherSpecify=  gsub("mosquito  coil","mosquito coil", str_to_lower(productObserved_otherSpecify) ) ,
    productObserved_otherSpecify=  gsub("beer ","beer", str_to_lower(productObserved_otherSpecify) ) ,
    productObserved_otherSpecify=  gsub("clams ( adorde)","clams (adorde)", str_to_lower(productObserved_otherSpecify) ) ,
    productObserved_otherSpecify=  gsub("donkeys","donkey", str_to_lower(productObserved_otherSpecify) ) ,
    productObserved_otherSpecify=  gsub("second hand","second-hand", str_to_lower(productObserved_otherSpecify) ) ,
    productObserved_otherSpecify=  gsub(" 1yr ","1yr", str_to_lower(productObserved_otherSpecify) ) ,
    productObserved_otherSpecify=  gsub("1yrto","1yr to", str_to_lower(productObserved_otherSpecify) ) ,
    
    # productObserved_otherSpecify=  gsub("\\([^\\)]","(", str_to_lower(productObserved_otherSpecify) ) ,
    
    productObserved_otherSpecify= case_when(
      str_detect(productObserved_otherSpecify,"sand") ~ "a trip of sand",
      TRUE ~ productObserved_otherSpecify
    ),
    productObserved_otherSpecify = str_squish(trim(trimws(productObserved_otherSpecify)))
    
    ) %>%
  distinct(productObserved, productObserved_otherSpecify,unit_of_measures)

write.xlsx(otherSpecList,"otherspec_nonOtherUOM.xlsx")

#########################################

  "Other (specify)"
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
  rm(wrgProdSel2)

  29 cases
  
  76-21-67-79
05-18-96-28

180


# write.xlsx(otherspec_nonOtherUnit,"otherspec_nonOtherUOM.xlsx")
# 