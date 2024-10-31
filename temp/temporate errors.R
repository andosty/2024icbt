library(dplyr)

direction <- icbt_data %>% 
             filter( 
               str_detect(observedRespondentDescription,'coming in|going out')
               # str_detect(observedRespondentDescription,'coming in') | str_detect(observedRespondentDescription,'going out') 
               # grepl('coming in|going out', observedRespondentDescription, ignore.case=TRUE) 
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


directionNotSpecified <- icbt_data %>% 
  filter( 
    !str_detect(str_squish(trim(trimws(str_to_lower(observedRespondentDescription)))),'coming in|going out')
  ) 



# filter for animals and check the gender of animal and age
