borderPersonStat <- icbt_data %>%
  distinct(regionCode, RegionName, districtName, borderPostName,interview_id, interview_key, enumerator_name, team_number, transpondent_id, .keep_all = T) %>%
  group_by(regionCode, RegionName, districtName, borderPostName ) %>%
  summarise(
    Transpondent_total = n(),
    #gender
    Males_total = sum(ifelse(sex=='Male',1,0)),
    Females_total = sum(ifelse(sex=='Female',1,0)),
    #flow
    Inflow_total = sum(ifelse(tradeDirection=='Coming in (Import)',1,0)),
    Inflow_male = sum(ifelse(sex=='Male' & tradeDirection=='Coming in (Import)',1,0)),
    Inflow_female = sum(ifelse(sex=='Female' & tradeDirection=='Coming in (Import)',1,0)),
    
    Outflow_total = sum(ifelse(tradeDirection=='Going out (Export)',1,0)),
    Outflow_male = sum(ifelse(sex=='Male' & tradeDirection=='Going out (Export)',1,0)),
    Outflow_female = sum(ifelse(sex=='Female' & tradeDirection=='Going out (Export)',1,0)),
    
    `NetInflow_(x-m)` = Outflow_total - Inflow_total,
    `Net_male_inflow_(x-m)` = Outflow_male - Inflow_male ,
    `Net_female_inflow_(x-m)` = Outflow_female - Inflow_female
  )

## trade by month
tradeByMonth <- icbt_data %>%
  mutate(
    monthName = lubridate::month(createdDate,label = TRUE,abbr = FALSE)
  )%>%
  distinct(regionCode, RegionName, districtName, borderPostName,interview_id, interview_key, enumerator_name, team_number, transpondent_id, .keep_all = T) %>%
  group_by(regionCode, RegionName, districtName, borderPostName )
  

## trade by commodity
# load pricing data
productCatalogue <- read_excel("OfflineReporting/Scripts/productCatalogue.xlsx") %>% 
  mutate( # fix some typo error
      Products=  gsub("colanut", "cola nut", Products, ignore.case = TRUE) ,
      `Unit of measure`=  gsub("ruuber bowl", "rubber bowl", `Unit of measure`, ignore.case = TRUE)
  ) %>% rename(
    catalogueProduct =Products,
    catalogue_unit_of_measure = "Unit of measure",
    cataloguePrice=Price,
    catalogueWeight=Weight,
    catalogueHsCode=HS
  )

# borderCommoditySummary <- icbt_data %>% 
#   mutate(
#     productObserved=  gsub("colanut", "cola nut", productObserved, ignore.case = TRUE),
#   ) %>% left_join(
#     productCatalogue , by = c("productObserved"="catalogueProduct" , "unit_of_measure"="catalogue_unit_of_measure"), 
#     relationship = "many-to-many"
#   ) %>%
#   mutate(
#     tradeValue = case_when(
#                             productObserved!='Other (specify)' & unit_of_measure !='Other (specify)' & !is.na(unit_of_measure) ~ commodityQuantity * cataloguePrice ,
#                             productObserved!='Other (specify)' & unit_of_measure =='Other (specify)' & !is.na(CommodityPrice) ~ commodityQuantity * CommodityPrice, 
#                             productObserved=='Other (specify)' ~ commodityQuantity * CommodityPrice,
#                             TRUE ~ NA
#     )
#   ) %>%
#   group_by(regionCode, RegionName, districtName, borderPostName ) %>%
#   summarise(
#     TradeVolume_total = n(),
#     `TradeVolume_Import(m)` = sum(ifelse(tradeDirection=='Coming in (Import)',1,0)),
#     `TradeVolume_Export(x)` =  sum(ifelse(tradeDirection=='Going out (Export)',1,0)),
#     
#     `TradeVolume_fromCatalogue(c)` = sum(ifelse(productObserved!='Other (specify)',1,0)),
#     TradeVolume_otherSpecified = sum(ifelse(productObserved=='Other (specify)',1,0)),
#     
#     `TradeVolume_ImportCatalogued(cm)` = sum(
#                                             ifelse(tradeDirection=='Coming in (Import)' &
#                                                      productObserved!='Other (specify)',1,0)
#                                             ),
#     `TradeVolume_ExportCatalogued(cx)` =  sum(ifelse(tradeDirection=='Going out (Export)' &
#                                                        productObserved!='Other (specify)' ,1,0)
#                                               ),
#     
#     TradeValue_total = format( sum(tradeValue), scientific = FALSE), 
#     `TradeValue_Import(m)` = format(sum(ifelse(tradeDirection=='Coming in (Import)',tradeValue,0)), scientific = FALSE) ,
#     `TradeValue_Export(x)` = format(sum(ifelse(tradeDirection=='Going out (Export)',tradeValue,0)), scientific = FALSE) ,
#     
#     `TradeValue_Import(cm)` = format(sum(ifelse(tradeDirection=='Coming in (Import)',tradeValue,0)), scientific = FALSE) ,
#     `TradeValue_Import(cx)` = format(sum(ifelse(tradeDirection=='Coming in (Import)',tradeValue,0)), scientific = FALSE) ,
#   )



borderTradeValueSummary <- icbt_data %>% 
  mutate(
    productObserved=  gsub("colanut", "cola nut", productObserved, ignore.case = TRUE),
  ) %>% left_join(
    productCatalogue , by = c("productObserved"="catalogueProduct" , "unit_of_measure"="catalogue_unit_of_measure"), 
    relationship = "many-to-many"
  ) %>%
  mutate(
    tradeValue = case_when(
      productObserved!='Other (specify)' & unit_of_measure !='Other (specify)' & !is.na(unit_of_measure) ~ commodityQuantity * cataloguePrice ,
      productObserved!='Other (specify)' & unit_of_measure =='Other (specify)' & !is.na(CommodityPrice) ~ commodityQuantity * CommodityPrice, 
      productObserved=='Other (specify)' ~ commodityQuantity * CommodityPrice,
      TRUE ~ NA
    )
  ) %>%
  group_by(regionCode, RegionName, districtName, borderPostName ) %>%
  summarise(
    TradeVolume_total = sum(commodityQuantity),
    `TradeVolume_Import(m)` = sum(ifelse(tradeDirection=='Coming in (Import)',commodityQuantity,0)),
    `TradeVolume_Export(x)` =  sum(ifelse(tradeDirection=='Going out (Export)',commodityQuantity,0)),
    `TradeVolume_net(x-m)`= `TradeVolume_Export(x)`  - `TradeVolume_Import(m)`,
    
    #Trade Qty Volume
    `TradeVolume_maleTranspondent` = sum(ifelse(sex=='Male',commodityQuantity,0)),
    `TradeVolume_malesImport` = sum(ifelse(sex=='Male' & tradeDirection=='Coming in (Import)',commodityQuantity,0)),
    `TradeVolume_malesExport` =  sum(ifelse(sex=='Male' & tradeDirection=='Going out (Export)',commodityQuantity,0)),
    
    `TradeVolume_femaleTranspondent` = sum(ifelse(sex=='Female',commodityQuantity,0)),
    `TradeVolume_femalesImport` = sum(ifelse(sex=='Female' & tradeDirection=='Coming in (Import)',commodityQuantity,0)),
    `TradeVolume_femalesExport` =  sum(ifelse(sex=='Female' & tradeDirection=='Going out (Export)',commodityQuantity,0)),
    
    `TradeVolume_fromCatalogue` = sum(ifelse(productObserved!='Other (specify)',commodityQuantity,0)),
    TradeVolume_otherSpecified = sum(ifelse(productObserved=='Other (specify)',commodityQuantity,0)),
    
    `TradeVolume_ImportCatalogued` = sum(
      ifelse(tradeDirection=='Coming in (Import)' & productObserved!='Other (specify)',commodityQuantity,0)
    ),
    `TradeVolume_ExportCatalogued` =  sum(ifelse(tradeDirection=='Going out (Export)' & productObserved!='Other (specify)' ,commodityQuantity,0)
    ),
    
    # Trade Value
    TradeValue_total = round(sum(tradeValue),2) ,# format(round(  sum(tradeValue),2), scientific = FALSE), 
    `TradeValue_Import(m)` = round(sum(ifelse(tradeDirection=='Coming in (Import)',tradeValue,0)),2) ,# format(round(sum(ifelse(tradeDirection=='Coming in (Import)',tradeValue,0)),2), scientific = FALSE) ,
    `TradeValue_Export(x)` = round(sum(ifelse(tradeDirection=='Going out (Export)',tradeValue,0)),2), # format(round(sum(ifelse(tradeDirection=='Going out (Export)',tradeValue,0)),2), scientific = FALSE) ,
    `TradeBalance(x-m)` = `TradeValue_Export(x)` -  `TradeValue_Import(m)`,
    TradeValue_maleTranspondent = round(sum(ifelse(sex=='Male',tradeValue,0))  ,2) ,# format(round(sum(ifelse(sex=='Male',tradeValue,0))  ,2), scientific = FALSE), 
    `TradeValue_maleImport` = round(sum(ifelse(sex=='Male' & tradeDirection=='Coming in (Import)',tradeValue,0)),2), # format(round(sum(ifelse(sex=='Male' & tradeDirection=='Coming in (Import)',tradeValue,0)),2), scientific = FALSE) ,
    `TradeValue_maleExport` = round(sum(ifelse(sex=='Male' & tradeDirection=='Going out (Export)',tradeValue,0)),2), # format(round(sum(ifelse(sex=='Male' & tradeDirection=='Going out (Export)',tradeValue,0)),2), scientific = FALSE) ,
    
    TradeValue_femaleTranspondent = round(sum(ifelse(sex=='Female',tradeValue,0))  ,2), # format(round(sum(ifelse(sex=='Female',tradeValue,0))  ,2), scientific = FALSE), 
    `TradeValue_femaleImport` = round(sum(ifelse(sex=='Female' & tradeDirection=='Coming in (Import)',tradeValue,0)),2), # format(round(sum(ifelse(sex=='Female' & tradeDirection=='Coming in (Import)',tradeValue,0)),2), scientific = FALSE) ,
    `TradeValue_femaleExport` = round(sum(ifelse(sex=='Female' & tradeDirection=='Going out (Export)',tradeValue,0)),2), # format(round(sum(ifelse(sex=='Female' & tradeDirection=='Going out (Export)',tradeValue,0)),2), scientific = FALSE) ,
    
    `TradeValue_fromCatalogue` = round(sum(ifelse(productObserved!='Other (specify)',tradeValue,0)),2), # format(round(sum(ifelse(productObserved!='Other (specify)',tradeValue,0)),2), scientific = FALSE) ,
    `TradeValue_otherSpecified` = round(sum(ifelse(productObserved=='Other (specify)',tradeValue,0)),2), # format(round(sum(ifelse(productObserved=='Other (specify)',tradeValue,0)),2), scientific = FALSE) ,
    
    `TradeValue_ImportCatalogued` = round( sum( ifelse(tradeDirection=='Coming in (Import)' & productObserved!='Other (specify)',tradeValue,0)),2), # format(round( sum( ifelse(tradeDirection=='Coming in (Import)' & productObserved!='Other (specify)',tradeValue,0)),2), scientific = FALSE) ,
    `TradeValue_ExportCatalogued` = round(sum(ifelse(tradeDirection=='Going out (Export)' & productObserved!='Other (specify)',tradeValue,0)),2) # format(round(sum(ifelse(tradeDirection=='Going out (Export)' & productObserved!='Other (specify)',tradeValue,0)),2), scientific = FALSE) ,
  )


borderCommodityDetailValueSummary <- icbt_data %>% 
  mutate(
    productObserved=  gsub("colanut", "cola nut", productObserved, ignore.case = TRUE),
  ) %>% left_join(
    productCatalogue , by = c("productObserved"="catalogueProduct" , "unit_of_measure"="catalogue_unit_of_measure"), 
    relationship = "many-to-many"
  ) %>%
  mutate(
    tradeValue = case_when(
                    productObserved!='Other (specify)' & unit_of_measure !='Other (specify)' & !is.na(unit_of_measure) ~ commodityQuantity * cataloguePrice ,
                    productObserved!='Other (specify)' & unit_of_measure =='Other (specify)' & !is.na(CommodityPrice) ~ commodityQuantity * CommodityPrice, 
                    productObserved=='Other (specify)' ~ commodityQuantity * CommodityPrice,
                    TRUE ~ NA
                  )
  ) %>%
  group_by(regionCode, RegionName, districtName, borderPostName,productObserved ) %>%
  summarise(
  distinct_unit_of_measures = n_distinct(unit_of_measure),
  totalTradeFrequency = n(),
  totalTradeQuantity =  sum(commodityQuantity),
  totalTradeValue = round(sum(tradeValue)  ,2),
  
  maleTradeFrequency = sum(ifelse(sex=='Male',1,0)),,
  maleTradeQuantity =  sum(ifelse(sex=='Male',commodityQuantity,0)),
  maleTradeValue =round(  sum(ifelse(sex=='Male',tradeValue,0))  ,2), 
  
  femaleTradeFrequency = sum(ifelse(sex=='Female',1,0)),,
  femaleTradeQuantity =  sum(ifelse(sex=='Female',commodityQuantity,0)),
  femaleTradeValue = round(sum(ifelse(sex=='Female',tradeValue,0)) ,2), 
  ) %>% 
  arrange(
    RegionName,districtName,productObserved
          )



borderCommodityGeneralValueSummary <- icbt_data %>% 
  mutate(
    productObserved=  gsub("colanut", "cola nut", productObserved, ignore.case = TRUE),
  ) %>% left_join(
    productCatalogue , by = c("productObserved"="catalogueProduct" , "unit_of_measure"="catalogue_unit_of_measure"), 
    relationship = "many-to-many"
  ) %>%
  mutate(
    tradeValue = case_when(
      productObserved!='Other (specify)' & unit_of_measure !='Other (specify)' & !is.na(unit_of_measure) ~ commodityQuantity * cataloguePrice ,
      productObserved!='Other (specify)' & unit_of_measure =='Other (specify)' & !is.na(CommodityPrice) ~ commodityQuantity * CommodityPrice, 
      productObserved=='Other (specify)' ~ commodityQuantity * CommodityPrice,
      TRUE ~ NA
    ),
    #generalise the products
    productObserved =  gsub("\\s*\\([^\\)]+\\)", "", productObserved),
    productObserved= gsub("[^[:alpha:][:space:]]","",productObserved),
    productObserved= str_squish(trim(trimws(productObserved))),
    productObserved = case_when(
      productObserved =="empty gallon L" ~ "empty gallon",
      productObserved =="barrels L" ~ "barrels",
      str_detect(productObserved,"fowl") ~ "fowl, cock, hen",
      TRUE ~ productObserved
    ),
     
  ) %>%
  group_by(regionCode, RegionName, districtName, borderPostName,productObserved ) %>%
  summarise(
    distinct_unit_of_measures = n_distinct(unit_of_measure),
    totalTradeFrequency = n(),
    totalTradeQuantity =  sum(commodityQuantity),
    totalTradeValue = round(sum(tradeValue)  ,2),
    
    maleTradeFrequency = sum(ifelse(sex=='Male',1,0)),,
    maleTradeQuantity =  sum(ifelse(sex=='Male',commodityQuantity,0)),
    maleTradeValue =round(  sum(ifelse(sex=='Male',tradeValue,0))  ,2), 
    
    femaleTradeFrequency = sum(ifelse(sex=='Female',1,0)),,
    femaleTradeQuantity =  sum(ifelse(sex=='Female',commodityQuantity,0)),
    femaleTradeValue = round(sum(ifelse(sex=='Female',tradeValue,0)) ,2), 
  ) %>% 
  arrange(
    RegionName,districtName,productObserved
  )

###########################
##save to excel file
###########################


#***** flow of transpondents
filename <-paste("C:/2024ICBT/OfflineReporting/Border_MonitorReport.xlsx", sep="")  
data <-borderPersonStat   %>% ungroup() %>% select(.,-regionCode)
sheetName_word <- "flow of migration"
if (file.exists(filename)) {
  wb <- loadWorkbook(filename) #load the workbook
  
  #check sheet existence name(wb) gives name of sheet in wb
  for (sheetnames in names(wb)){
    if (sheetnames == sheetName_word) {
      removeWorksheet(wb, sheetName_word)
    }
  }
  
}else{
  wb <- createWorkbook(filename) #create the workbook if file does not exist
}

addWorksheet(wb,sheetName_word)

width_vec <- apply(data, 2, function(x) max(nchar(as.character(x)) + 2, na.rm = TRUE))
width_vec_header <- nchar(colnames(data))  + 3
max_vec_header <- pmax(width_vec, width_vec_header)
setColWidths(wb, sheet = sheetName_word, cols = 1:ncol(data), widths = max_vec_header )

freezePane(wb, sheetName_word, firstActiveRow = 2, firstActiveCol = 4)

writeData(wb, sheet = sheetName_word, data, withFilter=TRUE)
saveWorkbook(wb,filename,overwrite = TRUE)



#***** Trade Summary
data <- borderTradeValueSummary   %>% ungroup() %>% select(.,-regionCode)
sheetName_word <- "trading Summary"
if (file.exists(filename)) {
  wb <- loadWorkbook(filename) #load the workbook
  
  #check sheet existence name(wb) gives name of sheet in wb
  for (sheetnames in names(wb)){
    if (sheetnames == sheetName_word) {
      removeWorksheet(wb, sheetName_word)
    }
  }
  
}else{
  wb <- createWorkbook(filename) #create the workbook if file does not exist
}

addWorksheet(wb,sheetName_word)
width_vec <- apply(data, 2, function(x) max(nchar(as.character(x)) + 2, na.rm = TRUE))
width_vec_header <- nchar(colnames(data))  + 3
max_vec_header <- pmax(width_vec, width_vec_header)
setColWidths(wb, sheet = sheetName_word, cols = 1:ncol(data), widths = max_vec_header )

freezePane(wb, sheetName_word, firstActiveRow = 2, firstActiveCol = 4)
writeData(wb, sheet = sheetName_word, data, withFilter=TRUE)
saveWorkbook(wb,filename,overwrite = TRUE)



#***** Commodity Trade Summary
data <-borderCommodityGeneralValueSummary %>% ungroup() %>% select(.,-regionCode)
sheetName_word <- "GeneralisedCommodity"
if (file.exists(filename)) {
  wb <- loadWorkbook(filename) #load the workbook
  
  #check sheet existence name(wb) gives name of sheet in wb
  for (sheetnames in names(wb)){
    if (sheetnames == sheetName_word) {
      removeWorksheet(wb, sheetName_word)
    }
  }
  
}else{
  wb <- createWorkbook(filename) #create the workbook if file does not exist
}

addWorksheet(wb,sheetName_word)
width_vec <- apply(data, 2, function(x) max(nchar(as.character(x)) + 2, na.rm = TRUE))
width_vec_header <- nchar(colnames(data))  + 3
max_vec_header <- pmax(width_vec, width_vec_header)
setColWidths(wb, sheet = sheetName_word, cols = 1:ncol(data), widths = max_vec_header )
freezePane(wb, sheetName_word, firstActiveRow = 2, firstActiveCol = 5)

writeData(wb, sheet = sheetName_word, data, withFilter=TRUE)
saveWorkbook(wb,filename,overwrite = TRUE)



#***** Commodity Trade Specifics
data <-borderCommodityDetailValueSummary %>% ungroup() %>% select(.,-regionCode)
sheetName_word <- "specific_Commodity"
if (file.exists(filename)) {
  wb <- loadWorkbook(filename) #load the workbook
  
  #check sheet existence name(wb) gives name of sheet in wb
  for (sheetnames in names(wb)){
    if (sheetnames == sheetName_word) {
      removeWorksheet(wb, sheetName_word)
    }
  }
  
}else{
  wb <- createWorkbook(filename) #create the workbook if file does not exist
}

addWorksheet(wb,sheetName_word)
width_vec <- apply(data, 2, function(x) max(nchar(as.character(x)) + 2, na.rm = TRUE))
width_vec_header <- nchar(colnames(data))  + 3
max_vec_header <- pmax(width_vec, width_vec_header)
setColWidths(wb, sheet = sheetName_word, cols = 1:ncol(data), widths = max_vec_header )
freezePane(wb, sheetName_word, firstActiveRow = 2, firstActiveCol = 5)

writeData(wb, sheet = sheetName_word, data, withFilter=TRUE)
saveWorkbook(wb,filename,overwrite = TRUE)

