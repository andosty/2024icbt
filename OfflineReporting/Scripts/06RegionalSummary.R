regionalPersonStat <- borderPersonStat %>% 
  ungroup() %>%
  group_by(regionCode, RegionName ) %>%
  summarise(
    Transpondent_total = sum(Transpondent_total),
    #gender
    Males_total = sum(Males_total),
    Females_total =sum(Females_total),
    #flow
    Inflow_total = sum(Inflow_total),
    Inflow_male =sum(Inflow_male),
    Inflow_female = sum(Inflow_female),
    
    Outflow_total = sum(Outflow_total),
    Outflow_male = sum(Outflow_male),
    Outflow_female = sum(Outflow_female),
    
    `NetInflow_(x-m)` = sum( `NetInflow_(x-m)`),
    `Net_male_inflow_(x-m)` = sum(`Net_male_inflow_(x-m)`),
    `Net_female_inflow_(x-m)` = sum(`Net_female_inflow_(x-m)`)
  )



regionTradeValueSummary <- borderTradeValueSummary %>% 
  ungroup() %>%
  group_by(regionCode, RegionName ) %>%
  summarise(
    TradeVolume_total = sum(TradeVolume_total),
    `TradeVolume_Import(m)` = sum(`TradeVolume_Import(m)`),
    `TradeVolume_Export(x)` = sum(`TradeVolume_Export(x)`),
    `TradeVolume_net(x-m)` = `TradeVolume_Export(x)` - `TradeVolume_Import(m)` ,
    #Trade Qty Volume
    `TradeVolume_maleTranspondent` = sum( `TradeVolume_maleTranspondent`),
    `TradeVolume_malesImport` = sum(`TradeVolume_malesImport`),
    `TradeVolume_malesExport` = sum(`TradeVolume_malesExport`),
    
    `TradeVolume_femaleTranspondent` = sum(`TradeVolume_femaleTranspondent`),
    `TradeVolume_femalesImport` = sum(`TradeVolume_femalesImport`),
    `TradeVolume_femalesExport` =  sum(`TradeVolume_femalesExport`),
    
    `TradeVolume_fromCatalogue` = sum(`TradeVolume_fromCatalogue`),
    TradeVolume_otherSpecified = sum(TradeVolume_otherSpecified ),
    
    `TradeVolume_ImportCatalogued` = sum(`TradeVolume_ImportCatalogued`),
    `TradeVolume_ExportCatalogued` = sum(`TradeVolume_ExportCatalogued`),
    
    # Trade Value
    TradeValue_total =  sum(TradeValue_total),
    `TradeValue_Import(m)` = sum(`TradeValue_Import(m)`),
    `TradeValue_Export(x)` = sum(`TradeValue_Export(x)` ),
    `TradeBalance(x-m)` = `TradeValue_Export(x)`  -  `TradeValue_Import(m)`,
    TradeValue_maleTranspondent = sum(TradeValue_maleTranspondent),
    `TradeValue_maleImport` = sum(`TradeValue_maleImport`),
    `TradeValue_maleExport` = sum(`TradeValue_maleExport`),
    
    TradeValue_femaleTranspondent = sum(TradeValue_femaleTranspondent),
    `TradeValue_femaleImport` = sum(`TradeValue_femaleImport`),
    `TradeValue_femaleExport` = sum(TradeValue_femaleExport),
    
    `TradeValue_fromCatalogue` = sum(TradeValue_fromCatalogue),
    `TradeValue_otherSpecified` = sum(TradeValue_otherSpecified),
    
    `TradeValue_ImportCatalogued` = sum(TradeValue_ImportCatalogued),
    `TradeValue_ExportCatalogued` = sum(TradeValue_ExportCatalogued)
  )




regionalProductDetail <- borderCommodityDetailValueSummary %>%
  ungroup() %>%
  group_by(regionCode, RegionName,productObserved ) %>%
  summarise(
    
    distinct_unit_of_measures = sum(distinct_unit_of_measures),
    totalTradeFrequency = sum(totalTradeFrequency),
    totalTradeQuantity =  sum(totalTradeQuantity),
    totalTradeValue = sum(totalTradeValue),
    
    maleTradeFrequency = sum(maleTradeFrequency),
    maleTradeQuantity =  sum(maleTradeQuantity),
    maleTradeValue = sum(maleTradeValue),
    
    femaleTradeFrequency = sum(femaleTradeFrequency),
    femaleTradeQuantity =  sum(femaleTradeQuantity),
    femaleTradeValue = sum(femaleTradeValue)
  ) %>% 
  arrange(
    RegionName,productObserved
  )


regioanlProductGeneral <- borderCommodityGeneralValueSummary %>%
  ungroup() %>%
  group_by(regionCode, RegionName,productObserved ) %>%
  summarise(
    distinct_unit_of_measures = sum(distinct_unit_of_measures),
    totalTradeFrequency = sum(totalTradeFrequency),
    totalTradeQuantity =  sum(totalTradeQuantity),
    totalTradeValue = sum(totalTradeValue),
    
    maleTradeFrequency = sum(maleTradeFrequency),
    maleTradeQuantity = sum(maleTradeQuantity),
    maleTradeValue = sum(maleTradeValue),
    
    femaleTradeFrequency = sum(femaleTradeFrequency),
    femaleTradeQuantity = sum(femaleTradeQuantity),
    femaleTradeValue = sum(femaleTradeValue)
  ) %>% 
  arrange(
    RegionName,productObserved
  )



###########################
##save to excel file
###########################


#***** flow of transpondents
filename <-paste("C:/2024ICBT/OfflineReporting/Region_MonitorReport.xlsx", sep="")  
data <-regionalPersonStat   %>% ungroup() %>% select(.,-regionCode)
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
data <- regionTradeValueSummary   %>% ungroup() %>% select(.,-regionCode)
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
data <-regioanlProductGeneral %>% ungroup() %>% select(.,-regionCode)
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



#***** Commodity Trade specifics
data <-regionalProductDetail %>% ungroup() %>% select(.,-regionCode)
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


