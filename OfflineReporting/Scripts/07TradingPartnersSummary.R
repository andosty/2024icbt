
tradingPartners <- icbt_data %>% 
  distinct(regionCode, RegionName, districtName, borderPostName) %>%
  mutate(
    tradePartner = case_when(
      str_detect(str_to_lower(RegionName),"western|western north|bono") ~ "Côte d'Ivoire",
      str_detect(str_to_lower(RegionName),"north east|northern|oti|volta") ~ "Togo",
      str_detect(str_to_lower(RegionName),"upper west") ~ "Burkina Faso",
      str_detect(str_to_lower(RegionName),"savannah") &  str_detect(str_to_lower(districtName),"bole")  ~ "Côte d'Ivoire",
      str_detect(str_to_lower(RegionName),"upper east") & str_detect(str_to_lower(districtName),"pusiga") & str_detect( str_to_lower(borderPostName),"bagri") ~ "Burkina Faso",
      str_detect(str_to_lower(RegionName),"upper east") & str_detect(str_to_lower(districtName),"pusiga") & str_detect(str_to_lower(borderPostName),"baddo") ~ "Burkina Faso",
      str_detect(str_to_lower(RegionName),"upper east") & str_detect(str_to_lower(districtName),"pusiga") & str_detect(str_to_lower(borderPostName),"kulungugu") ~ "Burkina Faso",
      str_detect(str_to_lower(RegionName),"upper east") & str_detect(str_to_lower(districtName),"bawku municipal|bawku west|bongo|kasena")  ~ "Burkina Faso",
      str_detect(str_to_lower(RegionName),"upper east") & str_detect(str_to_lower(districtName),"tempane") ~ "Togo",
      str_detect(str_to_lower(RegionName),"upper east") & str_detect(str_to_lower(districtName),"pusiga") & str_detect(str_to_lower(borderPostName),"widana") ~ "Togo",
      TRUE ~ "Uncertain"
    )
  ) %>%
  arrange(regionCode, RegionName, districtName, borderPostName,tradePartner)


tradePartnerPersonStat <- borderPersonStat %>% 
  ungroup() %>%
  left_join(tradingPartners, by = join_by(regionCode, RegionName, districtName, borderPostName)) %>%
  group_by(tradePartner) %>%
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
  ) %>%
  arrange(tradePartner)



tradePartnerTradeValueSummary <- borderTradeValueSummary %>% 
  ungroup() %>%
  left_join(tradingPartners, by = join_by(regionCode, RegionName, districtName, borderPostName)) %>%
  group_by(tradePartner) %>%
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
    `TradeBalance(x-m)` =  `TradeValue_Export(x)`-`TradeValue_Import(m)` ,
    
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
  )  %>%
  arrange(tradePartner)




tradePartnerProductDetail <- borderCommodityDetailValueSummary %>%
  ungroup() %>%
  left_join(tradingPartners, by = join_by(regionCode, RegionName, districtName, borderPostName)) %>%
  group_by(tradePartner,productObserved) %>%
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
  )  %>%
  arrange(tradePartner)


tradePartnerProductGeneral <- borderCommodityGeneralValueSummary %>%
  ungroup() %>%
  left_join(tradingPartners, by = join_by(regionCode, RegionName, districtName, borderPostName)) %>%
  group_by(tradePartner,productObserved) %>%
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
  )  %>%
  arrange(tradePartner)



###########################
##save to excel file
###########################


#*****list of trade partner classification
filename <-paste("C:/2024ICBT/OfflineReporting/tradePartner_MonitorReport.xlsx", sep="")  
data <-tradingPartners  
sheetName_word <- "tradePartner Mapping"
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

#***** flow of transpondents
data <-tradePartnerPersonStat
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
data <- tradePartnerTradeValueSummary 
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
data <-tradePartnerProductGeneral 
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
data <-tradePartnerProductDetail  
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


