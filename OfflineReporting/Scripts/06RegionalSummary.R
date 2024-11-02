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
    
    `Netflow(x-m)` = sum(`Netflow(x-m)`),
    `Netflow_male(x-m)` = sum(`Netflow_male(x-m)`),
    `Netflow_female(x-m)` = sum(`Netflow_female(x-m)`)
  )


regionTradeValueSummary <- borderTradeValueSummary %>% 
  # reframe(select(.,everything())) %>%
  group_by(regionCode, RegionName ) %>%
  summarise(
    #Trading Qty 
    TradeQuantity_total  =   sum( TradeQuantity_total  ),
    `TradeQuantity_Import(m)`  =   sum(   `TradeQuantity_Import(m)`  ),
    `TradeQuantity_Export(x)`  =   sum(   `TradeQuantity_Export(x)`  ),
    `Net-TradeQuantity(x-m)` =     sum( `Net-TradeQuantity(x-m)` ),

    `Count_total_observation_with_tradeVolume_valueProvided`  =  sum(    `Count_total_observation_with_tradeVolume_valueProvided` ) ,
    `Count_OtherSpecify_obs_with_TradeVolume_Provided`  =    sum(  `Count_OtherSpecify_obs_with_TradeVolume_Provided` ) ,
    `Count_OtherSpecify_Obs_with_missing_TradeVolume`  =    sum(  `Count_OtherSpecify_Obs_with_missing_TradeVolume`  ),

    #Trade Qty Volume
    `TradeVolume(kg)_total`  =   sum(   `TradeVolume(kg)_total`  ),
    `TradeVolume(Kg)_Import(m)`  =   sum(   `TradeVolume(Kg)_Import(m)`  ),
    `TradeVolume(Kg)_Export(x)`  =  sum(    `TradeVolume(Kg)_Export(x)` ) ,
    `Net-TradeVolume(Kg)(x-m)` =     sum( `Net-TradeVolume(Kg)(x-m)`  ),

    #Trade Qty Volume
    `TradeVolume(kg)_male_total`  =    sum(  `TradeVolume(kg)_male_total`  ),
    `TradeVolume(kg)_male_Import`  =   sum(   `TradeVolume(kg)_male_Import`  ),
    `TradeVolume(kg)_male_Export`  =   sum(   `TradeVolume(kg)_male_Export`  ),

    `TradeVolume(kg)_female_total`  =   sum(   `TradeVolume(kg)_female_total` ) ,
    `TradeVolume(kg)_female_Import`  =    sum(  `TradeVolume(kg)_female_Import`  ),
    `TradeVolume(kg)_female_Export`  =    sum(  `TradeVolume(kg)_female_Export` ) ,

    `TradeVolume(kg)_Catalogued`  =    sum(  `TradeVolume(kg)_Catalogued` ) ,
    `TradeVolume(kg)_ImportCatalogued`  = sum(     `TradeVolume(kg)_ImportCatalogued`  ),
    `TradeVolume(kg)_ExportCatalogued`  =  sum(    `TradeVolume(kg)_ExportCatalogued`  ),
    `TradeVolume(kg)_otherSpecified`  =    sum(  `TradeVolume(kg)_otherSpecified` ) ,

    # Trade Value GHS CURRENCY =     # Trade Value GHS CURRENCY
    `TradeValue_total(GHS_currency)` = sum(     `TradeValue_total(GHS_currency)` ),
    `TradeValue_Import(m)(GHS)`  =    sum(  `TradeValue_Import(m)(GHS)`  ),
    `TradeValue_Export(x)(GHS)`  =   sum(   `TradeValue_Export(x)(GHS)`  ),
    `TradeBalance(x-m)(GHS)`  =      sum(`TradeBalance(x-m)(GHS)` ) ,

    #BY gender male
    `TradeValue_maleTranspondent(GHS)`  =   sum(   `TradeValue_maleTranspondent(GHS)`  ),
    `TradeValue_maleImport(GHS)`  =   sum(   `TradeValue_maleImport(GHS)`  ),
    `TradeValue_maleExport(GHS)`  =   sum(   `TradeValue_maleExport(GHS)`  ),
    #BY gender Female
    `TradeValue_femaleTranspondent(GHS)`  =  sum(    `TradeValue_femaleTranspondent(GHS)` ) ,
    `TradeValue_femaleImport(GHS)`  =  sum(    `TradeValue_femaleImport(GHS)`  ),
    `TradeValue_femaleExport(GHS)`  =  sum(    `TradeValue_femaleExport(GHS)` ) ,

    `TradeValue_from_Catalogue(GHS)`  =    sum(  `TradeValue_from_Catalogue(GHS)`  ),
    `TradeValue_ImportCatalogued(GHS)`  =    sum(  `TradeValue_ImportCatalogued(GHS)`  ),
    `TradeValue_ExportCatalogued(GHS)`  =   sum(   `TradeValue_ExportCatalogued(GHS)` ) ,

    `TradeValue_from_OtherSpecified(GHS)`  =  sum(    `TradeValue_from_OtherSpecified(GHS)`  ),

    # Trade Value CFA CURRENCY =     # Trade Value CFA CURRENCY
    `TradeValue_total(CFA_currency)` =   sum(   `TradeValue_total(CFA_currency)` ),
    `TradeValue_Import(m)(CFA)`  =   sum(   `TradeValue_Import(m)(CFA)`  ),
    `TradeValue_Export(x)(CFA)`  =  sum(    `TradeValue_Export(x)(CFA)`  ),
    `TradeBalance(x-m)(CFA)`  =    sum(  `TradeBalance(x-m)(CFA)`  ),
    #BY gender male =     #BY gender male
    `TradeValue_maleTranspondent(CFA)`  =    sum(  `TradeValue_maleTranspondent(CFA)`  ),
    `TradeValue_maleImport(CFA)`  =   sum(   `TradeValue_maleImport(CFA)`  ),
    `TradeValue_maleExport(CFA)`  =  sum(    `TradeValue_maleExport(CFA)`  ),
    #BY gender Female =     #BY gender Female
    `TradeValue_femaleTranspondent(CFA)`  =  sum(    `TradeValue_femaleTranspondent(CFA)`  ),
    `TradeValue_femaleImport(CFA)`  =   sum(   `TradeValue_femaleImport(CFA)`  ),
    `TradeValue_femaleExport(CFA)`  =     sum( `TradeValue_femaleExport(CFA)`  ),

    `TradeValue_from_Catalogue(CFA)`  =    sum(  `TradeValue_from_Catalogue(CFA)`  ),
    `TradeValue_ImportCatalogued(CFA)`  =   sum(   `TradeValue_ImportCatalogued(CFA)`  ),
    `TradeValue_ExportCatalogued(CFA)`  =    sum(  `TradeValue_ExportCatalogued(CFA)` ) ,

    `TradeValue_from_OtherSpecified(CFA)`  =  sum(    `TradeValue_from_OtherSpecified(CFA)`  ),

    # Trade Value USD CURRENCY =     # Trade Value USD CURRENCY
    `TradeValue_total(USD_currency)` =   sum(   `TradeValue_total(USD_currency)`  ),

    `TradeValue_Import(m)(USD)`  =    sum(  `TradeValue_Import(m)(USD)`  ),
    `TradeValue_Export(x)(USD)`  =   sum(   `TradeValue_Export(x)(USD)`  ),
    `TradeBalance(x-m)(USD)`  =      sum(`TradeBalance(x-m)(USD)`  ),
    #BY gender male
    `TradeValue_maleTranspondent(USD)`  =    sum(  `TradeValue_maleTranspondent(USD)` ) ,
    `TradeValue_maleImport(USD)`  =    sum(  `TradeValue_maleImport(USD)`  ),
    `TradeValue_maleExport(USD)`  =     sum( `TradeValue_maleExport(USD)`  ),
    #BY gender Female
    `TradeValue_femaleTranspondent(USD)`  =     sum( `TradeValue_femaleTranspondent(USD)`  ),
    `TradeValue_femaleImport(USD)`  =     sum( `TradeValue_femaleImport(USD)`  ),
    `TradeValue_femaleExport(USD)`  =    sum(  `TradeValue_femaleExport(USD)`  ),

    `TradeValue_from_Catalogue(USD)`  =   sum(   `TradeValue_from_Catalogue(USD)` ) ,
    `TradeValue_ImportCatalogued(USD)`  =   sum(   `TradeValue_ImportCatalogued(USD)`  ),
    `TradeValue_ExportCatalogued(USD)`  =   sum(   `TradeValue_ExportCatalogued(USD)` ) ,

     `TradeValue_from_OtherSpecified(USD)`  =      sum(`TradeValue_from_OtherSpecified(USD)`  ),

    # Trade Value Naira CURRENCY =     # Trade Value Naira CURRENCY
    `TradeValue_total(Naira_currency)`  =   sum(   `TradeValue_total(Naira_currency)` ) ,

    `TradeValue_Import(m)(Naira)`  =   sum(   `TradeValue_Import(m)(Naira)`  ),
    `TradeValue_Export(x)(Naira)`  =   sum(   `TradeValue_Export(x)(Naira)` ) ,
    `TradeBalance(x-m)(Naira)`  =     sum( `TradeBalance(x-m)(Naira)`  ),
    #BY gender male
    `TradeValue_maleTranspondent(Naira)`  =    sum(  `TradeValue_maleTranspondent(Naira)`  ),
    `TradeValue_maleImport(Naira)`  =    sum(  `TradeValue_maleImport(Naira)`  ),
    `TradeValue_maleExport(Naira)`  =    sum(  `TradeValue_maleExport(Naira)`  ),
    #BY gender Female
    `TradeValue_femaleTranspondent(Naira)`  =     sum( `TradeValue_femaleTranspondent(Naira)` ) ,
    `TradeValue_femaleImport(Naira)`  =     sum( `TradeValue_femaleImport(Naira)`  ),
    `TradeValue_femaleExport(Naira)`  =    sum(  `TradeValue_femaleExport(Naira)`  ),

    `TradeValue_from_Catalogue(Naira)`  =     sum( `TradeValue_from_Catalogue(Naira)`  ),
    `TradeValue_ImportCatalogued(Naira)`  =    sum(  `TradeValue_ImportCatalogued(Naira)`  ),
    `TradeValue_ExportCatalogued(Naira)`  =     sum( `TradeValue_ExportCatalogued(Naira)` ),

    `TradeValue_from_OtherSpecified(Naira)`  =   sum(   `TradeValue_from_OtherSpecified(Naira)` ),
  
    #ANIMALS
    totalAnimalCrossingFrequency = sum(totalAnimalCrossingFrequency),
    
    totalAnimalCrossingInwardFrequency = sum(totalAnimalCrossingInwardFrequency),
    totalAnimalCrossingOutwardFrequency =  sum(totalAnimalCrossingOutwardFrequency),
    `net-totalAnimalCrossingInwardFrequency` = sum(`net-totalAnimalCrossingInwardFrequency` ),
    
    totalAnimalCrossingCount = sum(totalAnimalCrossingCount),
    totalAnimalCrossingInwardCount = sum(totalAnimalCrossingInwardCount) , 
    totalAnimalCrossingOutwardCount =  sum(totalAnimalCrossingOutwardCount),
    `net-totalAnimalCrossingCount` = sum(`net-totalAnimalCrossingCount`),
    
    #Currency GHS
    totalAnimalCrossingValue_GHS = sum(totalAnimalCrossingValue_GHS),
    `AnimalCrossingInward(x)_Value_GHS` = sum(`AnimalCrossingInward(x)_Value_GHS` ),
    `AnimalCrossingOutward(x)_Value_GHS` = sum(`AnimalCrossingOutward(x)_Value_GHS`),
    `net-AnimalCrossing_Value_GHS(x-m)` =  sum( `net-AnimalCrossing_Value_GHS(x-m)`),
    
    #Currency CFA
    totalAnimalCrossingValue_CFA = sum(totalAnimalCrossingValue_CFA),
    `AnimalCrossingInward(x)_Value_CFA` = sum( `AnimalCrossingInward(x)_Value_CFA`),
    `AnimalCrossingOutward(x)_Value_CFA` = sum( `AnimalCrossingOutward(x)_Value_CFA`),
    `net-AnimalCrossing_Value_CFA(x-m)` = sum(`net-AnimalCrossing_Value_CFA(x-m)`),
    
    #Currency USD
    totalAnimalCrossingValue_USD = sum( totalAnimalCrossingValue_USD),
    `AnimalCrossingInward(x)_Value_USD` = sum( `AnimalCrossingInward(x)_Value_USD`),
    `AnimalCrossingOutward(x)_Value_USD` = sum(`AnimalCrossingOutward(x)_Value_USD`),
    `net-AnimalCrossing_Value_USD(x-m)` =  sum( `net-AnimalCrossing_Value_USD(x-m)`),
    
    totalAnimalCrossingValue_Naira = sum(totalAnimalCrossingValue_Naira),
    `AnimalCrossingInward(x)_Value_Naira` =  sum(`AnimalCrossingInward(x)_Value_Naira`),
    `AnimalCrossingOutward(x)_Value_Naira` = sum(`AnimalCrossingOutward(x)_Value_Naira`),
    `net-AnimalCrossing_Value_Naira(x-m)` =  sum(`net-AnimalCrossing_Value_Naira(x-m)`)
  ) 




regionalProductDetail <- borderCommodityDetailValueSummary %>%
  ungroup() %>%
  group_by(regionCode, RegionName,productObserved ) %>%
  summarise(
    distinct_unit_of_measures = sum (distinct_unit_of_measures),
    totalTradeFrequency = sum(totalTradeFrequency),
    TradeQuantity_total = sum(TradeQuantity_total),
    `TradeQuantity_Import(m)` = sum(`TradeQuantity_Import(m)`),
    `TradeQuantity_Export(x)` = sum(`TradeQuantity_Export(x)`),
    `Net-TradeQuantity(x-m)` = sum(`Net-TradeQuantity(x-m)`),
    
    `TradeVolume(kg)_total` = sum( `TradeVolume(kg)_total` ),
    `TradeVolume(Kg)_Import(m)` = sum( `TradeVolume(Kg)_Import(m)`),
    `TradeVolume(Kg)_Export(x)`  = sum (`TradeVolume(Kg)_Export(x)` ),
    `Net-TradeVolume(Kg)(x-m)` = sum(`Net-TradeVolume(Kg)(x-m)`),
    
    # Trade Value GHS CURRENCY
    `TradeValue_total(GHS_currency)`= sum(`TradeValue_total(GHS_currency)`),
    `TradeValue_Import(m)(GHS)` = sum(`TradeValue_Import(m)(GHS)`),
    `TradeValue_Export(x)(GHS)` = sum(`TradeValue_Export(x)(GHS)`),
    `TradeBalance(x-m)(GHS)` = sum(`TradeBalance(x-m)(GHS)`),
    
    # Trade Value CFA CURRENCY
    `TradeValue_total(CFA_currency)` = sum(`TradeValue_total(CFA_currency)`),
    `TradeValue_Import(m)(CFA)` = sum(`TradeValue_Import(m)(CFA)`),
    `TradeValue_Export(x)(CFA)` = sum(`TradeValue_Export(x)(CFA)`),
    `TradeBalance(x-m)(CFA)` = sum(`TradeBalance(x-m)(CFA)`),
    
    # Trade Value USD CURRENCY
    `TradeValue_total(USD_currency)` = sum( `TradeValue_total(USD_currency)`),
    `TradeValue_Import(m)(USD)` = sum(`TradeValue_Import(m)(USD)`),
    `TradeValue_Export(x)(USD)` = sum(`TradeValue_Export(x)(USD)`),
    `TradeBalance(x-m)(USD)`  = sum(`TradeBalance(x-m)(USD)` ),
    
    # Trade Value Naira CURRENCY
    `TradeValue_total(Naira_currency)` = sum(`TradeValue_total(Naira_currency)`),
    `TradeValue_Import(m)(Naira)` = sum(`TradeValue_Import(m)(Naira)`),
    `TradeValue_Export(x)(Naira)` = sum(`TradeValue_Export(x)(Naira)`),
    `TradeBalance(x-m)(Naira)` = sum(`TradeBalance(x-m)(Naira)`),
    
    
  
  ) %>%
  arrange(
    RegionName,productObserved
  )


regioanlProductGeneral <- borderCommodityGeneralValueSummary %>%
  ungroup() %>%
  group_by(regionCode, RegionName,productObserved ) %>%
  summarise(
    distinct_unit_of_measures = sum (distinct_unit_of_measures),
    totalTradeFrequency = sum(totalTradeFrequency),
    TradeQuantity_total = sum(TradeQuantity_total),
    `TradeQuantity_Import(m)` = sum(`TradeQuantity_Import(m)`),
    `TradeQuantity_Export(x)` = sum(`TradeQuantity_Export(x)`),
    `Net-TradeQuantity(x-m)` = sum(`Net-TradeQuantity(x-m)`),
    
    `TradeVolume(kg)_total` = sum( `TradeVolume(kg)_total` ),
    `TradeVolume(Kg)_Import(m)` = sum( `TradeVolume(Kg)_Import(m)`),
    `TradeVolume(Kg)_Export(x)`  = sum (`TradeVolume(Kg)_Export(x)` ),
    `Net-TradeVolume(Kg)(x-m)` = sum(`Net-TradeVolume(Kg)(x-m)`),
    
    # Trade Value GHS CURRENCY
    `TradeValue_total(GHS_currency)`= sum(`TradeValue_total(GHS_currency)`),
    `TradeValue_Import(m)(GHS)` = sum(`TradeValue_Import(m)(GHS)`),
    `TradeValue_Export(x)(GHS)` = sum(`TradeValue_Export(x)(GHS)`),
    `TradeBalance(x-m)(GHS)` = sum(`TradeBalance(x-m)(GHS)`),
    
    # Trade Value CFA CURRENCY
    `TradeValue_total(CFA_currency)` = sum(`TradeValue_total(CFA_currency)`),
    `TradeValue_Import(m)(CFA)` = sum(`TradeValue_Import(m)(CFA)`),
    `TradeValue_Export(x)(CFA)` = sum(`TradeValue_Export(x)(CFA)`),
    `TradeBalance(x-m)(CFA)` = sum(`TradeBalance(x-m)(CFA)`),
    
    # Trade Value USD CURRENCY
    `TradeValue_total(USD_currency)` = sum( `TradeValue_total(USD_currency)`),
    `TradeValue_Import(m)(USD)` = sum(`TradeValue_Import(m)(USD)`),
    `TradeValue_Export(x)(USD)` = sum(`TradeValue_Export(x)(USD)`),
    `TradeBalance(x-m)(USD)`  = sum(`TradeBalance(x-m)(USD)` ),
    
    # Trade Value Naira CURRENCY
    `TradeValue_total(Naira_currency)` = sum(`TradeValue_total(Naira_currency)`),
    `TradeValue_Import(m)(Naira)` = sum(`TradeValue_Import(m)(Naira)`),
    `TradeValue_Export(x)(Naira)` = sum(`TradeValue_Export(x)(Naira)`),
    `TradeBalance(x-m)(Naira)` = sum(`TradeBalance(x-m)(Naira)`)
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


