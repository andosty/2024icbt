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
    
    `Netflow(x-m)` = Outflow_total - Inflow_total,
    `Netflow_male(x-m)` = Outflow_male - Inflow_male ,
    `Netflow_female(x-m)` = Outflow_female - Inflow_female
  )

# animal <- icbt_data %>%
#   filter(!is.na(livestockAge)) %>%
#   filter(productObserved=="Other (specify)") %>%
#   distinct(productObserved_otherSpecify)
#   distinct(productObserved)


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

# 4 possible currency to select in data
# Ghana Cedi , CFA , USD & Naira 

dataValua_Calculation <- icbt_data %>% 
  mutate(
    productObserved=  gsub("colanut", "cola nut", productObserved, ignore.case = TRUE),
  ) %>% left_join(
    productCatalogue , by = c("productObserved"="catalogueProduct" , "unit_of_measure"="catalogue_unit_of_measure"), 
    relationship = "many-to-many"
  ) %>% 
  # filter(is.na(catalogueWeight)) %>%
  # filter(borderPostName=='Appolenu') %>%
  mutate(
    # across(where(is.numeric), ~replace_na(.x, 0)),
    
    `tradeValue_(those in GHS)` = case_when(
      productObserved!='Other (specify)' & unit_of_measure !='Other (specify)' & !is.na(unit_of_measure) ~ commodityQuantity * cataloguePrice ,
      productObserved!='Other (specify)' & unit_of_measure =='Other (specify)' & !is.na(CommodityPrice) ~ commodityQuantity * CommodityPrice, 
      productObserved=='Other (specify)' & purchaseCurrency=='Ghana Cedi' ~ commodityQuantity * CommodityPrice,
      TRUE ~ NA
    ),
    `tradeValue_(those in CFA)` = case_when(
      productObserved=='Other (specify)' & purchaseCurrency=='CFA' ~ commodityQuantity * CommodityPrice,
      TRUE ~ NA
    ),
    `tradeValue_(those in USD)` = case_when(
      productObserved=='Other (specify)' & purchaseCurrency=='USD' ~ commodityQuantity * CommodityPrice,
      TRUE ~ NA
    ),
    `tradeValue_(those in Naira)` = case_when(
      productObserved=='Other (specify)' & purchaseCurrency=='Naira' ~ commodityQuantity * CommodityPrice,
      TRUE ~ NA
    ),
    
    # replace trade volume
    catalogueWeight = ifelse(is.na(catalogueWeight) & !is.na(commodityWeight_Kg) & !is.na(commodityQuantity),commodityWeight_Kg/commodityQuantity, catalogueWeight ),
                        # case(
                        #   is.na(catalogueWeight) & !is.na(commodityWeight_Kg) & !is.na(commodityQuantity) ~ (commodityWeight_Kg/commodityQuantity) ,
                        #   TRUE ~ catalogueWeight
                        #   ),
    # across(where(is.numeric), ~replace_na(.x, 0)),
    
  ) #%>%  # filter(is.na(catalogueWeight)) %>%


borderTradeValueSummary <- dataValua_Calculation %>%
  group_by(regionCode, RegionName, districtName, borderPostName ) %>%
  summarise(
    #Trading Qty 
    TradeQuantity_total = sum(commodityQuantity,na.rm=TRUE),
    `TradeQuantity_Import(m)` = sum(ifelse(tradeDirection=='Coming in (Import)',commodityQuantity,0),na.rm=TRUE),
    `TradeQuantity_Export(x)` =  sum(ifelse(tradeDirection=='Going out (Export)',commodityQuantity,0),na.rm=TRUE),
    `Net-TradeQuantity(x-m)`= `TradeQuantity_Export(x)`  - `TradeQuantity_Import(m)`,
    
    `Count_total_observation_with_tradeVolume_valueProvided` = sum(ifelse(!is.na(catalogueWeight) ,1,0 )),
    `Count_OtherSpecify_obs_with_TradeVolume_Provided` = sum(ifelse(!is.na(catalogueWeight) & productObserved=='Other (specify)' ,1,0 )),
    `Count_OtherSpecify_Obs_with_missing_TradeVolume` = sum(ifelse(is.na(catalogueWeight)  ,1,0 )), #& productObserved=='Other (specify)'
    
    `TradeVolume(kg)_total` =  round(sum(catalogueWeight,na.rm=TRUE), 4), # sum(ifelse(!is.na(catalogueWeight) ,catalogueWeight,0 )), # sum(catalogueWeight),
    `TradeVolume(Kg)_Import(m)` = round(sum(ifelse(tradeDirection=='Coming in (Import)',catalogueWeight,0),na.rm=TRUE), 4),
    `TradeVolume(Kg)_Export(x)` = round( sum(ifelse(tradeDirection=='Going out (Export)',catalogueWeight,0), na.rm=TRUE), 4),
    `Net-TradeVolume(Kg)(x-m)`= `TradeVolume(Kg)_Export(x)` - `TradeVolume(Kg)_Import(m)` ,
    
    #Trade Qty Volume
    `TradeVolume(kg)_male_total` = sum(ifelse(sex=='Male',catalogueWeight,0), na.rm=TRUE),
    `TradeVolume(kg)_male_Import` = sum(ifelse(sex=='Male' & tradeDirection=='Coming in (Import)',catalogueWeight,0) , na.rm=TRUE),
    `TradeVolume(kg)_male_Export` =  sum(ifelse(sex=='Male' & tradeDirection=='Going out (Export)',catalogueWeight,0), na.rm=TRUE),
    
    `TradeVolume(kg)_female_total` = sum(ifelse(sex=='Female',catalogueWeight,0) , na.rm=TRUE),
    `TradeVolume(kg)_female_Import` = sum(ifelse(sex=='Female' & tradeDirection=='Coming in (Import)',catalogueWeight,0) , na.rm=TRUE),
    `TradeVolume(kg)_female_Export` =  sum(ifelse(sex=='Female' & tradeDirection=='Going out (Export)',catalogueWeight,0) , na.rm=TRUE),
    
    `TradeVolume(kg)_Catalogued` = sum(ifelse(productObserved!='Other (specify)' , catalogueWeight ,0)  , na.rm=TRUE),
    
    `TradeVolume(kg)_ImportCatalogued` = sum(
      ifelse(tradeDirection=='Coming in (Import)' & productObserved!='Other (specify)',catalogueWeight,0)  , na.rm=TRUE 
    ),
    `TradeVolume(kg)_ExportCatalogued` =  sum(ifelse(tradeDirection=='Going out (Export)' & productObserved!='Other (specify)' ,catalogueWeight,0)  , na.rm=TRUE
    ),
    
    `TradeVolume(kg)_otherSpecified` = sum(ifelse(productObserved=='Other (specify)' , catalogueWeight ,0)  , na.rm=TRUE),
    
    # Trade Value GHS CURRENCY
    `TradeValue_total(GHS_currency)`= round(sum(`tradeValue_(those in GHS)` , na.rm=TRUE),2) ,# format(round(  sum(tradeValue),2), scientific = FALSE), 
    
    `TradeValue_Import(m)(GHS)` = round(sum(ifelse(tradeDirection=='Coming in (Import)',`tradeValue_(those in GHS)`,0) , na.rm=TRUE),2) ,# format(round(sum(ifelse(tradeDirection=='Coming in (Import)',tradeValue,0)),2), scientific = FALSE) ,
    `TradeValue_Export(x)(GHS)` = round(sum(ifelse(tradeDirection=='Going out (Export)',`tradeValue_(those in GHS)`,0) , na.rm=TRUE),2), # format(round(sum(ifelse(tradeDirection=='Going out (Export)',tradeValue,0)),2), scientific = FALSE) ,
    `TradeBalance(x-m)(GHS)` =round( `TradeValue_Export(x)(GHS)` - `TradeValue_Import(m)(GHS)`,2) ,
    #BY gender male
    `TradeValue_maleTranspondent(GHS)` = round(sum(ifelse(sex=='Male',`tradeValue_(those in GHS)`,0) , na.rm=TRUE)  ,2) ,# format(round(sum(ifelse(sex=='Male',tradeValue,0))  ,2), scientific = FALSE), 
    `TradeValue_maleImport(GHS)` = round(sum(ifelse(sex=='Male' & tradeDirection=='Coming in (Import)',`tradeValue_(those in GHS)`,0) , na.rm=TRUE),2), # format(round(sum(ifelse(sex=='Male' & tradeDirection=='Coming in (Import)',tradeValue,0)),2), scientific = FALSE) ,
    `TradeValue_maleExport(GHS)` = round(sum(ifelse(sex=='Male' & tradeDirection=='Going out (Export)',`tradeValue_(those in GHS)`,0) , na.rm=TRUE),2), # format(round(sum(ifelse(sex=='Male' & tradeDirection=='Going out (Export)',tradeValue,0)),2), scientific = FALSE) ,
    #BY gender Female
    `TradeValue_femaleTranspondent(GHS)` = round(sum(ifelse(sex=='Female',`tradeValue_(those in GHS)`,0) , na.rm=TRUE)  ,2), # format(round(sum(ifelse(sex=='Female',tradeValue,0))  ,2), scientific = FALSE), 
    `TradeValue_femaleImport(GHS)` = round(sum(ifelse(sex=='Female' & tradeDirection=='Coming in (Import)',`tradeValue_(those in GHS)`,0) , na.rm=TRUE),2), # format(round(sum(ifelse(sex=='Female' & tradeDirection=='Coming in (Import)',tradeValue,0)),2), scientific = FALSE) ,
    `TradeValue_femaleExport(GHS)` = round(sum(ifelse(sex=='Female' & tradeDirection=='Going out (Export)',`tradeValue_(those in GHS)`,0) , na.rm=TRUE),2), # format(round(sum(ifelse(sex=='Female' & tradeDirection=='Going out (Export)',tradeValue,0)),2), scientific = FALSE) ,
    
    `TradeValue_from_Catalogue(GHS)` = round(sum(ifelse(productObserved!='Other (specify)',`tradeValue_(those in GHS)`,0) , na.rm=TRUE),2), # format(round(sum(ifelse(productObserved!='Other (specify)',tradeValue,0)),2), scientific = FALSE) ,
    `TradeValue_ImportCatalogued(GHS)` = round( sum( ifelse(tradeDirection=='Coming in (Import)' & productObserved!='Other (specify)',`tradeValue_(those in GHS)`,0) , na.rm=TRUE),2), # format(round( sum( ifelse(tradeDirection=='Coming in (Import)' & productObserved!='Other (specify)',tradeValue,0)),2), scientific = FALSE) ,
    `TradeValue_ExportCatalogued(GHS)` = round(sum(ifelse(tradeDirection=='Going out (Export)' & productObserved!='Other (specify)',`tradeValue_(those in GHS)`,0) , na.rm=TRUE),2) , # format(round(sum(ifelse(tradeDirection=='Going out (Export)' & productObserved!='Other (specify)',tradeValue,0)),2), scientific = FALSE) ,
    
    `TradeValue_from_OtherSpecified(GHS)` = round(sum(ifelse(productObserved=='Other (specify)',`tradeValue_(those in GHS)`,0) , na.rm=TRUE),2), # format(round(sum(ifelse(productObserved=='Other (specify)',tradeValue,0)),2), scientific = FALSE) ,
    
    # Trade Value CFA CURRENCY
    `TradeValue_total(CFA_currency)`= round(sum(`tradeValue_(those in CFA)` , na.rm=TRUE),2) ,# format(round(  sum(tradeValue),2), scientific = FALSE), 
    
    `TradeValue_Import(m)(CFA)` = round(sum(ifelse(tradeDirection=='Coming in (Import)',`tradeValue_(those in CFA)`,0) , na.rm=TRUE),2) ,# format(round(sum(ifelse(tradeDirection=='Coming in (Import)',tradeValue,0)),2), scientific = FALSE) ,
    `TradeValue_Export(x)(CFA)` = round(sum(ifelse(tradeDirection=='Going out (Export)',`tradeValue_(those in CFA)`,0) , na.rm=TRUE),2), # format(round(sum(ifelse(tradeDirection=='Going out (Export)',tradeValue,0)),2), scientific = FALSE) ,
    `TradeBalance(x-m)(CFA)` =`TradeValue_Export(x)(CFA)` - `TradeValue_Import(m)(CFA)` ,
    #BY gender male
    `TradeValue_maleTranspondent(CFA)` = round(sum(ifelse(sex=='Male',`tradeValue_(those in CFA)`,0) , na.rm=TRUE)  ,2) ,# format(round(sum(ifelse(sex=='Male',tradeValue,0))  ,2), scientific = FALSE), 
    `TradeValue_maleImport(CFA)` = round(sum(ifelse(sex=='Male' & tradeDirection=='Coming in (Import)',`tradeValue_(those in CFA)`,0) , na.rm=TRUE),2), # format(round(sum(ifelse(sex=='Male' & tradeDirection=='Coming in (Import)',tradeValue,0)),2), scientific = FALSE) ,
    `TradeValue_maleExport(CFA)` = round(sum(ifelse(sex=='Male' & tradeDirection=='Going out (Export)',`tradeValue_(those in CFA)`,0) , na.rm=TRUE),2), # format(round(sum(ifelse(sex=='Male' & tradeDirection=='Going out (Export)',tradeValue,0)),2), scientific = FALSE) ,
    #BY gender Female
    `TradeValue_femaleTranspondent(CFA)` = round(sum(ifelse(sex=='Female',`tradeValue_(those in CFA)`,0)  , na.rm=TRUE)  ,2), # format(round(sum(ifelse(sex=='Female',tradeValue,0))  ,2), scientific = FALSE), 
    `TradeValue_femaleImport(CFA)` = round(sum(ifelse(sex=='Female' & tradeDirection=='Coming in (Import)',`tradeValue_(those in CFA)`,0) , na.rm=TRUE),2), # format(round(sum(ifelse(sex=='Female' & tradeDirection=='Coming in (Import)',tradeValue,0)),2), scientific = FALSE) ,
    `TradeValue_femaleExport(CFA)` = round(sum(ifelse(sex=='Female' & tradeDirection=='Going out (Export)',`tradeValue_(those in CFA)`,0) , na.rm=TRUE),2), # format(round(sum(ifelse(sex=='Female' & tradeDirection=='Going out (Export)',tradeValue,0)),2), scientific = FALSE) ,
    
    `TradeValue_from_Catalogue(CFA)` = round(sum(ifelse(productObserved!='Other (specify)',`tradeValue_(those in CFA)`,0) , na.rm=TRUE),2), # format(round(sum(ifelse(productObserved!='Other (specify)',tradeValue,0)),2), scientific = FALSE) ,
    `TradeValue_ImportCatalogued(CFA)` = round( sum( ifelse(tradeDirection=='Coming in (Import)' & productObserved!='Other (specify)',`tradeValue_(those in CFA)`,0) , na.rm=TRUE),2), # format(round( sum( ifelse(tradeDirection=='Coming in (Import)' & productObserved!='Other (specify)',tradeValue,0)),2), scientific = FALSE) ,
    `TradeValue_ExportCatalogued(CFA)` = round(sum(ifelse(tradeDirection=='Going out (Export)' & productObserved!='Other (specify)',`tradeValue_(those in CFA)`,0) , na.rm=TRUE),2) , # format(round(sum(ifelse(tradeDirection=='Going out (Export)' & productObserved!='Other (specify)',tradeValue,0)),2), scientific = FALSE) ,
    
    `TradeValue_from_OtherSpecified(CFA)` = round(sum(ifelse(productObserved=='Other (specify)',`tradeValue_(those in CFA)`,0) , na.rm=TRUE),2), # format(round(sum(ifelse(productObserved=='Other (specify)',tradeValue,0)),2), scientific = FALSE) ,
    
    # Trade Value USD CURRENCY
    `TradeValue_total(USD_currency)`= round(sum(`tradeValue_(those in USD)` , na.rm=TRUE),2) ,# format(round(  sum(tradeValue),2), scientific = FALSE), 
    
    `TradeValue_Import(m)(USD)` = round(sum(ifelse(tradeDirection=='Coming in (Import)',`tradeValue_(those in USD)`,0) , na.rm=TRUE),2) ,# format(round(sum(ifelse(tradeDirection=='Coming in (Import)',tradeValue,0)),2), scientific = FALSE) ,
    `TradeValue_Export(x)(USD)` = round(sum(ifelse(tradeDirection=='Going out (Export)',`tradeValue_(those in USD)`,0) , na.rm=TRUE),2), # format(round(sum(ifelse(tradeDirection=='Going out (Export)',tradeValue,0)),2), scientific = FALSE) ,
    `TradeBalance(x-m)(USD)` =`TradeValue_Export(x)(USD)` - `TradeValue_Import(m)(USD)` ,
    #BY gender male
    `TradeValue_maleTranspondent(USD)` = round(sum(ifelse(sex=='Male',`tradeValue_(those in USD)`,0) , na.rm=TRUE)  ,2) ,# format(round(sum(ifelse(sex=='Male',tradeValue,0))  ,2), scientific = FALSE), 
    `TradeValue_maleImport(USD)` = round(sum(ifelse(sex=='Male' & tradeDirection=='Coming in (Import)',`tradeValue_(those in USD)`,0) , na.rm=TRUE),2), # format(round(sum(ifelse(sex=='Male' & tradeDirection=='Coming in (Import)',tradeValue,0)),2), scientific = FALSE) ,
    `TradeValue_maleExport(USD)` = round(sum(ifelse(sex=='Male' & tradeDirection=='Going out (Export)',`tradeValue_(those in USD)`,0) , na.rm=TRUE),2), # format(round(sum(ifelse(sex=='Male' & tradeDirection=='Going out (Export)',tradeValue,0)),2), scientific = FALSE) ,
    #BY gender Female
    `TradeValue_femaleTranspondent(USD)` = round(sum(ifelse(sex=='Female',`tradeValue_(those in USD)`,0) , na.rm=TRUE)  ,2), # format(round(sum(ifelse(sex=='Female',tradeValue,0))  ,2), scientific = FALSE), 
    `TradeValue_femaleImport(USD)` = round(sum(ifelse(sex=='Female' & tradeDirection=='Coming in (Import)',`tradeValue_(those in USD)`,0) , na.rm=TRUE),2), # format(round(sum(ifelse(sex=='Female' & tradeDirection=='Coming in (Import)',tradeValue,0)),2), scientific = FALSE) ,
    `TradeValue_femaleExport(USD)` = round(sum(ifelse(sex=='Female' & tradeDirection=='Going out (Export)',`tradeValue_(those in USD)`,0) , na.rm=TRUE),2), # format(round(sum(ifelse(sex=='Female' & tradeDirection=='Going out (Export)',tradeValue,0)),2), scientific = FALSE) ,
    
    `TradeValue_from_Catalogue(USD)` = round(sum(ifelse(productObserved!='Other (specify)',`tradeValue_(those in USD)`,0) , na.rm=TRUE),2), # format(round(sum(ifelse(productObserved!='Other (specify)',tradeValue,0)),2), scientific = FALSE) ,
    `TradeValue_ImportCatalogued(USD)` = round( sum( ifelse(tradeDirection=='Coming in (Import)' & productObserved!='Other (specify)',`tradeValue_(those in USD)`,0) , na.rm=TRUE),2), # format(round( sum( ifelse(tradeDirection=='Coming in (Import)' & productObserved!='Other (specify)',tradeValue,0)),2), scientific = FALSE) ,
    `TradeValue_ExportCatalogued(USD)` = round(sum(ifelse(tradeDirection=='Going out (Export)' & productObserved!='Other (specify)',`tradeValue_(those in USD)`,0) , na.rm=TRUE),2) , # format(round(sum(ifelse(tradeDirection=='Going out (Export)' & productObserved!='Other (specify)',tradeValue,0)),2), scientific = FALSE) ,
    
    `TradeValue_from_OtherSpecified(USD)` = round(sum(ifelse(productObserved=='Other (specify)',`tradeValue_(those in USD)`,0) , na.rm=TRUE),2), # format(round(sum(ifelse(productObserved=='Other (specify)',tradeValue,0)),2), scientific = FALSE) ,
    
    # Trade Value Naira CURRENCY
    `TradeValue_total(Naira_currency)` = round(sum(`tradeValue_(those in Naira)` , na.rm=TRUE),2) ,# format(round(  sum(tradeValue),2), scientific = FALSE), 
    
    `TradeValue_Import(m)(Naira)` = round(sum(ifelse(tradeDirection=='Coming in (Import)',`tradeValue_(those in Naira)`,0) , na.rm=TRUE),2) ,# format(round(sum(ifelse(tradeDirection=='Coming in (Import)',tradeValue,0)),2), scientific = FALSE) ,
    `TradeValue_Export(x)(Naira)` = round(sum(ifelse(tradeDirection=='Going out (Export)',`tradeValue_(those in Naira)`,0) , na.rm=TRUE),2), # format(round(sum(ifelse(tradeDirection=='Going out (Export)',tradeValue,0)),2), scientific = FALSE) ,
    `TradeBalance(x-m)(Naira)` =`TradeValue_Export(x)(Naira)` - `TradeValue_Import(m)(Naira)` ,
    #BY gender male
    `TradeValue_maleTranspondent(Naira)` = round(sum(ifelse(sex=='Male',`tradeValue_(those in Naira)`,0))  ,2) ,# format(round(sum(ifelse(sex=='Male',tradeValue,0))  ,2), scientific = FALSE), 
    `TradeValue_maleImport(Naira)` = round(sum(ifelse(sex=='Male' & tradeDirection=='Coming in (Import)',`tradeValue_(those in Naira)`,0) , na.rm=TRUE),2), # format(round(sum(ifelse(sex=='Male' & tradeDirection=='Coming in (Import)',tradeValue,0)),2), scientific = FALSE) ,
    `TradeValue_maleExport(Naira)` = round(sum(ifelse(sex=='Male' & tradeDirection=='Going out (Export)',`tradeValue_(those in Naira)`,0) , na.rm=TRUE),2), # format(round(sum(ifelse(sex=='Male' & tradeDirection=='Going out (Export)',tradeValue,0)),2), scientific = FALSE) ,
    #BY gender Female
    `TradeValue_femaleTranspondent(Naira)` = round(sum(ifelse(sex=='Female',`tradeValue_(those in Naira)`,0) , na.rm=TRUE)  ,2), # format(round(sum(ifelse(sex=='Female',tradeValue,0))  ,2), scientific = FALSE), 
    `TradeValue_femaleImport(Naira)` = round(sum(ifelse(sex=='Female' & tradeDirection=='Coming in (Import)',`tradeValue_(those in Naira)`,0) , na.rm=TRUE),2), # format(round(sum(ifelse(sex=='Female' & tradeDirection=='Coming in (Import)',tradeValue,0)),2), scientific = FALSE) ,
    `TradeValue_femaleExport(Naira)` = round(sum(ifelse(sex=='Female' & tradeDirection=='Going out (Export)',`tradeValue_(those in Naira)`,0) , na.rm=TRUE),2), # format(round(sum(ifelse(sex=='Female' & tradeDirection=='Going out (Export)',tradeValue,0)),2), scientific = FALSE) ,
    
    `TradeValue_from_Catalogue(Naira)` = round(sum(ifelse(productObserved!='Other (specify)',`tradeValue_(those in Naira)`,0)  , na.rm=TRUE),2), # format(round(sum(ifelse(productObserved!='Other (specify)',tradeValue,0)),2), scientific = FALSE) ,
    `TradeValue_ImportCatalogued(Naira)` = round( sum( ifelse(tradeDirection=='Coming in (Import)' & productObserved!='Other (specify)',`tradeValue_(those in Naira)`,0) , na.rm=TRUE),2), # format(round( sum( ifelse(tradeDirection=='Coming in (Import)' & productObserved!='Other (specify)',tradeValue,0)),2), scientific = FALSE) ,
    `TradeValue_ExportCatalogued(Naira)` = round(sum(ifelse(tradeDirection=='Going out (Export)' & productObserved!='Other (specify)',`tradeValue_(those in Naira)`,0) , na.rm=TRUE),2) , # format(round(sum(ifelse(tradeDirection=='Going out (Export)' & productObserved!='Other (specify)',tradeValue,0)),2), scientific = FALSE) ,
    
    `TradeValue_from_OtherSpecified(Naira)` = round(sum(ifelse(productObserved=='Other (specify)',`tradeValue_(those in Naira)`,0) , na.rm=TRUE),2), # format(round(sum(ifelse(productObserved=='Other (specify)',tradeValue,0)),2), scientific = FALSE) ,
    
    #ANIMALS
    totalAnimalCrossingFrequency = sum(ifelse(!is.na(livestockAge), 1,0), na.rm = T),
    
    totalAnimalCrossingInwardFrequency = sum(ifelse(!is.na(livestockAge) & tradeDirection=='Coming in (Import)', 1,0), na.rm = T),
    totalAnimalCrossingOutwardFrequency =  sum(ifelse(!is.na(livestockAge) & tradeDirection=='Going out (Export)', 1,0), na.rm = T),
    `net-totalAnimalCrossingInwardFrequency` = totalAnimalCrossingOutwardFrequency - totalAnimalCrossingInwardFrequency, 
    
    totalAnimalCrossingCount = sum(ifelse(!is.na(livestockAge), commodityQuantity,0), na.rm = T),
    totalAnimalCrossingInwardCount = sum(ifelse(!is.na(livestockAge) & tradeDirection=='Coming in (Import)', commodityQuantity,0), na.rm = T),
    totalAnimalCrossingOutwardCount =  sum(ifelse(!is.na(livestockAge) & tradeDirection=='Going out (Export)', commodityQuantity,0), na.rm = T),
    `net-totalAnimalCrossingCount` = totalAnimalCrossingOutwardCount - totalAnimalCrossingInwardCount ,
    
    #Currency GHS
    totalAnimalCrossingValue_GHS = round(sum(ifelse(!is.na(livestockAge), `tradeValue_(those in GHS)`,0), na.rm = T) ,2),
   `AnimalCrossingInward(x)_Value_GHS` = round(sum(ifelse(!is.na(livestockAge) & tradeDirection=='Coming in (Import)', `tradeValue_(those in GHS)`,0), na.rm = T), 2),
   `AnimalCrossingOutward(x)_Value_GHS` = round(sum(ifelse(!is.na(livestockAge) & tradeDirection=='Going out (Export)', `tradeValue_(those in GHS)`,0), na.rm = T) ,2),
   `net-AnimalCrossing_Value_GHS(x-m)` =  `AnimalCrossingOutward(x)_Value_GHS` -  `AnimalCrossingInward(x)_Value_GHS`,
   
    #Currency CFA
   totalAnimalCrossingValue_CFA = round(sum(ifelse(!is.na(livestockAge), `tradeValue_(those in CFA)`,0), na.rm = T) ,2),
   `AnimalCrossingInward(x)_Value_CFA` = round(sum(ifelse(!is.na(livestockAge) & tradeDirection=='Coming in (Import)', `tradeValue_(those in CFA)`,0), na.rm = T), 2),
   `AnimalCrossingOutward(x)_Value_CFA` = round(sum(ifelse(!is.na(livestockAge) & tradeDirection=='Going out (Export)', `tradeValue_(those in CFA)`,0), na.rm = T) ,2),
   `net-AnimalCrossing_Value_CFA(x-m)` =  `AnimalCrossingOutward(x)_Value_CFA` -  `AnimalCrossingInward(x)_Value_CFA`,
   
    #Currency USD
   totalAnimalCrossingValue_USD = round(sum(ifelse(!is.na(livestockAge), `tradeValue_(those in USD)`,0), na.rm = T) ,2),
   `AnimalCrossingInward(x)_Value_USD` = round(sum(ifelse(!is.na(livestockAge) & tradeDirection=='Coming in (Import)', `tradeValue_(those in USD)`,0), na.rm = T), 2),
   `AnimalCrossingOutward(x)_Value_USD` = round(sum(ifelse(!is.na(livestockAge) & tradeDirection=='Going out (Export)', `tradeValue_(those in USD)`,0), na.rm = T) ,2),
   `net-AnimalCrossing_Value_USD(x-m)` =  `AnimalCrossingOutward(x)_Value_USD` -  `AnimalCrossingInward(x)_Value_USD`,
   
   totalAnimalCrossingValue_Naira = round(sum(ifelse(!is.na(livestockAge), `tradeValue_(those in Naira)`,0), na.rm = T) ,2),
   `AnimalCrossingInward(x)_Value_Naira` = round(sum(ifelse(!is.na(livestockAge) & tradeDirection=='Coming in (Import)', `tradeValue_(those in Naira)`,0), na.rm = T), 2),
   `AnimalCrossingOutward(x)_Value_Naira` = round(sum(ifelse(!is.na(livestockAge) & tradeDirection=='Going out (Export)', `tradeValue_(those in Naira)`,0), na.rm = T) ,2),
   `net-AnimalCrossing_Value_Naira(x-m)` =  `AnimalCrossingOutward(x)_Value_Naira` -  `AnimalCrossingInward(x)_Value_Naira`,
   
  ) %>% ungroup()


borderCommodityDetailValueSummary <- dataValua_Calculation %>% ungroup() %>%
  group_by(regionCode, RegionName, districtName, borderPostName,productObserved ) %>%
  summarise(
  distinct_unit_of_measures = n_distinct(unit_of_measure),
  totalTradeFrequency = n(),

  TradeQuantity_total = sum(commodityQuantity,na.rm=TRUE),
  `TradeQuantity_Import(m)` = sum(ifelse(tradeDirection=='Coming in (Import)',commodityQuantity,0),na.rm=TRUE),
  `TradeQuantity_Export(x)` =  sum(ifelse(tradeDirection=='Going out (Export)',commodityQuantity,0),na.rm=TRUE),
  `Net-TradeQuantity(x-m)`= `TradeQuantity_Export(x)`  - `TradeQuantity_Import(m)`,
  

  `TradeVolume(kg)_total` =  round(sum(catalogueWeight,na.rm=TRUE),4), # sum(ifelse(!is.na(catalogueWeight) ,catalogueWeight,0 )), # sum(catalogueWeight),
  `TradeVolume(Kg)_Import(m)` = round(sum(ifelse(tradeDirection=='Coming in (Import)',catalogueWeight,0),na.rm=TRUE) , 4),
  `TradeVolume(Kg)_Export(x)` = round( sum(ifelse(tradeDirection=='Going out (Export)',catalogueWeight,0), na.rm=TRUE) , 4),
  `Net-TradeVolume(Kg)(x-m)`= `TradeVolume(Kg)_Export(x)` - `TradeVolume(Kg)_Import(m)` ,
  
  # Trade Value GHS CURRENCY
  `TradeValue_total(GHS_currency)`= round(sum(`tradeValue_(those in GHS)` , na.rm=TRUE),2) ,# format(round(  sum(tradeValue),2), scientific = FALSE), 
  
  `TradeValue_Import(m)(GHS)` = round(sum(ifelse(tradeDirection=='Coming in (Import)',`tradeValue_(those in GHS)`,0) , na.rm=TRUE),2) ,# format(round(sum(ifelse(tradeDirection=='Coming in (Import)',tradeValue,0)),2), scientific = FALSE) ,
  `TradeValue_Export(x)(GHS)` = round(sum(ifelse(tradeDirection=='Going out (Export)',`tradeValue_(those in GHS)`,0) , na.rm=TRUE),2), # format(round(sum(ifelse(tradeDirection=='Going out (Export)',tradeValue,0)),2), scientific = FALSE) ,
  `TradeBalance(x-m)(GHS)` =round( `TradeValue_Export(x)(GHS)` - `TradeValue_Import(m)(GHS)`,2) ,
  
  # Trade Value CFA CURRENCY
  `TradeValue_total(CFA_currency)`= round(sum(`tradeValue_(those in CFA)` , na.rm=TRUE),2) ,# format(round(  sum(tradeValue),2), scientific = FALSE), 
  
  `TradeValue_Import(m)(CFA)` = round(sum(ifelse(tradeDirection=='Coming in (Import)',`tradeValue_(those in CFA)`,0) , na.rm=TRUE),2) ,# format(round(sum(ifelse(tradeDirection=='Coming in (Import)',tradeValue,0)),2), scientific = FALSE) ,
  `TradeValue_Export(x)(CFA)` = round(sum(ifelse(tradeDirection=='Going out (Export)',`tradeValue_(those in CFA)`,0) , na.rm=TRUE),2), # format(round(sum(ifelse(tradeDirection=='Going out (Export)',tradeValue,0)),2), scientific = FALSE) ,
  `TradeBalance(x-m)(CFA)` =`TradeValue_Export(x)(CFA)` - `TradeValue_Import(m)(CFA)` ,
  
  # Trade Value USD CURRENCY
  `TradeValue_total(USD_currency)`= round(sum(`tradeValue_(those in USD)` , na.rm=TRUE),2) ,# format(round(  sum(tradeValue),2), scientific = FALSE), 
  
  `TradeValue_Import(m)(USD)` = round(sum(ifelse(tradeDirection=='Coming in (Import)',`tradeValue_(those in USD)`,0) , na.rm=TRUE),2) ,# format(round(sum(ifelse(tradeDirection=='Coming in (Import)',tradeValue,0)),2), scientific = FALSE) ,
  `TradeValue_Export(x)(USD)` = round(sum(ifelse(tradeDirection=='Going out (Export)',`tradeValue_(those in USD)`,0) , na.rm=TRUE),2), # format(round(sum(ifelse(tradeDirection=='Going out (Export)',tradeValue,0)),2), scientific = FALSE) ,
  `TradeBalance(x-m)(USD)` =`TradeValue_Export(x)(USD)` - `TradeValue_Import(m)(USD)` ,
  
  
  # Trade Value Naira CURRENCY
  `TradeValue_total(Naira_currency)`= round(sum(`tradeValue_(those in Naira)` , na.rm=TRUE),2) ,# format(round(  sum(tradeValue),2), scientific = FALSE), 
  
  `TradeValue_Import(m)(Naira)` = round(sum(ifelse(tradeDirection=='Coming in (Import)',`tradeValue_(those in Naira)`,0) , na.rm=TRUE),2) ,# format(round(sum(ifelse(tradeDirection=='Coming in (Import)',tradeValue,0)),2), scientific = FALSE) ,
  `TradeValue_Export(x)(Naira)` = round(sum(ifelse(tradeDirection=='Going out (Export)',`tradeValue_(those in Naira)`,0) , na.rm=TRUE),2), # format(round(sum(ifelse(tradeDirection=='Going out (Export)',tradeValue,0)),2), scientific = FALSE) ,
  `TradeBalance(x-m)(Naira)` =`TradeValue_Export(x)(Naira)` - `TradeValue_Import(m)(Naira)` ,
  
  
  # maleTradeFrequency = sum(ifelse(sex=='Male',1,0)),,
  # maleTradeQuantity =  sum(ifelse(sex=='Male',commodityQuantity,0)),
  # maleTradeValue =round(  sum(ifelse(sex=='Male',tradeValue,0))  ,2), 
  # 
  # femaleTradeFrequency = sum(ifelse(sex=='Female',1,0)),,
  # femaleTradeQuantity =  sum(ifelse(sex=='Female',commodityQuantity,0)),
  # femaleTradeValue = round(sum(ifelse(sex=='Female',tradeValue,0)) ,2), 
  ) %>% 
  arrange(
    RegionName,districtName,productObserved
          )



borderCommodityGeneralValueSummary <- dataValua_Calculation %>% 
  mutate(
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
    # totalTradeQuantity =  sum(commodityQuantity),
    # totalTradeValue = round(sum(tradeValue)  ,2),
    # 
    # maleTradeFrequency = sum(ifelse(sex=='Male',1,0)),,
    # maleTradeQuantity =  sum(ifelse(sex=='Male',commodityQuantity,0)),
    # maleTradeValue =round(  sum(ifelse(sex=='Male',tradeValue,0))  ,2), 
    # 
    # femaleTradeFrequency = sum(ifelse(sex=='Female',1,0)),,
    # femaleTradeQuantity =  sum(ifelse(sex=='Female',commodityQuantity,0)),
    # femaleTradeValue = round(sum(ifelse(sex=='Female',tradeValue,0)) ,2), 
    
    
    TradeQuantity_total = sum(commodityQuantity,na.rm=TRUE),
    `TradeQuantity_Import(m)` = sum(ifelse(tradeDirection=='Coming in (Import)',commodityQuantity,0),na.rm=TRUE),
    `TradeQuantity_Export(x)` =  sum(ifelse(tradeDirection=='Going out (Export)',commodityQuantity,0),na.rm=TRUE),
    `Net-TradeQuantity(x-m)`= `TradeQuantity_Export(x)`  - `TradeQuantity_Import(m)`,
    
    
    `TradeVolume(kg)_total` =  round(sum(catalogueWeight,na.rm=TRUE),4), # sum(ifelse(!is.na(catalogueWeight) ,catalogueWeight,0 )), # sum(catalogueWeight),
    `TradeVolume(Kg)_Import(m)` = round(sum(ifelse(tradeDirection=='Coming in (Import)',catalogueWeight,0),na.rm=TRUE) , 4),
    `TradeVolume(Kg)_Export(x)` = round( sum(ifelse(tradeDirection=='Going out (Export)',catalogueWeight,0), na.rm=TRUE) , 4),
    `Net-TradeVolume(Kg)(x-m)`= `TradeVolume(Kg)_Export(x)` - `TradeVolume(Kg)_Import(m)` ,
    
    # Trade Value GHS CURRENCY
    `TradeValue_total(GHS_currency)`= round(sum(`tradeValue_(those in GHS)` , na.rm=TRUE),2) ,# format(round(  sum(tradeValue),2), scientific = FALSE), 
    
    `TradeValue_Import(m)(GHS)` = round(sum(ifelse(tradeDirection=='Coming in (Import)',`tradeValue_(those in GHS)`,0) , na.rm=TRUE),2) ,# format(round(sum(ifelse(tradeDirection=='Coming in (Import)',tradeValue,0)),2), scientific = FALSE) ,
    `TradeValue_Export(x)(GHS)` = round(sum(ifelse(tradeDirection=='Going out (Export)',`tradeValue_(those in GHS)`,0) , na.rm=TRUE),2), # format(round(sum(ifelse(tradeDirection=='Going out (Export)',tradeValue,0)),2), scientific = FALSE) ,
    `TradeBalance(x-m)(GHS)` =round( `TradeValue_Export(x)(GHS)` - `TradeValue_Import(m)(GHS)`,2) ,
    
    # Trade Value CFA CURRENCY
    `TradeValue_total(CFA_currency)`= round(sum(`tradeValue_(those in CFA)` , na.rm=TRUE),2) ,# format(round(  sum(tradeValue),2), scientific = FALSE), 
    
    `TradeValue_Import(m)(CFA)` = round(sum(ifelse(tradeDirection=='Coming in (Import)',`tradeValue_(those in CFA)`,0) , na.rm=TRUE),2) ,# format(round(sum(ifelse(tradeDirection=='Coming in (Import)',tradeValue,0)),2), scientific = FALSE) ,
    `TradeValue_Export(x)(CFA)` = round(sum(ifelse(tradeDirection=='Going out (Export)',`tradeValue_(those in CFA)`,0) , na.rm=TRUE),2), # format(round(sum(ifelse(tradeDirection=='Going out (Export)',tradeValue,0)),2), scientific = FALSE) ,
    `TradeBalance(x-m)(CFA)` =`TradeValue_Export(x)(CFA)` - `TradeValue_Import(m)(CFA)` ,
    
    # Trade Value USD CURRENCY
    `TradeValue_total(USD_currency)`= round(sum(`tradeValue_(those in USD)` , na.rm=TRUE),2) ,# format(round(  sum(tradeValue),2), scientific = FALSE), 
    
    `TradeValue_Import(m)(USD)` = round(sum(ifelse(tradeDirection=='Coming in (Import)',`tradeValue_(those in USD)`,0) , na.rm=TRUE),2) ,# format(round(sum(ifelse(tradeDirection=='Coming in (Import)',tradeValue,0)),2), scientific = FALSE) ,
    `TradeValue_Export(x)(USD)` = round(sum(ifelse(tradeDirection=='Going out (Export)',`tradeValue_(those in USD)`,0) , na.rm=TRUE),2), # format(round(sum(ifelse(tradeDirection=='Going out (Export)',tradeValue,0)),2), scientific = FALSE) ,
    `TradeBalance(x-m)(USD)` =`TradeValue_Export(x)(USD)` - `TradeValue_Import(m)(USD)` ,
    
    
    # Trade Value Naira CURRENCY
    `TradeValue_total(Naira_currency)`= round(sum(`tradeValue_(those in Naira)` , na.rm=TRUE),2) ,# format(round(  sum(tradeValue),2), scientific = FALSE), 
    
    `TradeValue_Import(m)(Naira)` = round(sum(ifelse(tradeDirection=='Coming in (Import)',`tradeValue_(those in Naira)`,0) , na.rm=TRUE),2) ,# format(round(sum(ifelse(tradeDirection=='Coming in (Import)',tradeValue,0)),2), scientific = FALSE) ,
    `TradeValue_Export(x)(Naira)` = round(sum(ifelse(tradeDirection=='Going out (Export)',`tradeValue_(those in Naira)`,0) , na.rm=TRUE),2), # format(round(sum(ifelse(tradeDirection=='Going out (Export)',tradeValue,0)),2), scientific = FALSE) ,
    `TradeBalance(x-m)(Naira)` =`TradeValue_Export(x)(Naira)` - `TradeValue_Import(m)(Naira)` ,
    
    
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

