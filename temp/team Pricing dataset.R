#team pricing dataset
# ANDY NEW DO

library(readxl) 
library(tidyverse) 

# specifying the path for file 
path <- "C:/2024ICBT/pricing/"
# "icbts sup_team.xlsx"
# set the working directory  
setwd(path) 

# accessing all the sheets  
sheet = excel_sheets("icbts sup_team.xlsx") 

# applying sheet names to dataframe names 
RegionTeams = lapply(setNames(sheet, sheet),  
                    function(x) read_excel("icbts sup_team.xlsx", sheet=x)) 

# attaching all dataframes together 
RegionTeams = bind_rows(RegionTeams, .id="Sheet") %>% 
  rename(Team=team_number,
         SupName = enumerator_name) %>%
  mutate(
    team_number = parse_number(Team),
    teamString = str_to_upper(paste(region, "Team",team_number))
  ) %>% select(-Sheet, -`_responsible`,-Team)

#get supervisors handling more than 1 team
SupMoreTeams <- RegionTeams %>%
arrange(, SupName,region, team_number,teamString) %>%
  group_by(SupName,region ) %>%
  mutate(duplicate.flag = n() > 1) %>%
  filter(duplicate.flag) %>% 
  mutate(
    newTeamNumber = first(team_number)
  ) # %>% filter(newTeamNumber !=team_number )

#Take the dataset and group Product, UOM, by all teams
# check with processed data
icbt_data <- readRDS("C:/2024ICBT/Data_final/icbt_data.RDS") %>%
  mutate(
    ### Change those two Enumerators for Nov to OTI
    # PRECIOUS ELORM ADZO AGIDI
    # ANOMAH PETER GODLOVE
    # and Month == "November", 
    # change the region to region code and name for OTI
    RegionName=  case_when(
                          ( (enumerator_name=="PRECIOUS ELORM ADZO AGIDI" |
                               enumerator_name=="ANOMAH PETER GODLOVE" )
                            & team_number==13 & month == "November") ~ "Oti",
                          TRUE ~ RegionName
                    ),
    regionCode=  case_when(
                          ( (enumerator_name=="PRECIOUS ELORM ADZO AGIDI" |
                               enumerator_name=="ANOMAH PETER GODLOVE" )
                            & team_number==13 & month == "November") ~ 11,
                          TRUE ~ 11
                    )
  ) %>%
  filter( month == "November") %>%
  select(
    regionCode, RegionName, districtCode, districtName, team_number,
    productObserved, productObserved_otherSpecify,
    unit_of_measure, unit_of_measure_otherSpecify
  ) %>%
  distinct()

#work on the team numbers merge for same Supervisor for multiTeam
for (i in 1:nrow(SupMoreTeams %>% filter(newTeamNumber !=team_number ))){
  # row <- SupMoreTeams[1,]
  row <- SupMoreTeams[i,]
  
  icbt_data <- icbt_data %>%
    mutate(
       team_number = case_when(team_number== row$team_number ~ row$team_number,
                                   TRUE ~ team_number)
    )
}

# generate the commodity item concatenate with unit of measrue in various forms of permutations
# 1: CatListProducts with UoM NOT as OtherSpecified
mainProducts_UomMain <- icbt_data %>%
  filter(
    productObserved != "Other (specify)" & unit_of_measure != "Other (specify)"
         ) %>% 
  mutate(Commodity = paste(str_to_title(productObserved),"-",str_to_lower(unit_of_measure))) %>%
  select(regionCode, RegionName, districtCode, districtName, team_number, Commodity  )

# 2: CatListProducts with UoM also as OtherSpecified
mainProducts_OumOtherSpecified <- icbt_data %>%
  filter(
    productObserved != "Other (specify)" & unit_of_measure == "Other (specify)"
         ) %>%
  mutate(Commodity = paste(str_to_title(productObserved),"-",str_to_lower(unit_of_measure_otherSpecify))) %>%
  select(regionCode, RegionName, districtCode, districtName, team_number, Commodity  )


# 3: OtherSpecifiedProducts with UoM NOT as OtherSpecified
otherSpecifiedProducts_UomMain <- icbt_data %>%
  filter(
    productObserved == "Other (specify)" & unit_of_measure != "Other (specify)"
  ) %>%
  mutate(Commodity = paste(str_to_title(productObserved_otherSpecify),"-",str_to_lower(unit_of_measure))) %>%
  select(regionCode, RegionName, districtCode, districtName, team_number, Commodity  )


# 4: OtherSpecifiedProducts with UoM also as OtherSpecified
otherProducts_UomOtherSpecified <- icbt_data %>%
  filter(
    productObserved == "Other (specify)" & unit_of_measure == "Other (specify)"
         ) %>%
  mutate(Commodity = paste(str_to_title(productObserved_otherSpecify),"-",str_to_lower(unit_of_measure_otherSpecify))) %>%
  select(regionCode, RegionName, districtCode, districtName, team_number, Commodity  )

finalProductTeams <- rbind(
  mainProducts_UomMain, mainProducts_OumOtherSpecified,
  otherSpecifiedProducts_UomMain, otherProducts_UomOtherSpecified
) %>% mutate(
  teamString = paste(RegionName, "Team" , team_number)
)

rm(  mainProducts_UomMain, mainProducts_OumOtherSpecified,
     otherSpecifiedProducts_UomMain, otherProducts_UomOtherSpecified,
     icbt_data
     )

UniqueNationalCommodityList <- finalProductTeams %>% distinct(Commodity) %>%
  arrange(Commodity) %>%
  mutate(Commodity_ID = row_number())

UniqueTeams <- finalProductTeams %>% distinct(RegionName,team_number,teamString) %>%
  arrange(RegionName,team_number) %>%
  select(RegionName,teamString, team_number ) %>%
  mutate(Team_ID = row_number())

ALL_TeamsCommodityList <- finalProductTeams %>% left_join(
  UniqueTeams, by = join_by(RegionName, team_number, teamString)
) %>% left_join(UniqueNationalCommodityList, by = join_by(Commodity) ) %>%
  group_by(teamString, team_number) %>% distinct(Commodity_ID, .keep_all = T)

UniqueCommodities <- UniqueNationalCommodityList 

# SelectionCommodity <- ALL_TeamsCommodityList %>%
#   select(teamString,team_number,Team_ID,Commodity_ID )

#save Unique Products

## save to file
# filename <- c("")
filename <-paste("unique Product ICBT.xlsx", sep="")

#save enum stats
DataSave <- UniqueCommodities 
if(length(DataSave)>0){
  sheetName_word <- "uniqueProducts"
  if (file.exists(filename)) {
    wb <- loadWorkbook(filename) #load the workbook
    #check sheet existence name(wb) gives name of sheet in wb
    for (sheetnames in names(wb)){
      if (sheetnames == sheetName_word) {
        removeWorksheet(wb, sheetName_word) } }
  }else{
    wb <- createWorkbook(filename) } #create the workbook if file does not exist
  addWorksheet(wb,sheetName_word)
  writeData(wb, sheet = sheetName_word, DataSave)
  saveWorkbook(wb,filename,overwrite = TRUE)
}

filename <-paste("unique Teams ICBT.xlsx", sep="")

DataSave <- UniqueTeams 
if(length(DataSave)>0){
  sheetName_word <- "uniqueTeams"
  if (file.exists(filename)) {
    wb <- loadWorkbook(filename) #load the workbook
    #check sheet existence name(wb) gives name of sheet in wb
    for (sheetnames in names(wb)){
      if (sheetnames == sheetName_word) {
        removeWorksheet(wb, sheetName_word) } }
  }else{
    wb <- createWorkbook(filename) } #create the workbook if file does not exist
    addWorksheet(wb,sheetName_word)
  writeData(wb, sheet = sheetName_word, DataSave)
  saveWorkbook(wb,filename,overwrite = TRUE)
}

#Pricing COmmodites for Various Teams In region
filename <-paste("Team Pricing Commodities.xlsx", sep="")


SelectionCommodity <- ALL_TeamsCommodityList  %>% arrange(RegionName,team_number,Team_ID,Commodity_ID)
regionMetaName <- ALL_TeamsCommodityList %>% ungroup() %>% distinct(RegionName)

for (i in 1:nrow(regionMetaName)){
  regionPricing <- regionMetaName[i,]
  
  if(regionPricing$RegionName=="Volta"){
    minTeamNumber <- min(unique(ALL_TeamsCommodityList$team_number))
    medianTeamNumber <- median(unique(ALL_TeamsCommodityList$team_number))
    maxTeamNumber <- max(unique(ALL_TeamsCommodityList$team_number))
    
    #save first have of volta teams
    DataSave <- ALL_TeamsCommodityList %>% filter(RegionName==regionPricing$RegionName) %>%
      filter(team_number >=minTeamNumber & team_number <=medianTeamNumber) %>%
      select(teamString,team_number,Team_ID,Commodity_ID )
    if(length(DataSave)>0){
      sheetName_word <- paste0(regionPricing$RegionName,1)
      if (file.exists(filename)) {
        wb <- loadWorkbook(filename) #load the workbook
        #check sheet existence name(wb) gives name of sheet in wb
        for (sheetnames in names(wb)){
          if (sheetnames == sheetName_word) {
            removeWorksheet(wb, sheetName_word) } }
      }else{
        wb <- createWorkbook(filename) } #create the workbook if file does not exist
      addWorksheet(wb,sheetName_word)
      writeData(wb, sheet = sheetName_word, DataSave)
      saveWorkbook(wb,filename,overwrite = TRUE)
    }
    #save second have of volta teams
    DataSave <- ALL_TeamsCommodityList %>% filter(RegionName==regionPricing$RegionName) %>%
      filter(team_number > medianTeamNumber & team_number <=maxTeamNumber) %>%
      select(teamString,team_number,Team_ID,Commodity_ID )
    if(length(DataSave)>0){
      sheetName_word <- paste0(regionPricing$RegionName,2)
      if (file.exists(filename)) {
        wb <- loadWorkbook(filename) #load the workbook
        #check sheet existence name(wb) gives name of sheet in wb
        for (sheetnames in names(wb)){
          if (sheetnames == sheetName_word) {
            removeWorksheet(wb, sheetName_word) } }
      }else{
        wb <- createWorkbook(filename) } #create the workbook if file does not exist
      addWorksheet(wb,sheetName_word)
      writeData(wb, sheet = sheetName_word, DataSave)
      saveWorkbook(wb,filename,overwrite = TRUE)
    }
    
  }else{
    DataSave <- ALL_TeamsCommodityList %>% filter(RegionName==regionPricing$RegionName) %>%
      select(teamString,team_number,Team_ID,Commodity_ID )
    if(length(DataSave)>0){
      sheetName_word <- regionPricing$RegionName
      if (file.exists(filename)) {
        wb <- loadWorkbook(filename) #load the workbook
        #check sheet existence name(wb) gives name of sheet in wb
        for (sheetnames in names(wb)){
          if (sheetnames == sheetName_word) {
            removeWorksheet(wb, sheetName_word) } }
      }else{
        wb <- createWorkbook(filename) } #create the workbook if file does not exist
      addWorksheet(wb,sheetName_word)
      writeData(wb, sheet = sheetName_word, DataSave)
      saveWorkbook(wb,filename,overwrite = TRUE)
    }
  }
}



## save to supervisorTeams
filename <-paste("Supervisor Teams ICBT.xlsx", sep="")

#save all supervisory teams
DataSave <- RegionTeams 
if(length(DataSave)>0){
  sheetName_word <- "allSupervisorTeams"
  if (file.exists(filename)) {
    wb <- loadWorkbook(filename) #load the workbook
    #check sheet existence name(wb) gives name of sheet in wb
    for (sheetnames in names(wb)){
      if (sheetnames == sheetName_word) {
        removeWorksheet(wb, sheetName_word) } }
  }else{
    wb <- createWorkbook(filename) } #create the workbook if file does not exist
  addWorksheet(wb,sheetName_word)
  writeData(wb, sheet = sheetName_word, DataSave)
  saveWorkbook(wb,filename,overwrite = TRUE)
}
#save sups with more than 1 team
DataSave <- SupMoreTeams %>% select(-duplicate.flag)
if(length(DataSave)>0){
  sheetName_word <- "Sups_more_Teams"
  if (file.exists(filename)) {
    wb <- loadWorkbook(filename) #load the workbook
    #check sheet existence name(wb) gives name of sheet in wb
    for (sheetnames in names(wb)){
      if (sheetnames == sheetName_word) {
        removeWorksheet(wb, sheetName_word) } }
  }else{
    wb <- createWorkbook(filename) } #create the workbook if file does not exist
  addWorksheet(wb,sheetName_word)
  writeData(wb, sheet = sheetName_word, DataSave)
  saveWorkbook(wb,filename,overwrite = TRUE)
}