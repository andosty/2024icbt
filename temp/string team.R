library(haven)
teamDATA <- read_dta("C:/2024ICBT/Data_Download/HQ_extracted/ICBT_MAIN_FIELD_WORK.dta") %>% select(
  "team_number","id02",   "id02a","id03" ,"id03b"
  # "team_number", "quarter","id02",   "id02a", "id03", "id03b","id04", "id06", "id06b" 
) %>% 
  distinct() %>%
  # select(id02a,id02, team_number,districtName) %>%
  rename (RegionName =id02,
          regionCode= id02a,
         
          districtName =  id03
          ) %>%
  mutate(
    regionCode = case_when(
                    regionCode==16 & str_to_lower(RegionName)=='savanna'~ 13, #fix savanna with wrong region codes
                    str_to_lower(districtName)== 'bole' & regionCode==16 ~ 13,
                    str_to_lower(RegionName)=='northern'~ 12, # fix northern region cases with wrong region codes
                    TRUE ~ regionCode
                           ),
    team_number = case_when(
                  regionCode==1 & team_number=='WR Team 2' ~ 'WR 2',
                  regionCode==13 & team_number=='UW Team 2' ~ 'Savannah Team 2',
                  regionCode==13 & team_number=='UW Team 1' ~ 'Savannah Team 1',
                  TRUE ~ team_number),
    RegionName = case_when(str_to_lower(RegionName)=='savanna'~ 'SAVANNAH',
                           TRUE ~ RegionName),
    RegionName= str_to_title(RegionName)
  ) %>%
  distinct(regionCode,RegionName,team_number) %>% 
  rename(teamString=team_number) %>%
  mutate(
    # teamString = team_number,
    team_number = parse_number(teamString)
  ) %>%
  arrange(regionCode, RegionName, team_number) %>%
  group_by(RegionName,team_number ) %>%
  mutate(duplicate.flag = n() > 1) %>%
  filter(duplicate.flag)  #Then, you could use filter to subset each group, as needed:
  # %>% filter(!duplicate.flag)  #keep none dups
  

teamDATA <- teamDATA %>%
  mutate()

# check with processed data
icbt_data <- readRDS("C:/2024ICBT/Data_final/icbt_data.RDS") %>%
  select(
    RegionName,regionCode,team_number
  ) %>%
  distinct() %>%
  arrange(regionCode, RegionName, team_number) %>%
  group_by(RegionName,team_number ) %>%
  mutate(duplicate.flag = n() > 1) %>%
  filter(duplicate.flag)  #Then, you could use filter to subset each group, as needed:




