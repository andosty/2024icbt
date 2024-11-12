#interviewers with cases not rejecting

notGettingRejects <- read_excel("C:/2024ICBT/temp/InterviewersNotReceivingRejects.xlsx")

errorCaseData <- readRDS("C:/2024ICBT/Data_final/error_data.RDS")

interviewerList <- errorCaseData %>%
      filter(
        interview_id %in% notGettingRejects$interviewID
      ) %>%
  mutate(team_number=parse_number(team_number))%>%
  group_by(regionCode, RegionName,districtCode,districtName,borderPostName, team_number,enumerator_name,enumerator_contact) %>%
  summarise(
    uniqueCount_errorCases= n_distinct(interview_key),
    totalErrors = n()
  ) %>%
  arrange(
    RegionName,team_number,enumerator_name
  )
  

write.xlsx(interviewerList, "C:/2024ICBT/enums_not_receiving_rejectedCases.xlsx")




75-31-09-96
fec55bf65cd44ed3b16860794d0a3cba
Jessie Elom Adjorlolo
0249511561
9
A man going out in a tricycle
1
4 pieces of 1/2 plywood
product Qty mismatch
product description implies the quantity ='12'. commodityQuantity entered = '4'. production description = '4 pieces of 1/2 plywood'