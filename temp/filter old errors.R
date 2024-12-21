library(dplyr)

er <- readRDS("C:/2024ICBT/Data_final/error_data.RDS") %>%
  filter(createdDate < '2024-12-08')


er <- readRDS("C:/2024ICBT/Data_final/error_data.RDS") %>%
  filter(createdDate > '2024-12-08')

saveRDS(er,"C:/2024ICBT/Data_final/error_data.RDS" )


kk <- readRDS("C:/2024ICBT/Data_final/icbt_data.RDS") %>%
  select(-interview_id) %>%
  rename(
    interview_id = InterviewId ,
         UserName = ResponsibleName)

saveRDS(kk, "C:/2024ICBT/Data_final/icbt_data.RDS")

