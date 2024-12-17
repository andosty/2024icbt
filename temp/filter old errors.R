library(dplyr)

er <- readRDS("C:/2024ICBT/Data_final/error_data.RDS") %>%
  filter(createdDate < '2024-12-08')


er <- readRDS("C:/2024ICBT/Data_final/error_data.RDS") %>%
  filter(createdDate > '2024-12-08')

saveRDS(er,"C:/2024ICBT/Data_final/error_data.RDS" )


region <- readRDS("C:/2024ICBT/Data_final/icbt_data.RDS") %>%
  filter(createdDate > '2024-12-08') %>%
distinct(RegionName)
