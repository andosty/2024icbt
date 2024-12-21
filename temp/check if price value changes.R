#check if price value changes to reflect month of data collection

library(dplyr)

priceData <- readRDS("C:/2024ICBT/Data_final/icbt_data.RDS") %>% 
  filter(
   productObserved== 'cement'
  ) %>% distinct(month, .keep_all = T)