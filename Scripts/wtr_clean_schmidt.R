# Prepare workspace 
rm(list = ls())

library(tidyverse)
library(rLakeAnalyzer)


# load data
df <- list.files(path = "Data/Raw/boue/Croche2016-2021", pattern = ".csv", full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows

df <- df %>% 
  select(timestamp, waterT01_0.5m_oC, waterT02_0.5m_oC, waterT03_1m_oC, waterT06_4m_oC, waterT07_4.5m_oC, waterT08_5.5m_oC, waterT09_6.5m_oC, waterT10_7.5m_oC, waterT11_8.5m_oC, waterT12_10.5m_oC) 

df <-  df %>% 
  rename(datetime = timestamp,
         wtr_0.5 = waterT01_0.5m_oC,
         wtr_00.5 = waterT02_0.5m_oC, 
         wtr_1.0 = waterT03_1m_oC, 
         wtr_4.0 = waterT06_4m_oC, 
         wtr_4.5 = waterT07_4.5m_oC, 
         wtr_5.5 = waterT08_5.5m_oC, 
         wtr_6.5 = waterT09_6.5m_oC, 
         wtr_7.5 = waterT10_7.5m_oC, 
         wtr_8.5 = waterT11_8.5m_oC, 
         wtr_10.5 =waterT12_10.5m_oC)




write.csv(df, "Data/Processed/lake/wtr_temp_for_schmidt.csv", row.names = FALSE)

