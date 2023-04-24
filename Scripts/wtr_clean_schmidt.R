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

df_2022 <- read.csv("Data/Raw/boue/2022CR6_QAQC3.csv")



df_2022 <- df_2022 %>% 
  select(timestamp, TChain_Temp_1_Deg_C_Smp_0.65m,TChain_Temp_2_Deg_C_Smp_0.65m,TChain_Temp_3_Deg_C_Smp_0.73m,TChain_Temp_4_Deg_C_Smp_1.73m,TChain_Temp_5_Deg_C_Smp_2.73m,TChain_Temp_6_Deg_C_Smp_3.73m,TChain_Temp_7_Deg_C_Smp_4.58m,TChain_Temp_8_Deg_C_Smp_5.58m,TChain_Temp_9_Deg_C_Smp_6.53m,TChain_Temp_10_Deg_C_Smp_7.53m,TChain_Temp_11_Deg_C_Smp_8.53m,TChain_Temp_12_Deg_C_Smp_10.53m) %>% 
  rename( datetime = timestamp, 
          wtr_0.65 = TChain_Temp_1_Deg_C_Smp_0.65m,
          wtr_00.65 = TChain_Temp_2_Deg_C_Smp_0.65m,
          wtr_0.73 = TChain_Temp_3_Deg_C_Smp_0.73m,
          wtr_1.73 = TChain_Temp_4_Deg_C_Smp_1.73m,
          wtr_2.73 = TChain_Temp_5_Deg_C_Smp_2.73m,
          wtr_3.73 = TChain_Temp_6_Deg_C_Smp_3.73m,
          wtr_4.58 = TChain_Temp_7_Deg_C_Smp_4.58m,
          wtr_5.58 = TChain_Temp_8_Deg_C_Smp_5.58m,
          wtr_6.53 = TChain_Temp_9_Deg_C_Smp_6.53m,
          wtr_7.53 = TChain_Temp_10_Deg_C_Smp_7.53m,
          wtr_8.53 = TChain_Temp_11_Deg_C_Smp_8.53m,
          wtr_10.53 = TChain_Temp_12_Deg_C_Smp_10.53m
  )


write.csv(df_2022, "Data/Processed/lake/wtr_temp_for_schmidt_2022.csv", row.names = FALSE)


df_2015 <- read.csv("Data/Raw/boue/Crochefinal2015.csv")

df_2015 <- df_2015 %>% 
  select(timestamp, waterT01_1.7m_oC, waterT02_4m_oC, waterT03_5m_oC, waterT04_6m_oC, waterT05_7m_oC, waterT06_8m_oC, waterT7_9m_oC, waterT8_10m_oC, waterT9_11m_oC, waterT10_12m_oC) %>% 
  rename(datetime = timestamp, 
         wtr_1.7 = waterT01_1.7m_oC,
         wtr_4.0 = waterT02_4m_oC, 
         wtr_5.0 = waterT03_5m_oC, 
         wtr_6.0 = waterT04_6m_oC, 
         wtr_7.0 = waterT05_7m_oC, 
         wtr_8.0 = waterT06_8m_oC, 
         wtr_9.0 = waterT7_9m_oC, 
         wtr_10.0 = waterT8_10m_oC, 
         wtr_11.0 = waterT9_11m_oC, 
         wtr_12.0 = waterT10_12m_oC
  )

write.csv(df_2015, "Data/Processed/lake/wtr_temp_for_schmidt_2015.csv", row.names = FALSE)
