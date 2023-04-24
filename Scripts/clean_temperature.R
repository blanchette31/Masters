## Prepare workspace ##
rm(list = ls())

library(ggplot2)
library(tidyverse)
library(tidyr)
library(dplyr)
library(data.table)
library(readr)

# Set the directory containing the raw CSV files
dir_raw <- "Data/Raw/boue/Croche2016-2021/"

# Get a list of all CSV files in the directory
files <- list.files(path = dir_raw, pattern = "\\.csv$")

# Loop over all CSV files
for (file in files) {
  # Load data
  df <- read.csv(paste0(dir_raw, file), header = TRUE)
  
  df <- df %>% 
    select(timestamp, airP_hPa, waterT01_0.5m_oC, waterT02_0.5m_oC, waterT03_1m_oC, waterT06_4m_oC, waterT07_4.5m_oC, waterT08_5.5m_oC, waterT09_6.5m_oC, waterT10_7.5m_oC, waterT11_8.5m_oC, waterT12_10.5m_oC, DOa_0.5m_uM, DOb_5.5m_uM, DOc_8.5m_uncorr_uM)
  
  df <- df %>% 
    rename(waterT03_1.0m_oC = waterT03_1m_oC,
           waterT06_4.0m_oC = waterT06_4m_oC,
    )
  
  #Set timestamp to correct format (still not appearing in EST though)
  df$timestamp <- as.POSIXct(df$timestamp, format = "%Y-%m-%d%H:%M:%S", tz = "America/New_York")
  df$timestamp_est <- with_tz(df$timestamp, tzone = "EST")
  
  #Extract date, year and doy from timestamp
  df = df %>%
    mutate(
      Date = as.Date(timestamp_est, format = "%Y%m%d"),
      doy = strftime(timestamp_est, format = "%j"),
      year = strftime(timestamp_est, format = "%Y"))
  
  df_longer <- df %>% 
    pivot_longer(
      cols = starts_with("waterT"),
      names_to = "id", 
      cols_vary = "slowest")
  
  df_longer$Depth <- as.numeric(gsub("waterT\\d+_(\\d+\\.\\d)m_oC", "\\1", df_longer$id))
  
  names(df_longer)[names(df_longer) == "value"] <- "Temperature"
  df_longer <- df_longer[complete.cases(df_longer$Temperature), ]
  df_longer$Date <- as.Date(df_longer$Date)
  df_final <- df_longer %>% 
    group_by(id, Depth, Date, year) %>%  
    summarise(
      Temperature = mean(Temperature),
      airP_hPa = mean(airP_hPa)
      
    )
  
  # Save the cleaned CSV file for the current year
  write_csv(df_final, file = paste0("Data/Processed/lake/temp_", substr(file, 12, 15), ".csv"))
}


# clean 2015 seperately

df_2015 = read_csv("Data/Raw/boue/Crochefinal2015.csv")


df_2015 <- df_2015 %>% 
  select(timestamp, airP_hPa, waterT01_1.7m_oC, waterT02_4m_oC, waterT03_5m_oC, waterT04_6m_oC, waterT05_7m_oC,waterT06_8m_oC, waterT7_9m_oC, waterT8_10m_oC, waterT9_11m_oC, waterT10_12m_oC)

df_2015 <- df_2015 %>% 
  rename(waterT02_4.0m_oC = waterT02_4m_oC,
         waterT03_5.0m_oC = waterT03_5m_oC, 
         waterT04_6.0m_oC = waterT04_6m_oC,
         waterT05_7.0m_oC = waterT05_7m_oC,
         waterT06_8.0m_oC = waterT06_8m_oC, 
         waterT07_9.0m_oC = waterT7_9m_oC, 
         waterT08_10.0m_oC = waterT8_10m_oC, 
         waterT09_11.0m_oC = waterT9_11m_oC, 
         waterT10_12.0m_oC = waterT10_12m_oC)

#Set timestamp to correct format (still not appearing in EST though)
df_2015$timestamp <- as.POSIXct(df_2015$timestamp, format = "%Y-%m-%d%H:%M:%S", tz = "America/New_York")

df_2015$timestamp_est <- with_tz(df_2015$timestamp, tzone = "EST")

#Extract date, year and doy from timestamp
df_2015 = df_2015 %>%
  mutate(
    Date = as.Date(timestamp_est, format = "%Y%m%d"),
    doy = strftime(timestamp_est, format = "%j"),
    year = strftime(timestamp_est, format = "%Y"))

df_longer_2015 <- df_2015 %>% 
  pivot_longer(
    cols = starts_with("waterT"),
    names_to = "id", 
    cols_vary = "slowest")

df_longer_2015$Depth <- as.numeric(gsub("waterT\\d+_(\\d+\\.\\d)m_oC", "\\1", df_longer_2015$id))

names(df_longer_2015)[names(df_longer_2015) == "value"] <- "Temperature"
df_longer_2015 <- df_longer_2015[complete.cases(df_longer_2015$Temperature), ]
df_longer_2015$Date <- as.Date(df_longer_2015$Date)
df_final_2015 <- df_longer_2015 %>% 
  group_by(id, Depth, Date, year) %>%  
  summarise(
    Temperature = mean(Temperature),
    airP_hPa = mean(airP_hPa)
  )
write_csv(df_final_2015, "Data/Processed/lake/temp_2015.csv")


## 2022 ## 
df_2022 <- read.csv("Data//Raw//boue//2022CR6_QAQC3.csv", header = TRUE)


#select desired columns
df_2022 <- df_2022 %>% 
  select(timestamp, AirPressure_hpa_Smp, TChain_Temp_1_Deg_C_Smp_0.65m, TChain_Temp_2_Deg_C_Smp_0.65m, TChain_Temp_3_Deg_C_Smp_0.73m, TChain_Temp_4_Deg_C_Smp_1.73m, TChain_Temp_5_Deg_C_Smp_2.73m, TChain_Temp_6_Deg_C_Smp_3.73m, TChain_Temp_7_Deg_C_Smp_4.58m, TChain_Temp_8_Deg_C_Smp_5.58m, TChain_Temp_9_Deg_C_Smp_6.53m, TChain_Temp_10_Deg_C_Smp_7.53m, TChain_Temp_11_Deg_C_Smp_8.53m, TChain_Temp_12_Deg_C_Smp_10.53m, TChain_O2_1_umol_per_L_Smp_0.5m, TChain_O2_2_umol_per_L_Smp_5.48m, TChain_O2_3_umol_per_L_Smp_8.43m)


#Set timestamp to correct format (still not appearing in EST though)
df_2022$timestamp <- as.POSIXct(df_2022$timestamp, format = "%Y-%m-%d%H:%M:%S", tz = "America/New_York")
df_2022$timestamp_est <- with_tz(df_2022$timestamp, tzone = "EST")

#Extract date, year and doy from timestamp
df_2022 <- df_2022 %>%
  mutate(
    Date = as.Date(timestamp_est, format = "%Y%m%d"),
    doy = strftime(timestamp_est, format = "%j"),
    year = strftime(timestamp_est, format = "%Y"))

# pivot dataframe from long to wide
df_longer_2022 <- df_2022 %>%
  pivot_longer(
    cols = starts_with("TChain_Temp"),
    names_to = "id",
    values_to = "value",
    cols_vary = "slowest") %>% 
  mutate(Depth = as.numeric(sub("TChain_Temp_([0-9]+)_Deg_C_Smp_([0-9\\.]+)m", "\\2", id)))

names(df_longer_2022)[names(df_longer_2022) == "value"] <- "Temperature"
df_longer_2022 <- df_longer_2022[complete.cases(df_longer_2022$Temperature), ]
df_longer_2022$Date <- as.Date(df_longer_2022$Date)
df_final_2022 <- df_longer_2022 %>% 
  group_by(id, Depth, Date, year) %>%  
  summarise(
    Temperature = mean(Temperature),
    airP_hPa = mean(AirPressure_hpa_Smp)
  )

write_csv(df_final_2022, "Data/Processed/lake/temp_2022.csv")


df_2022 <- df_2022 %>% 
  rename(airP_hPa = AirPressure_hpa_Smp, 
        '0.65A_m' = TChain_Temp_1_Deg_C_Smp_0.65m, 
        '0.65B_m' = TChain_Temp_2_Deg_C_Smp_0.65m, 
        '0.73m' = TChain_Temp_3_Deg_C_Smp_0.73m, 
        '1.73m' = TChain_Temp_4_Deg_C_Smp_1.73m, 
        '2.73m' = TChain_Temp_5_Deg_C_Smp_2.73m, 
        '3.73m' = TChain_Temp_6_Deg_C_Smp_3.73m, 
        '4.58m' = TChain_Temp_7_Deg_C_Smp_4.58m, 
        '5.58m' = TChain_Temp_8_Deg_C_Smp_5.58m, 
        '6.53m' = TChain_Temp_9_Deg_C_Smp_6.53m, 
        '7.53m' = TChain_Temp_10_Deg_C_Smp_7.53m, 
        '8.53m' = TChain_Temp_11_Deg_C_Smp_8.53m, 
        '10.53m' =  TChain_Temp_12_Deg_C_Smp_10.53m)



df_2022_wide <- df_2022 %>% 
  group_by(Date, year) %>% 
  summarise(airP_hPa = mean(airP_hPa),
            '0.65A_m' = mean('0.65A_m'),
            '0.65B_m' = mean('0.65B_m'),
            '0.73m' = mean('0.73m'),
            '1.73m' = mean('1.73m'),
            '2.73m' = mean('2.73m'),
            '3.73m' = mean('3.73m'),
            '4.58m' = mean('4.58m'),
            '5.58m' = mean('5.58m'),
            '6.53m' = mean('6.53m'),
            '7.53m' = mean('7.53m'),
            '8.53m' = mean('8.53m'),
            '10.53m' = mean('10.53m'))
