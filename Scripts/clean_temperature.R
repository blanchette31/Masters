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
    select(timestamp, waterT01_0.5m_oC, waterT02_0.5m_oC, waterT03_1m_oC, waterT06_4m_oC, waterT07_4.5m_oC, waterT08_5.5m_oC, waterT09_6.5m_oC, waterT10_7.5m_oC, waterT11_8.5m_oC, waterT12_10.5m_oC, DOa_0.5m_uM, DOb_5.5m_uM, DOc_8.5m_uncorr_uM)
  
  df <- df %>% 
    rename(waterT03_1.0m_oC = waterT03_1m_oC,
           waterT06_4.0m_oC = waterT06_4m_oC,
    )
  
  #Set timestamp to correct format (still not appearing in EST though)
  df$timestamp <- as.POSIXct(df$timestamp, format = "%Y-%m-%d%H:%M:%S", tz = "America/New_York")
  
  #Extract date, year and doy from timestamp
  df = df %>%
    mutate(
      Date = as.Date(timestamp, format = "%Y%m%d"),
      doy = strftime(timestamp, format = "%j"),
      year = strftime(timestamp, format = "%Y"))
  
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
    group_by(id, Depth, Date) %>%  
    summarise(
      Temperature = mean(Temperature)
    )
  
  # Save the cleaned CSV file for the current year
  write_csv(df_final, file = paste0("Data/Processed/lake/temp_", substr(file, 12, 15), ".csv"))
}
