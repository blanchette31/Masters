# Prepare workspace
rm(list = ls())

# Load libraries
library(tidyverse)
library(patchwork)  # For combining plots

# Load data
era5 <- read.csv("Data/Processed/precip/era5_cut_buoy.csv")

# Filter data for DOY 257 onward
era5_filtered <- era5 %>%
  filter(doy >= 257) %>%
  arrange(year, doy)

# Table: Raw data for DOY 257
table_doy257 <- era5_filtered %>%
  filter(doy == 257) %>%
  select(year, doy, air_temp.C., wind_sp.m.s., global_rad.MJ.m2., rain)

# Table: Summary statistics for DOY 257 onward
summary_doy257_onward <- era5_filtered %>%
  group_by(year) %>%
  summarize(
    total_temp = sum(air_temp.C., na.rm = TRUE),
    average_temp = mean(air_temp.C., na.rm = TRUE),
    total_rain_vol_perc = sum(rain*(1075472+179000)/1000/836000*100, na.rm = TRUE),
    total_rad = sum(global_rad.MJ.m2., na.rm = TRUE),
    mean_wind = mean(wind_sp.m.s., na.rm = TRUE),
    max_wind = max(wind_sp.m.s., na.rm = TRUE)
  ) %>%
  arrange(year)



# Print tables
print(table_doy257)
print(summary_doy257_onward)

# Save tables to CSV
write.csv(table_doy257, "Data/Processed/comments/raw_era5_doy257.csv", row.names = FALSE)
write.csv(summary_doy257_onward, "Data/Processed/comments/summary_era5_doy257_onward.csv", row.names = FALSE)
