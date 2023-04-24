# Prepare workspace
rm(list = ls())

# Load libraries
library(tidyverse)
library(patchwork)  # For combining plots

# Load data
era5 <- read.csv("Data/Processed/precip/era5_prep.csv")
pheno <- read.csv("Data/Processed/phenocam/pheno_clean.csv")

# Filter to exclude everything before DOY 121
era5 <- era5 %>%
  filter(doy >= 121, doy <= 307)

# Merge phenocam data
df <- merge(era5, pheno[, c("doy_start", "doy_end", "year")], by = "year")

# Filter data for DOY 180–220
df_doy180_220 <- df %>%
  filter(doy >= 180, doy <= 220) %>%
  arrange(year, doy)

# Table: Raw data for DOY 200
table_doy200 <- df_doy180_220 %>%
  filter(doy == 200) %>%
  select(year, doy, air_temp.C., wind_sp.m.s., global_rad.MJ.m2., rain)

# Table: Summary statistics for DOY 180–220
summary_doy180_220 <- df_doy180_220 %>%
  group_by(year) %>%
  summarize(
    total_temp = sum(air_temp.C., na.rm = TRUE),
    average_temp = mean(air_temp.C., na.rm = TRUE),
    total_rain = sum(rain, na.rm = TRUE),
    total_rad = sum(global_rad.MJ.m2., na.rm = TRUE),
    mean_wind = mean(wind_sp.m.s., na.rm = TRUE),
    max_wind = max(wind_sp.m.s., na.rm = TRUE)
  ) %>%
  arrange(year)

# Print tables
print(table_doy200)
print(summary_doy180_220)

# Save tables to CSV
write.csv(table_doy200, "Data/Processed/comments/raw_era5_doy200.csv", row.names = FALSE)
write.csv(summary_doy180_220, "Data/Processed/comments/summary_era5_doy180_220.csv", row.names = FALSE)