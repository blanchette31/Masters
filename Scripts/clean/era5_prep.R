#prepare workspace
rm(list = ls())

#libraries
library(tidyverse)

#data 
era5 <- read.table("Data/Processed/precip/input_meteo_era5_CRO_20152022.txt", header = TRUE)
pheno <- read.csv("Data//Processed//phenocam//pheno_clean.csv", header = TRUE)
head(era5)

era5 <- era5[, !grepl("^inflow", names(era5))]

# Replace 'era5' with the actual name of your dataframe
era5$date <- as.Date(paste(era5$year, era5$month, era5$day, sep = "-"))

era5 <- era5 %>% 
  mutate(doy = strftime(date, format = "%j")
  ) %>% 
  rename(rain = precip.mm.day.)


# Merge the summary_df with climatic_data to get the day of year information
merged <- merge(era5, pheno[, c("year","doy_start","buoy_start", "buoy_end")], by = "year")

# Create a new column indicating whether the data is "buoy" or "outside"
merged$category <- ifelse(
  merged$doy >= merged$buoy_start & merged$doy <= merged$buoy_end,
  "buoy",
  "outside"
)

merged <- merged %>% 
  rowwise() %>%
  mutate(period = 
           case_when(doy < doy_start ~ "summer", 
                     doy >= doy_start & doy <= buoy_end ~ "lf", 
                     TRUE ~ "winter"))


# Subset the dataframe to only keep "buoy" data
buoy_data <- subset(merged, category == "buoy")
summer_cut <- subset(buoy_data, period == "summer")
merged_cut <- subset(buoy_data, period == "lf")

write.csv(era5, "Data/Processed/precip/era5_prep.csv")
write.csv(buoy_data, "Data/Processed/precip/era5_cut_buoy.csv")
write.csv(merged_cut, "Data/Processed/precip/era5_cut_lf.csv")
write.csv(summer_cut, "Data/Processed/precip/era5_cut_summer.csv")
