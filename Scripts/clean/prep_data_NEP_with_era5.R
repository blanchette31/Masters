#prepare workspace
rm(list = ls())

# load libraries 
library(tidyverse)
library(LakeMetabolizer)

#load data
lake_area <- 63132

model <- read.csv("Data/model/results/temp_all_years.csv")
era5 <- read.csv("Data/Processed/precip/era5_prep.csv")
era5_cut <- read.csv("Data/Processed/precip/era5_cut_buoy.csv")

wind <- era5 %>% 
  select(date, wind_sp.m.s.) %>% 
  rename(datetime = date,
         wnd = wind_sp.m.s.)

k600 <- k.vachon(wind, lake_area, params = c(2.51,1.48,0.39))

# Remove rows before 2015-05-14
k600 <- k600 %>% filter(datetime >= as.Date("2015-05-14"))

# Create a new column 'Avg_Temp_0.5m' with the row-wise average temperature
model$wtr_0.5 <- rowMeans(model[c('wtr_0.0', 'wtr_1.0')])

wtr <- model %>% 
  select(date, wtr_0.5) %>% 
  rename(datetime = date,
         wtr = wtr_0.5)
wtr <- merge(wtr, k600)

# Remove rows where 'wtr' is below 4
wtr <- wtr[wtr$wtr >= 4, ]

wtr_cut <- merge(wtr, era5_cut[,c("date", "buoy_start","buoy_end")], by.x = "datetime", by.y = "date")

wtr_cut <- wtr_cut %>% 
  mutate(doy = strftime(datetime, format = "%j"))

wtr_cut <- wtr_cut %>% 
  rowwise() %>% 
mutate(period = 
         case_when(doy >= buoy_start & doy <= buoy_end ~ "buoy", 
                   TRUE ~ "winter"))
wtr_cut <- wtr_cut %>% 
  rename(k600 = k600,
         )
wtr_final <- wtr_cut %>% 
select(datetime, wtr, k600)


gas_vel <- k600.2.kGAS(wtr, "O2")
write.csv(gas_vel, "Data/Processed/combined/gas_velocity.csv")
