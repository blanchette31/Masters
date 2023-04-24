# prepare workspace
rm(list = ls())

# load libraries 
library(tidyverse)

# load data

era5 <- read.csv("Data/Processed/precip/era5_cut_lf.csv")

rain <- era5 %>% 
  select(date, year, doy, rain)

rain$area_wslake <- 1075472+179000
rain$vol_lake <- 836000
rain$delta <- 1849.18
rain$vol_b1 <- 377064

calcs <- rain %>% 
  mutate(
    vol_watershed = area_wslake*rain/1000,
    mass_watershed_all_lake = area_wslake*rain/1000*delta,
    mass_watershed_b1 = (area_wslake*rain/1000)*vol_b1/vol_lake
  )
calcs <- calcs %>% 
  mutate(
    perc_all_lake = vol_watershed/vol_lake*100,
    perc_b1 =vol_watershed/vol_b1*100
  )

write.csv(calcs, "Data/Processed/combined/mass_co2_watershed.csv")
