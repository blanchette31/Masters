# Clear previous data
rm(list = ls())

# Load libraries
library(tidyverse)
library(LakeMetabolizer)
library(rLakeAnalyzer)

# List of years
years <- c(2016, 2017, 2018, 2019, 2021)


for (yr in years) {
  
  # Load data
  df <- read.csv(paste0("Data/Raw/boue/Croche2016-2021/Crochefinal", yr, ".csv"))
  assign(paste0("df_", yr), df)
  
  para <- df %>%
    select(timestamp, airP_hPa, waterT01_0.5m_oC, waterT02_0.5m_oC, windspeedWVcavg_m_per_s,
           DOa_0.5m_uM, net_irradiance_W_per_m2, PAR_1m_umol_per_sm2, airP_hPa) %>%
    rename(datetime = timestamp)
  assign(paste0("para_", yr), para)
  
  temp <- df %>%
    select(timestamp, waterT01_0.5m_oC, waterT02_0.5m_oC, waterT03_1m_oC, waterT04_2m_oC,
           waterT05_3m_oC, waterT06_4m_oC, waterT07_4.5m_oC, waterT08_5.5m_oC,
           waterT09_6.5m_oC, waterT10_7.5m_oC, waterT11_8.5m_oC, waterT12_10.5m_oC)
  
  temp$wtr_0.5 <- rowMeans(temp[c('waterT01_0.5m_oC', 'waterT02_0.5m_oC')])
  
  temp <- temp %>%
    select(-c(waterT01_0.5m_oC, waterT02_0.5m_oC)) %>%
    rename(datetime = timestamp,
           wtr_1.0 = waterT03_1m_oC,
           wtr_2.0 = waterT04_2m_oC,
           wtr_3.0 = waterT05_3m_oC,
           wtr_4.0 = waterT06_4m_oC,
           wtr_4.5 = waterT07_4.5m_oC,
           wtr_5.5 = waterT08_5.5m_oC,
           wtr_6.5 = waterT09_6.5m_oC,
           wtr_7.5 = waterT10_7.5m_oC,
           wtr_8.5 = waterT11_8.5m_oC,
           wtr_10.5 = waterT12_10.5m_oC)
  assign(paste0("temp_", yr), temp)
  
  wtr <- temp[, c(1,12,2,3,4,5,6,7,8,9,10,11)]
  assign(paste0("wtr_", yr), wtr)
  
  t.d <- ts.thermo.depth(wtr, Smin = 0.1, na.rm = FALSE)
  
  para <- merge(para, t.d, by = "datetime")
  para$wtr <- rowMeans(para[c('waterT01_0.5m_oC', 'waterT02_0.5m_oC')])
  
  para <- para %>%
    rename(wind = windspeedWVcavg_m_per_s) %>%
    select(-c(waterT01_0.5m_oC, waterT02_0.5m_oC))
  
  wind <- para %>% select(datetime, wind) %>% rename(wnd = wind)
  wtr_sat <- para %>% select(datetime, wtr)
  
  k600 <- k.vachon(wind, lake.area = 63132, params = c(2.51, 1.48, 0.39))
  para <- merge(para, k600, by = "datetime")
  
  ko2 <- k600.2.kGAS(para, gas = "O2")
  para <- merge(para, ko2, by = "datetime")
  
  baro <- para$airP_hPa
  sat <- o2.at.sat(wtr_sat, baro, salinity = rep(0, length(wtr_sat)), model = "garcia-benson")
  para <- merge(para, sat, by = "datetime")
  
  fin <- para %>% mutate(do.obs = DOa_0.5m_uM * 32 / 1000)
  fin$datetime <- as.POSIXct(fin$datetime)
  assign(paste0("fin_", yr), fin)
  
  df_f <- fin %>%
    rename(irr = PAR_1m_umol_per_sm2, z.mix = thermo.depth) %>%
    select(-c(net_irradiance_W_per_m2))
  assign(paste0("df_f_", yr), df_f)
  
  metab <- metab(df_f, datetime = "datetime", wtr = "wtr", irr = "irr",
                 do.obs = "do.obs", do.sat = "do.sat", k.gas = "k.gas",
                 z.mix = "thermo_depth", lake.lat = 46.0)
  assign(paste0("metab_", yr), metab)

  # write metab (assumed a dataframe) to CSV named metab_YEAR.csv
  write.csv(metab, file = paste0("Data/Processed/comments/metab_", yr, ".csv"), row.names = FALSE)
  
}

