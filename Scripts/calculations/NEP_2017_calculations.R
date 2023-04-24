#prepare workspace
rm(list = ls())

# load libraries 
library(tidyverse)
library(LakeMetabolizer)
library(rLakeAnalyzer)

#load data 
df_2017 <- read.csv("Data/Raw/boue/Croche2016-2021/Crochefinal2017.csv")

para_2017 <- df_2017 %>% 
  select(timestamp, airP_hPa, waterT01_0.5m_oC, waterT02_0.5m_oC, windspeedWVcavg_m_per_s,DOa_0.5m_uM, net_irradiance_W_per_m2, PAR_1m_umol_per_sm2, airP_hPa) %>% 
  rename(datetime = timestamp)

temp <- df_2017 %>% 
  select(timestamp, waterT01_0.5m_oC, waterT02_0.5m_oC, waterT03_1m_oC, waterT04_2m_oC, waterT05_3m_oC, waterT06_4m_oC, waterT07_4.5m_oC, waterT08_5.5m_oC, waterT09_6.5m_oC,waterT10_7.5m_oC, waterT11_8.5m_oC, waterT12_10.5m_oC)


temp$wtr_0.5 <- rowMeans(temp[c('waterT01_0.5m_oC', 'waterT02_0.5m_oC')])
temp <- temp %>% 
  select(-c(waterT01_0.5m_oC, waterT02_0.5m_oC)) %>% 
rename(datetime = timestamp, 
       wtr_1.0 = waterT03_1m_oC,
       wtr_2.0 = waterT04_2m_oC,
       wtr_3.0 = waterT05_3m_oC,
       wtr_4.0 = waterT06_4m_oC,
       wtr_4.5 = waterT07_4.5m_oC,
       wtr_5.5 =  waterT08_5.5m_oC,
       wtr_6.5 = waterT09_6.5m_oC,
       wtr_7.5 = waterT10_7.5m_oC,
       wtr_8.5 = waterT11_8.5m_oC,
       wtr_10.5 = waterT12_10.5m_oC)

wtr <- temp[, c(1,12,2,3,4,5,6,7,8,9,10,11)]

t.d <- ts.thermo.depth(wtr, Smin = 0.1, na.rm = FALSE)

para_17 <- merge(para_2017, t.d, by = "datetime" )


para_17$wtr <- rowMeans(para_17[c('waterT01_0.5m_oC', 'waterT02_0.5m_oC')])

para_17 <- para_17 %>% 
  rename(wind = windspeedWVcavg_m_per_s) %>% 
  select(-c(waterT01_0.5m_oC, waterT02_0.5m_oC))


wind <- para_17 %>% 
  select(datetime, wind) %>% 
  rename(wnd = wind)


wtr_sat <- para_17 %>% 
  select(datetime, wtr)
k600 = k.vachon(wind, lake.area = 63132,params=c(2.51,1.48,0.39) )

para_17 <- merge(para_17, k600, by = "datetime")

ko2 <- k600.2.kGAS(para_17, gas = "O2")

para_17 <- merge(para_17, ko2, by = "datetime")


baro = para_2017$airP_hPa
sat <- o2.at.sat(wtr_sat, baro, salinity = rep(0, length(wtr_sat)),
                 model = "garcia-benson")

para_17 <- merge(para_17, sat, by = "datetime")
fin_17 <- para_17 %>% 
  mutate(do.obs = DOa_0.5m_uM*32/1000)


fin_17$datetime <- as.POSIXct(fin_17$datetime)

df_f17 <- fin_17 %>% 
  rename(irr = PAR_1m_umol_per_sm2,
         z.mix = thermo.depth) %>% 
  select(-c(net_irradiance_W_per_m2))

metab <- metab(df_f17, datetime = "datetime", wtr = "wtr", irr = "irr", do.obs = "do.obs", do.sat = "do.sat", k.gas = "k.gas", z.mix = "thermo_depth", lake.lat = 46.0)



ggplot(metab, aes(x = doy, y = NEP), color = "black")+
  geom_line()+
  geom_line(aes(y = GPP), color = "green")+
  geom_line(aes(y= R), color = "red")+
  theme_classic()+
  geom_hline(yintercept = 0, lty = 2)
summary(metab)

