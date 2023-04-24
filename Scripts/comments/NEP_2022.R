# prepare workspace
rm(list = ls())

# libraries
library(tidyverse)
library(LakeMetabolizer)
library(rLakeAnalyzer)

#data
df <- read.csv("Data/Raw/boue/2022CR6_QAQC3.csv")

para <- df %>%
  select(timestamp, AirPressure_hpa_Smp, TChain_Temp_1_Deg_C_Smp_0.65m,TChain_Temp_2_Deg_C_Smp_0.65m, mean_WindSpeed_m_per_s_WVc,OA_0.5corr,TChain_PAR_umol_per_sm2_Smp_0.8m) %>%
  rename(datetime = timestamp)

temp <- df %>%
  select(timestamp, TChain_Temp_1_Deg_C_Smp_0.65m,TChain_Temp_2_Deg_C_Smp_0.65m, TChain_Temp_3_Deg_C_Smp_0.73m, TChain_Temp_4_Deg_C_Smp_1.73m, TChain_Temp_5_Deg_C_Smp_2.73m,TChain_Temp_6_Deg_C_Smp_3.73m, TChain_Temp_7_Deg_C_Smp_4.58m,TChain_Temp_8_Deg_C_Smp_5.58m, TChain_Temp_9_Deg_C_Smp_6.53m,TChain_Temp_10_Deg_C_Smp_7.53m, TChain_Temp_11_Deg_C_Smp_8.53m , TChain_Temp_12_Deg_C_Smp_10.53m)

temp$wtr_0.65 <- rowMeans(temp[c('TChain_Temp_1_Deg_C_Smp_0.65m', 'TChain_Temp_2_Deg_C_Smp_0.65m')])

temp <- temp %>%
  select(-c(TChain_Temp_1_Deg_C_Smp_0.65m,TChain_Temp_2_Deg_C_Smp_0.65m)) %>%
  rename(datetime = timestamp,
         wtr_0.7 = TChain_Temp_3_Deg_C_Smp_0.73m,
         wtr_1.7 = TChain_Temp_4_Deg_C_Smp_1.73m,
         wtr_2.7 = TChain_Temp_5_Deg_C_Smp_2.73m,
         wtr_3.7 = TChain_Temp_6_Deg_C_Smp_3.73m,
         wtr_4.6 = TChain_Temp_7_Deg_C_Smp_4.58m,
         wtr_5.6 = TChain_Temp_8_Deg_C_Smp_5.58m,
         wtr_6.5 = TChain_Temp_9_Deg_C_Smp_6.53m,
         wtr_7.5 = TChain_Temp_10_Deg_C_Smp_7.53m,
         wtr_8.5 = TChain_Temp_11_Deg_C_Smp_8.53m,
         wtr_10.5 = TChain_Temp_12_Deg_C_Smp_10.53m)


wtr <- temp[, c(1,12,2,3,4,5,6,7,8,9,10,11)]

t.d <- ts.thermo.depth(wtr, Smin = 0.1, na.rm = FALSE)

para <- merge(para, t.d, by = "datetime")
para$wtr <- rowMeans(para[c('TChain_Temp_1_Deg_C_Smp_0.65m', 'TChain_Temp_2_Deg_C_Smp_0.65m')])

para <- para %>%
  rename(wind = mean_WindSpeed_m_per_s_WVc) %>%
  select(-c(TChain_Temp_1_Deg_C_Smp_0.65m,TChain_Temp_2_Deg_C_Smp_0.65m))

df$timestamp <- as.POSIXct(df$timestamp)
df$timestamp <- as.POSIXct(df$timestamp, format = "%Y-%m-%d %H:%M:%S")
df <- df %>%
  mutate(year = as.numeric(format(timestamp, "%Y")),
         doy = as.numeric(format(timestamp, "%j")))

wind <- para %>% select(datetime, wind) %>% rename(wnd = wind)
wtr_sat <- para %>% select(datetime, wtr)

k600 <- k.vachon(wind, lake.area = 63132, params = c(2.51, 1.48, 0.39))
para <- merge(para, k600, by = "datetime")

ko2 <- k600.2.kGAS(para, gas = "O2")
para <- merge(para, ko2, by = "datetime")

baro <- para$AirPressure_hpa_Smp
sat <- o2.at.sat(wtr_sat, baro, salinity = rep(0, length(wtr_sat)), model = "garcia-benson")
para <- merge(para, sat, by = "datetime")

fin <- para %>% mutate(do.obs = OA_0.5corr * 32 / 1000)
fin$datetime <- as.POSIXct(fin$datetime)


df_f <- fin %>%
  rename(irr = TChain_PAR_umol_per_sm2_Smp_0.8m, z.mix = thermo.depth)


metab_2022 <- metab(df_f, datetime = "datetime", wtr = "wtr", irr = "irr",
               do.obs = "do.obs", do.sat = "do.sat", k.gas = "k.gas",
               z.mix = "thermo_depth", lake.lat = 46.0)


write.csv(metab_2022, "Data/Processed/comments/metab_2022.csv", row.names = FALSE)


