#prepare workspace

rm(list = ls())
library(tidyverse)
library(LakeMetabolizer)
library(lubridate)
library(here)

# load data

df <-  read.csv(here("Data/Raw/lake/Croche_CO2_SMasseProj_forBrandon.csv"), header = TRUE)
df_ls <-  read.csv(here("Data/Raw/lake/Croche_CO2_LacSenti_forBrandon.csv"), header = TRUE)

# remove delta column as there is an error in the data
df <- df[, -which(names(df) == "deltaCO2_nM")]


# calculate gas solubility I could calculate using temperature and pressure but since CO2_nM has already been corrected for it i can use a shortcut to determine solubility

conc.CO2.insitu <- df$CO2_nM  #CO2 data is in uM and not nM
pCO2.insitu <- df$pCO2
sol.CO2.in <- conc.CO2.insitu /(1000 * pCO2.insitu)

# calculate 
pCO2.atm <- df$pCO2_air
conc.CO2.atm <- pCO2.atm *1000 * sol.CO2.in
conc.CO2.atm
delta.CO2 <- (conc.CO2.insitu - conc.CO2.atm) / 1000

delta <- df %>% 
  mutate(conc.CO2.atm, delta.CO2) %>% 
  rename( conc.CO2.insitu = CO2_nM)

write.csv(delta, "Data/Processed/lake/delta_co2_SM.csv")

## Repeat with Lacs Sentinels

conc.CO2.insitu <- df_ls$CO2_nM  #CO2 data is in uM and not nM
pCO2.insitu <- df_ls$CO2_ppm
sol.CO2.in <- conc.CO2.insitu /(1000 * pCO2.insitu)

# calculate 
pCO2.atm <- df_ls$CO2_air_ppm
conc.CO2.atm <- pCO2.atm *1000 * sol.CO2.in
conc.CO2.atm
delta.CO2 <- (conc.CO2.insitu - conc.CO2.atm) / 1000


delta_ls <- df_ls %>% 
  mutate(conc.CO2.atm, delta.CO2) %>% 
  rename( conc.CO2.insitu = CO2_nM)

write.csv(delta_ls, "Data/Processed/lake/delta_co2_LS.csv")

delta_ls <- delta_ls[, !(names(delta_ls) %in% c("Lac"))]
delta_ls$date <- delta_ls$Date
delta <- delta[, !names(delta) %in% c("code")]


delta_merged <- merge(delta, delta_ls, by= c("date, conc.CO2.insitu, conc.CO2.atm") all = TRUE)
  