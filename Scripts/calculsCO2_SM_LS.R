#prepare workspace

rm(list = ls())
library(tidyverse)
library(LakeMetabolizer)
library(lubridate)
library(here)

# load data

df <-  read.csv(here("Data/Raw/lake/Croche_CO2_SMasseProj_forBrandon.csv"), header = TRUE)

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
