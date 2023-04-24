# prepare workspace
rm(list = ls())

# library 
library(tidyverse)


#data
ws <- read.csv("Data/Processed/carbon/Final_Brandon_fall23.csv")

temp.in = 5.5
sal = 0
pCO2.insitu = ws$pCO2.mean
pCO2.atm = ws$pCO2.head
baro = 97.46



sol.CO2.in = 10^-(-((9345.17/(temp.in + 273.15))-60.2409+23.3585*log((273.15+temp.in)/100)+sal*(0.023517-0.023656*((273.15+temp.in)/100)+0.0047036*((273.15+temp.in)/100)^2))/log(10))
conc.CO2.insitu = pCO2.insitu * sol.CO2.in*1000
conc.CO2.atm = pCO2.atm*sol.CO2.in*1000
delta.CO2 = (conc.CO2.insitu - conc.CO2.atm)*0.001
