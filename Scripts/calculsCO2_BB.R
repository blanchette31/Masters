#prepare workspace
rm(list = ls())
library(tidyverse)
library(LakeMetabolizer)
library(lubridate)


#load data


df = read.csv("Data//Processed//boue//boue_cut.csv", header = TRUE) #change path as needed

#subset 2015 for calculation 
df_2015 = df[df$year == 2015, ]
df  = subset(df, year != 2015)



# define variables with df columns 
temp.in = df_2015$waterT01_1.7m_oC
sal = 0
pCO2.insitu = df_2015$CO2_0.5m_ppm
pCO2.atm = df_2015$co2_atm
baro = df_2015$airP_hPa

temp_2015 = df_2015 %>%
  select(timestamp, waterT01_1.7m_oC)
colnames(temp_2015)[1:2] = c("datetime", "wtr")

sat_2015 = o2.at.sat(temp_2015, baro, salinity = rep(0, length(temp_2015)), model = "garcia-benson")

# Add new columns to dataframe (debugging)
delta_2015 = df_2015 %>%
  mutate(temp.in, pCO2.insitu, pCO2.atm)



#
delta_2015 = delta_2015 %>%
  #group_by(year, timestamp, date, doy) %>%
  summarise(
    sol.CO2.in = 10^-(-((9345.17/(temp.in + 273.15))-60.2409+23.3585*log((273.15+temp.in)/100)+sal*(0.023517-0.023656*((273.15+temp.in)/100)+0.0047036*((273.15+temp.in)/100)^2))/log(10)),
    conc.CO2.insitu = pCO2.insitu * sol.CO2.in*1000,
    conc.CO2.atm = pCO2.atm*sol.CO2.in*1000,
  delta.CO2 = (conc.CO2.insitu - conc.CO2.atm)*0.001)

delta_2015 = cbind(delta_2015, temp.in)
delta_2015[, "timestamp"] = df_2015[, "timestamp"]
delta_2015[, "do_umol"] = df_2015[, "DOa_0.5m_uM"]
delta_2015[, "do_sat"] = sat_2015[, "do.sat"]





###### Repeat operation for other years ########



# define variables with df columns 
temp.in = df$waterT01_0.5m_oC
sal = 0
pCO2.insitu = df$CO2_0.5m_ppm
pCO2.atm = df$co2_atm
baro = df$airP_hPa


temp = df %>%
  select(timestamp, waterT01_0.5m_oC)
colnames(temp)[1:2] = c("datetime", "wtr")

sat = o2.at.sat(temp, baro, salinity = 0, model = "garcia-benson")

# Add new columns to dataframe (debugging)
delta = df %>%
  mutate(temp.in, pCO2.insitu, pCO2.atm)
#
delta = delta %>%
  #group_by(year, timestamp, date, doy) %>%
  summarise(
    sol.CO2.in = 10^-(-((9345.17/(temp.in + 273.15))-60.2409+23.3585*log((273.15+temp.in)/100)+sal*(0.023517-0.023656*((273.15+temp.in)/100)+0.0047036*((273.15+temp.in)/100)^2))/log(10)),
    conc.CO2.insitu = pCO2.insitu * sol.CO2.in*1000,
    conc.CO2.atm = pCO2.atm*sol.CO2.in*1000,
    delta.CO2 = (conc.CO2.insitu - conc.CO2.atm)*0.001)
delta = cbind(delta, temp.in)
delta[, "timestamp"] = df[, "timestamp"]
delta[, "do_umol"] = df[, "DOa_0.5m_uM"]
delta[, "do_sat"] = sat[, "do.sat"]

## merge dataframes


final_delta = rbind(delta_2015,delta)

do_mg_l = final_delta %>%
  summarise(do_mg_l = do_umol * 32/1000)

final_delta = cbind(final_delta, do_mg_l)

do_sat_perc = final_delta %>%
  summarise(do_sat_perc = do_mg_l/do_sat*100)

do_deviation = final_delta %>%
  summarise(do_deviation = (do_mg_l - do_sat)*1000/32)

final_delta = cbind(final_delta, do_sat_perc, do_deviation)

final_delta$timestamp <- as.POSIXct(final_delta$timestamp, format = "%Y-%m-%d%H:%M:%S", tz = "America/New_York")
final_delta = final_delta %>%
  mutate(
    date = as.Date(timestamp, format = "%Y%m%d"),
    doy = strftime(timestamp, format = "%j"),
    year = strftime(timestamp, format = "%Y"))

write.csv(final_delta, "Data//Processed//boue//delta_co2.csv", row.names = TRUE) #change path as needed



### calculs pour delta CO2

#concentration CO2 in situ (nmol/L)
conc.CO2.insitu = pCO2.insitu * sol.CO2.in*1000

# concentration CO2 atmosphérique (nmol/L)
conc.CO2.atm = pCO2.atm*sol.CO2.in*1000

# delta CO2 (en nmol/L, x0.001 pour avoir des umol/L)
delta.CO2 = conc.CO2.insitu - conc.CO2.atm

# solubilité du CO2 in situ
sol.CO2.in = 10^-(-((9345.17/(temp.in + 273.15))-60.2409+23.3585*log((273.15+temp.in)/100)+sal*(0.023517-0.023656*((273.15+temp.in)/100)+0.0047036*((273.15+temp.in)/100)^2))/log(10))
  
  
#variables nécessaires
# pCO2.insitu (eau)
# pCO2.atm
# temp.in = température de l'eau 
# Find the index of the row with the maximum value in the "Value" column
max_index <- which.max(final_delta$delta.CO2)

# Extract the row with the maximum value
max_row <- final_delta[max_index, ]

# Print the row with the maximum value
print(max_row)


max_sol = which.max(final_delta$sol.CO2.in)

max_row_sol = final_delta[max_sol, ]

print(max_row_sol)

min_sol = which.min(final_delta$sol.CO2.in)

min_row_sol = final_delta[min_sol, ]

print(min_row_sol)

