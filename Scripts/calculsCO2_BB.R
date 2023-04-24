#prepare workspace
rm(list = ls())
library(tidyverse)
library(LakeMetabolizer)
library(lubridate)


#load data


df <- read.csv("Data//Processed//boue//boue_2016-2021.csv", header = TRUE)
df_2015 <- read.csv("Data//Processed//boue//boue_2015.csv", header =TRUE)
df_2022 <- read.csv("Data//Processed//boue//boue_2022.csv", header =TRUE)
df_model <- read.csv("Data//model//results//temp_all_years.csv", header = TRUE)
df_model$wtr_0.5 <- rowMeans(df_model[,c("wtr_0.0", "wtr_1.0")])

df_model <- df_model %>% 
  mutate(year = strftime(date, "%Y"),
         doy = strftime(date, "%j"))



model_2015 <- subset(df_model, year == 2015)
df_2015 <- df_2015 %>% 
  mutate(date = strftime(timestamp, "%Y-%m-%d"))
df_2015$date <- as.Date(df_2015$date)

df_2015 <- df_2015 %>% 
  group_by(date) %>% 
  summarise_at(
    vars(airP_hPa, CO2_0.5m_ppm, co2_atm, DOa_0.5m_uM, DOb_4.7m_uM, DOc_8.7m_uM),
    list(mean = ~mean(., na.rm = TRUE))
  )
# Remove "_mean" suffix from column names
colnames(df_2015) <- gsub("_mean", "", colnames(df_2015))
model_2015 <- merge(model_2015, df_2015[,c("date", "airP_hPa","CO2_0.5m_ppm","co2_atm","DOa_0.5m_uM","DOb_4.7m_uM","DOc_8.7m_uM")], by = "date", all = TRUE)





# define variables with df columns 

# temp.in = df_2015$waterT01_1.7m_oC
temp.in = model_2015$wtr_0.5
sal = 0
pCO2.insitu = model_2015$CO2_0.5m_ppm
pCO2.atm = model_2015$co2_atm
baro = model_2015$airP_hPa


temp_2015 = model_2015 %>%
  select(date, wtr_0.5)
colnames(temp_2015)[1:2] = c("datetime", "wtr")

sat_2015 = o2.at.sat(temp_2015, baro, salinity = rep(0, length(temp_2015)), model = "garcia-benson")


temp_meta_2015 = model_2015 %>%
  select(date) %>% 
  mutate(
    wtr = rowMeans(model_2015[, c("wtr_4.0", "wtr_5.0")])
  )
colnames(temp_meta_2015)[1:2] = c("datetime", "wtr")

sat_meta_2015 = o2.at.sat(temp_meta_2015, baro, salinity = rep(0, length(temp_meta_2015)), model = "garcia-benson")


temp_hypo_2015 = model_2015 %>%
  select(date) %>% 
  mutate(
    wtr = rowMeans(model_2015[, c("wtr_8.0", "wtr_9.0")])
  )
colnames(temp_hypo_2015)[1:2] = c("datetime", "wtr")


sat_hypo_2015 = o2.at.sat(temp_hypo_2015, baro, salinity = rep(0, length(temp_hypo_2015)), model = "garcia-benson")


# Add new columns to dataframe (debugging)
delta_2015 = model_2015 %>%
  mutate(temp.in, pCO2.insitu, pCO2.atm)


#
delta_2015 = delta_2015 %>%
  #group_by(year, timestamp, date, doy) %>%
  mutate(
    sol.CO2.in = 10^-(-((9345.17/(temp.in + 273.15))-60.2409+23.3585*log((273.15+temp.in)/100)+sal*(0.023517-0.023656*((273.15+temp.in)/100)+0.0047036*((273.15+temp.in)/100)^2))/log(10)),
    conc.CO2.insitu = pCO2.insitu * sol.CO2.in*1000,
    conc.CO2.atm = pCO2.atm*sol.CO2.in*1000,
  delta.CO2 = (conc.CO2.insitu - conc.CO2.atm)*0.001)





# delta_2015 = cbind(delta_2015, temp.in)
delta_2015[, "timestamp"] = model_2015[, "date"]
delta_2015[, "do_umol"] = model_2015[, "DOa_0.5m_uM"]
delta_2015[, "do_sat"] = sat_2015[, "do.sat"]
delta_2015[, "do_meta"] = model_2015[, "DOb_4.7m_uM"]
delta_2015[,"sat_meta"] = sat_meta_2015[,"do.sat"]
delta_2015[, "do_hypo"] = model_2015[,"DOc_8.7m_uM"]
delta_2015[,"sat_hypo"] = sat_hypo_2015[,"do.sat"]


start_date <- "2015-05-14"
end_date <- "2015-11-05"

delta_2015 <- delta_2015 %>% 
  filter(date >= start_date & date <= end_date)

delta_2015 <- delta_2015[, (ncol(delta_2015) - 13):ncol(delta_2015)]
delta_2015 <- delta_2015 %>% 
  select(-pCO2.insitu, -pCO2.atm)

colnames(delta_2015)[colnames(delta_2015) == "timestamp"] <- "date"

delta_2015$date <- as.Date(delta_2015)

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

temp_meta = df %>%
  select(timestamp,waterT08_5.5m_oC) 
 
colnames(temp_meta)[1:2] = c("datetime", "wtr")

sat_meta = o2.at.sat(temp_meta, baro, salinity = rep(0, length(temp_meta)), model = "garcia-benson")

temp_hypo = df %>%
  select(timestamp, waterT11_8.5m_oC) 
 
colnames(temp_hypo)[1:2] = c("datetime", "wtr")

sat_hypo = o2.at.sat(temp_hypo, baro, salinity = rep(0, length(temp_hypo)), model = "garcia-benson")

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
delta[, "do_meta"] = df[, "DOb_5.5m_uM"]
delta[,"sat_meta"] = sat_meta[,"do.sat"]
delta[, "do_hypo"] = df[,"DOc_8.5m_uM"]
delta[,"sat_hypo"] = sat_hypo[,"do.sat"]

#repeat for 2022 ##

# define variables with df columns 
temp.in = df_2022$TChain_Temp_1_Deg_C_Smp_0.65m
sal = 0
pCO2.insitu = df_2022$Co2_ppm_Smp
pCO2.atm = df_2022$co2_atm
baro = df_2022$AirPressure_hpa_Smp


temp_2022 = df_2022 %>%
  select(timestamp, TChain_Temp_1_Deg_C_Smp_0.65m)
colnames(temp_2022)[1:2] = c("datetime", "wtr")

sat_2022 = o2.at.sat(temp_2022, baro, salinity = 0, model = "garcia-benson")


temp_meta_2022 = df_2022 %>%
  select(timestamp, TChain_Temp_8_Deg_C_Smp_5.58m)

colnames(temp_meta_2022)[1:2] = c("datetime", "wtr")

sat_meta_2022 = o2.at.sat(temp_meta_2022, baro, salinity = rep(0, length(temp_2022)), model = "garcia-benson")

temp_hypo_2022 = df_2022 %>%
  select(timestamp, TChain_Temp_11_Deg_C_Smp_8.53m)

colnames(temp_hypo_2022)[1:2] = c("datetime", "wtr")

sat_hypo_2022 = o2.at.sat(temp_hypo_2022, baro, salinity = rep(0, length(temp_hypo_2022)), model = "garcia-benson")

# Add new columns to dataframe (debugging)
delta_2022 = df_2022 %>%
  mutate(temp.in, pCO2.insitu, pCO2.atm)
#
delta_2022 = delta_2022 %>%
  #group_by(year, timestamp, date, doy) %>%
  summarise(
    sol.CO2.in = 10^-(-((9345.17/(temp.in + 273.15))-60.2409+23.3585*log((273.15+temp.in)/100)+sal*(0.023517-0.023656*((273.15+temp.in)/100)+0.0047036*((273.15+temp.in)/100)^2))/log(10)),
    conc.CO2.insitu = pCO2.insitu * sol.CO2.in*1000,
    conc.CO2.atm = pCO2.atm*sol.CO2.in*1000,
    delta.CO2 = (conc.CO2.insitu - conc.CO2.atm)*0.001)
delta_2022 = cbind(delta_2022, temp.in)
delta_2022[, "timestamp"] = df_2022[, "timestamp"]
delta_2022[, "do_umol"] = df_2022[, "TChain_O2_1_umol_per_L_Smp_0.5m"]
delta_2022[, "do_sat"] = sat_2022[, "do.sat"]
delta_2022[, "do_meta"] = df_2022[, "TChain_O2_2_umol_per_L_Smp_5.48m"]
delta_2022[,"sat_meta"] = sat_meta_2022[,"do.sat"]
delta_2022[, "do_hypo"] = df_2022[,"TChain_O2_3_umol_per_L_Smp_8.43m"]
delta_2022[,"sat_hypo"] = sat_hypo_2022[,"do.sat"]

## merge dataframes


delta_1622 <- rbind(delta, delta_2022)

delta_1622 <- delta_1622 %>% 
  mutate(date = strftime(timestamp, format = "%Y-%m-%d", tz = "EST"))
delta_1622$date <- as.Date(delta_1622$date)

delta_1622_day <- delta_1622 %>% 
  group_by(date) %>% 
  summarise(sol.CO2.in = mean(sol.CO2.in, na.rm = TRUE),
            conc.CO2.insitu = mean(conc.CO2.insitu, na.rm = TRUE),
            conc.CO2.atm = mean(conc.CO2.atm, na.rm = TRUE),
            delta.CO2 = mean(delta.CO2, na.rm = TRUE), 
            temp.in = mean(temp.in, na.rm = TRUE),
            do_umol = mean(do_umol, na.rm = TRUE),
            do_sat = mean(do_sat, na.rm = TRUE),
            do_meta = mean(do_meta, na.rm = TRUE),
            sat_meta = mean(sat_meta, na.rm =TRUE),
            do_hypo = mean(do_hypo, na.rm = TRUE),
            sat_hypo = mean(sat_hypo, na.rm = TRUE))

final_delta <- rbind(delta_2015, delta_1622_day)

do_mg_l = final_delta %>%
  summarise(do_mg_l = do_umol * 32/1000)
do_mg_l_meta = final_delta %>%
  summarise(do_mg_l_meta = do_meta * 32/1000)
do_mg_l_hypo = final_delta %>%
  summarise(do_mg_l_hypo = do_hypo * 32/1000)

final_delta = cbind(final_delta, do_mg_l, do_mg_l_meta, do_mg_l_hypo)

do_sat_perc = final_delta %>%
  summarise(do_sat_perc = do_mg_l/do_sat*100)

do_deviation = final_delta %>%
  summarise(do_deviation = (do_mg_l - do_sat)*1000/32)

do_sat_perc_meta = final_delta %>%
  summarise(do_sat_perc_meta = do_mg_l_meta/sat_meta*100)

do_deviation_meta = final_delta %>%
  summarise(do_deviation_meta = (do_mg_l_meta - sat_meta)*1000/32)

do_sat_perc_hypo = final_delta %>%
  summarise(do_sat_perc_hypo = do_mg_l_hypo/sat_hypo*100)

do_deviation_hypo = final_delta %>%
  summarise(do_deviation_hypo = (do_mg_l_hypo - sat_hypo)*1000/32)

final_delta = cbind(final_delta, do_sat_perc, do_deviation,do_sat_perc_meta,do_deviation_meta,do_sat_perc_hypo,do_deviation_hypo)

# final_delta$timestamp <- as.POSIXct(final_delta$timestamp, format = "%Y-%m-%d%H:%M:%S", tz = "EST")
final_delta = final_delta %>%
  mutate(
    doy = strftime(date, format = "%j"),
    year = strftime(date, format = "%Y"))

write.csv(final_delta, "Data//Processed//boue//delta_co2-2_model_2015.csv", row.names = TRUE) #change path as needed



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

