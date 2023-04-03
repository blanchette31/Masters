#Load packages 
library(readxl)
library(dplyr)
library(plyr)

#Load data 
df = list.files(path = "Data//Raw//precip//SBL//head//", pattern = "*.xlsx", full.names = TRUE) %>% 
lapply(read_excel) %>%
  rbind.fill(df)
df_clean = df %>% 
  select(année, jour, heure, Vent, Vent_, Temp_, HumRel, P_kPa, "(Photosyn. Active Radiation)", 
         UVB, 'Temp garage (temp of probe at garage level)', Pluitmp, Temp_sol)
df_clean =  df_clean %>% 
  dplyr::rename(
    "year" = "année",
    "doy" = "jour",
    "time" = "heure",
    'wind_spd' = "Vent",
    'wind_dir' = "Vent_",
    "temperature" = "Temp_",
    "relhum" = "HumRel",
    "pres" = "P_kPa",
   "PAR" = "(Photosyn. Active Radiation)",
    "temp_garage" = 'Temp garage (temp of probe at garage level)',
    "precip" = "Pluitmp",
    "temp_ground" = "Temp_sol")

df_clean = df_clean[-c(1),]



write.csv(df_clean, "Data//Processed//precip//precip_sbl_clean.csv", row.names = F)

df = read.csv("Data//Processed//precip//precip_sbl_clean.csv", header = T, stringsAsFactors = F)

new_names = paste0(as.character(df[1,], as.character(df[2,])))

names(df) = new_names
df_header_fixed = df[3:nrow(df),]
 
write.csv(df_header_fixed,"Data//Processed//precip//precip_sbl_processed.csv")

precip_daily = df_clean %>% group_by(doy) %>%
  summarise(daily_precip = sum(precip))
df_brief = df_header_fixed %>%
  select('# Station', ...2, jour, ...4, Vent, Vent_, Temp_, HumRel, P_kPa, (Photosyn. Active Radiation), UVB,
         UVtemp_K, ...13, Pluitmp,"1,", "Il est important de noter que les prÃ©cipitation (mm) enregistrÃ©es dans ce fichier ne sont que les prÃ©cipitations liquide (pluie)",
         ...3, ...5, ...6, ...7, Temp_Sol)
ddf_new_colnames = df_brief %>%
  rename(
    station = "# Station"
  )


df_brief
