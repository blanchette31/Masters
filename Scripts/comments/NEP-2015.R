# prepare workspace
rm(list = ls())

# libraries
library(tidyverse)
library(LakeMetabolizer)
library(rLakeAnalyzer)

#data
df <- read.csv("Data/Raw/boue/Crochefinal2015.csv")
temp_0.5 <- read.csv("Data/Processed/boue/delta_co2-2_model_2015.csv")
temp <- read.csv("Data/model/results/temp_all_years.csv")
head(df)

temp_0.5$date <- as.Date(temp_0.5$date)
temp_0.5$date <- as.Date(temp_0.5$date, format = "%d/%m/%Y")
temp_0.5_2015 <- temp_0.5 %>% filter(format(date, "%Y") == "2015")

df$timestamp <- as.POSIXct(df$timestamp)
df$timestamp <- as.POSIXct(df$timestamp, format = "%Y-%m-%d %H:%M:%S")
df <- df %>%
  mutate(year = as.numeric(format(timestamp, "%Y")),
         doy = as.numeric(format(timestamp, "%j")))

df <- df %>%
  left_join(temp_0.5_2015 %>% mutate(doy = yday(date)) %>% select(year, doy, temp.in),
            by = c("year", "doy"))

temp$date <- as.Date(temp$date)
temp <- temp %>% filter(format(date, "%Y") == "2015")

temp <- temp %>% rename(datetime = date) %>% select(-c(X))


para <- df %>%
  select(timestamp, airP_hPa, windspeedavg_m_per_s,temp.in,
         DOa_0.5m_uM, net_irradiance_W_per_m2, PAR_1.4m_umol_per_sm2, airP_hPa) %>%
  rename(datetime = timestamp)
para$date <- as.Date(para$datetime)


t.d <- ts.thermo.depth(temp, Smin = 0.1, na.rm = FALSE)
t.d <- t.d   %>% mutate(date = as.Date(datetime))
t.d <- t.d %>% select(-datetime)


wtr <- temp_0.5_2015$temp.in



# assume workspace has objects from your last block:
# para (with datetime, temp.in, DOa_0.5m_uM, PAR_1.4m_umol_per_sm2, airP_hPa, wind renamed), 
# t.d (thermo depth from ts.thermo.depth), and temp_0.5_2015 (daily temp.in used earlier)

library(lubridate)

# Ensure datetime types are consistent
# If t.d already has a 'date' column
para <- left_join(para,
                  t.d,
                  by = "date")



# Use temp.in (already joined into df earlier) as bulk surface water temperature (wtr)
# If para$temp.in is NA for some rows, optionally propagate last observation carried forward:
# para$temp.in <- zoo::na.locf(para$temp.in, na.rm = FALSE)
para <- para %>% mutate(wtr = temp.in)

# Rename wind column (you already did but ensure it exists)
para <- para %>% rename(wind = windspeedavg_m_per_s) 

# Prepare inputs for gas/oxygen computations
wind_df <- para %>% select(datetime, wind) %>% rename(wnd = wind)
wtr_sat  <- para %>% select(datetime, wtr)

# Compute k600 (Vachon) and merge
k600 <- k.vachon(wind_df, lake.area = 63132, params = c(2.51, 1.48, 0.39))
k600$datetime <- as.POSIXct(k600$datetime)
para <- merge(para, k600, by = "datetime", all.x = TRUE)

# Convert k600 to gas-specific k (O2) and merge
ko2 <- k600.2.kGAS(para, gas = "O2")
ko2$datetime <- as.POSIXct(ko2$datetime)
para <- merge(para, ko2, by = "datetime", all.x = TRUE)

# Compute O2 saturation (do.sat) and merge
baro <- para$airP_hPa
sat <- o2.at.sat(wtr_sat, baro, salinity = rep(0, nrow(wtr_sat)), model = "garcia-benson")
sat$datetime <- as.POSIXct(sat$datetime)
para <- merge(para, sat, by = "datetime", all.x = TRUE)

# Compute observed DO in the same units you used before
# original: DOa_0.5m_uM * 32 / 1000  (g O2 / m3)
para <- para %>% mutate(do.obs = DOa_0.5m_uM * 32 / 1000)

# Ensure column names for metab inputs
# rename PAR column to irr and thermo.depth to z.mix; ensure k.gas exists
para <- para %>%
  rename(irr = PAR_1.4m_umol_per_sm2,
         z.mix = thermo.depth) 

# If k600.2.kGAS produced a different k column name, standardize to k.gas
if(!"k.gas" %in% names(para) && "k600" %in% names(para)) {
  # attempt to convert existing k column to k.gas; inspect names and choose appropriate one
  possible_k <- names(para)[grepl("^k(600|GAS|gas|600.*)$", names(para), ignore.case = TRUE)]
  if(length(possible_k) > 0) para <- para %>% rename(k.gas = all_of(possible_k[1]))
}

# Build df_f for metab, keep only required columns in expected types
df_f <- para %>%
  select(datetime, wtr, irr, do.obs, do.sat, k.gas, z.mix) %>%
  mutate(datetime = as.POSIXct(datetime))

# Quick checks (uncomment to inspect)
# str(df_f); summary(df_f$wtr); sum(is.na(df_f$k.gas)); sum(is.na(df_f$do.sat))

# required columns for metab
req_cols <- c("wtr", "irr", "do.obs", "do.sat", "k.gas", "z.mix", "datetime")

# 1. ensure datetime is POSIXct
df_f$datetime <- as.POSIXct(df_f$datetime)




# Run metabolism
metab_results <- metab(df_f,
                       datetime = "datetime",
                       wtr = "wtr",
                       irr = "irr",
                       do.obs = "do.obs",
                       do.sat = "do.sat",
                       k.gas = "k.gas",
                       z.mix = "z.mix",
                       lake.lat = 46.0)

# Save outputs to objects for inspection
fin_2015   <- para
df_f_2015  <- df_f
metab_2015 <- metab_results

write.csv(metab_2015, "Data/Processed/comments/metab_2015.csv", row.names = F)

# Optionally inspect outputs
# head(fin_2015); head(df_f_2015); metab_2015
