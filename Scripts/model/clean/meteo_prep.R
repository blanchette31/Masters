## Prepare workspace ##
rm(list = ls())


# load libraries
library(ggplot2)
library(tidyverse)
library(tidyr)
library(dplyr)
library(data.table)
library(readr)
library(lubridate)

#load data
df_2015 <- read.csv("Data//Raw//boue//Crochefinal2015.csv")
df_2016 <- read.csv("Data//Raw//boue//Croche2016-2021//Crochefinal2016.csv")
df_2017 <- read.csv("Data//Raw//boue//Croche2016-2021//Crochefinal2017.csv")
df_2018 <- read.csv("Data//Raw//boue//Croche2016-2021//Crochefinal2018.csv")
df_2019 <- read.csv("Data//Raw//boue//Croche2016-2021//Crochefinal2019.csv")
df_2021 <- read.csv("Data//Raw//boue//Croche2016-2021//Crochefinal2021.csv")
df_2022 <- read.csv("Data//Raw//boue//2022CR6_QAQC3.csv")

df_2022 <- df_2022 %>% 
  rename(windspeedavg_m_per_s = WindSpd_Avg_m_per_s_Avg,
         airP_hPa = AirPressure_hpa_Smp,
         RH_perc = RelHumidity_percent_Smp,
         airT_oC = AirTemp_Deg_C_Smp)


selected_columns <- c("timestamp", "airT_oC", "airP_hPa", "windspeedavg_m_per_s", "RH_perc")

merged_df <- rbind(
  df_2015[, selected_columns],
  df_2016[, selected_columns],
  df_2017[, selected_columns],
  df_2018[, selected_columns],
  df_2019[, selected_columns],
  df_2021[, selected_columns],
  df_2022[, selected_columns])

#Set timestamp to correct format (still not appearing in EST though)
merged_df$timestamp <- as.POSIXct(merged_df$timestamp, format = "%Y-%m-%d%H:%M:%S", tz = "America/New_York")

merged_df$timestamp_est <- with_tz(merged_df$timestamp, tzone = "EST")

merged_df$date <- as.Date(merged_df$timestamp_est, format = "%Y%m%d")

# Extract the year
merged_df$year <- year(merged_df$timestamp_est)

# Extract the month
merged_df$month <- month(merged_df$timestamp_est)

# Extract the day
merged_df$day <- day(merged_df$timestamp_est)

merged_df <- na.omit(merged_df)

df_daily <- merged_df %>% 
  group_by(date, year) %>% 
  summarise( airT_oC = mean(airT_oC),
             airP_hPa = mean(airP_hPa),
             wind = mean(windspeedavg_m_per_s),
             relhum = mean(RH_perc))

nasad <- read.csv("Data//Processed//precip//nasa_daily.csv")

df_daily = merge(df_daily[,], nasad[, c("date", "rain")],
           by = "date")

era5 <- read.table("Data//model//inputs//meteo//era5//input_meteo_era5_CRO_20152022.txt", header = TRUE, skip = 1) # skip = 1 to skip the first line of the table 

era5$date <- as.Date(paste(era5$year, era5$month, era5$day, sep = "-"))

sth <- read.csv("Data//Processed//precip//precip_2011-2023.csv")


era_sth <- merge(era5[, c("date", "year", "precip.mm.day.")], sth[, c("date", "rain")], by = "date")


# Example actual and predicted values
actual <- era_sth$rain
predicted <- era_sth$precip.mm.day.

# Calculate residuals
residuals <- actual - predicted

# Calculate squared errors
squared_errors <- residuals^2

# Calculate mean squared error
mse <- mean(squared_errors)

# Calculate RMSE
rmse <- sqrt(mse)

# Print the RMSE
print(rmse)

#Plot rain ST-Hippolyte vs ERA5
ggplot(era_sth, aes(x = rain, y = precip.mm.day., color = factor(year))) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(x = "Saint-Hippolyte (meteo.gc.ca)", y = "ERA5") +
  ggtitle("Precipitation comparison Saint-Hippolyte vs ERA5") +
  geom_text(x = 50, y = 45, label = paste("RMSE:", round(rmse, 3)), hjust = 0, vjust = 1, color = "black") +
  theme_minimal()







era_nasa <- merge(era5[, c("date", "year", "precip.mm.day.")], nasad[, c("date", "rain")], by = "date")

# Example actual and predicted values
actual <- era_nasa$rain
predicted <- era_nasa$precip.mm.day.

# Calculate residuals
residuals <- actual - predicted

# Calculate squared errors
squared_errors <- residuals^2

# Calculate mean squared error
mse <- mean(squared_errors)

# Calculate RMSE
rmse <- sqrt(mse)

# Print the RMSE
print(rmse)

#Plot rain ST-Hippolyte vs ERA5
ggplot(era_nasa, aes(x = rain, y = precip.mm.day., color = factor(year))) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(x = "NASA Data Rods", y = "ERA5") +
  ggtitle("Precipitation comparison NASA Data Rods vs ERA5") +
  geom_text(x = 25, y = 40, label = paste("RMSE:", round(rmse, 3)), hjust = 0, vjust = 1, color = "black") +
  theme_minimal()





sth_nasa <- merge(sth[, c("date", "Year", "rain")], nasad[, c("date", "rain")], by = "date")

sth_nasa <- sth_nasa[sth_nasa$Year >= 2015, ]

# Example actual and predicted values
actual <- sth_nasa$rain.x
predicted <- sth_nasa$rain.y

# Calculate residuals
residuals <- actual - predicted

# Calculate squared errors
squared_errors <- residuals^2

# Calculate mean squared error
mse <- mean(squared_errors)

# Calculate RMSE
rmse <- sqrt(mse)

# Print the RMSE
print(rmse)

#Plot rain ST-Hippolyte vs ERA5
ggplot(sth_nasa, aes(x = rain.x, y = rain.y, color = factor(Year))) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(x = "Saint-Hippolyte (meteo.gc.ca)", y = "NASA Data Rods") +
  ggtitle("Precipitation comparison Saint-Hippolyte vs NASA Data Rods") +
  geom_text(x = 50, y = 25, label = paste("RMSE:", round(rmse, 3)), hjust = 0, vjust = 1, color = "black") +
  theme_minimal()

cor(era_sth$rain, era_sth$precip.mm.day., method = "pearson")

cor(era_sth$rain, era_sth$precip.mm.day., method = "pearson")

temp <- merge(df_daily[, c("date", "airT_oC")], era5[, c("date","year", "air_temp.C.")], by = "date")

resid_temp <- temp$airT_oC - temp$air_temp.C.

se_temp <- resid_temp^2

mse_temp <- mean(se_temp)

rmse_temp <- sqrt(mse_temp)

paste(rmse_temp)

ggplot(temp, aes(x = airT_oC, y = air_temp.C., color = factor(year))) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(x = "Buoy", y = "ERA5") +
  ggtitle("Temperature comparison Buoy and ERA5") +
  geom_text(x = 0, y = 20, label = paste("RMSE:", round(rmse_temp, 3)), hjust = 0, vjust = 1, color = "black") +
  theme_minimal()

wind <- merge(df_daily[, c("date", "wind")], era5[, c("date","year", "wind_sp.m.s.")], by = "date")

resid_wind <- wind$wind - wind$wind_sp.m.s.

se_wind <- resid_wind^2

mse_wind <- mean(se_wind)

rmse_wind <- sqrt(mse_wind)

paste(rmse_wind)

ggplot(wind, aes(x = wind, y = wind_sp.m.s., color = factor(year))) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(x = "Buoy", y = "ERA5") +
  ggtitle("Wind comparison Buoy and ERA5") +
  geom_text(x = 3, y = 2, label = paste("RMSE:", round(rmse_wind, 3)), hjust = 0, vjust = 1, color = "black") +
  theme_minimal()

lm <- lm(wind ~ wind_sp.m.s., data = wind)
summary(lm)



correction_coefficient <- mean(wind$wind / wind$wind_sp.m.s.)

wind$corrected <- wind$wind_sp.m.s. * correction_coefficient


resid_windc <- wind$wind - wind$corrected

se_windc <- resid_windc^2

mse_windc <- mean(se_windc)

rmse_windc <- sqrt(mse_windc)

paste(rmse_windc)


ggplot(wind, aes(x = wind, y = corrected, color = factor(year))) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(x = "Buoy", y = "ERA5") +
  ggtitle("Wind comparison Buoy and ERA5 *with correction*") +
  geom_text(x = 3, y = 2, label = paste("RMSE:", round(rmse_windc, 3)), hjust = 0, vjust = 1, color = "black") +
  theme_minimal()



