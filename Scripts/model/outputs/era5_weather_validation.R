#prepare workspace
rm(list = ls())

# load libraries 
library(tidyverse)
library(cowplot)
library(lubridate)
library(ggpubr)

# load data
sth <- read.csv("Data/Processed/precip/precip_sth_merged.csv")
era5 <- read.csv("Data/Processed/precip/era5_prep.csv")
df_15 <- read.csv("Data/Processed/boue/boue_2015.csv")
df_1621 <- read.csv("Data/Processed/boue/boue_2016-2021.csv")
df_22 <- read.csv("Data/Processed/boue/boue_2022.csv")


# Convert date column to Date format (assuming it's named "date")
sth <- sth %>%
  mutate(date = as.Date(date)) %>%
  filter(Year %in% c(2015:2019, 2021:2022) & date >= as.Date("2015-05-14")) %>% 
  select(date, Year, Total.Rain..mm.) %>% 
  rename( year = Year,
          rain_sth = Total.Rain..mm.) %>% 
  mutate(doy = yday(date))

era5 <- era5 %>% filter(year != 2020 & date >= as.Date("2015-05-14"))


df_15$date <- as.Date(df_15$date)
df_1621$date <- as.Date(df_1621$date)
df_22 <- df_22 %>%
  mutate(date = as.Date(timestamp)) 

# Keep only specified columns in df_15, df_1621, and df_22
selected_columns <- c("year", "date", "doy", "airT_oC", "windspeedavg_m_per_s")


df_15 <- df_15 %>% select(all_of(selected_columns))
df_1621 <- df_1621 %>% select(all_of(selected_columns))


# Keep only specified columns in df_22 (with its different column names)
df_22 <- df_22 %>% select(year, date, doy, AirTemp_Deg_C_Smp, WindSpd_Avg_m_per_s_Avg)




# Aggregate (average) temperature and wind speed by date
daily_15 <- df_15 %>%
  group_by(date) %>%
  summarise(
    airtemp = mean(airT_oC, na.rm = TRUE),
    windavg = mean(windspeedavg_m_per_s, na.rm = TRUE)
  )

daily_1621 <- df_1621 %>%
  group_by(date) %>%
  summarise(
    airtemp = mean(airT_oC, na.rm = TRUE),
    windavg = mean(windspeedavg_m_per_s, na.rm = TRUE)
  )

daily_22 <- df_22 %>%
  group_by(date) %>%
  summarise(
    airtemp = mean(AirTemp_Deg_C_Smp, na.rm = TRUE),
    windavg = mean(WindSpd_Avg_m_per_s_Avg, na.rm = TRUE)
  )


df <- rbind(daily_15, daily_1621, daily_22)

df <- df %>% 
  mutate(doy = yday(date),
         year = year(date))

era5_tw <- era5 %>% select(date, air_temp.C., wind_sp.m.s.) %>% 
            rename(airtemp_era = air_temp.C.,
                   windavg_era = wind_sp.m.s.)

era5_rain <- era5 %>% select(date, rain)

comp_rain <- merge(sth, era5_rain, by = "date", all = TRUE)
comp_rain$year <- as.factor(comp_rain$year)

comp_rain$year <- factor(comp_rain$year, levels = rev(levels(comp_rain$year)))
comp_rain$year <- factor(comp_rain$year, levels(comp_rain$year)[c(5, 2, 1, 3, 6, 7, 4)])

# Function to calculate RMSE
calc_rmse <- function(obs, pred) {
  sqrt(mean((obs - pred)^2, na.rm = TRUE))
}






custom_palette <-  c("#e76254","#ef8a47", "#f7aa58", "#ffd06f" ,"#72bcd5" ,"#528fad" ,"#1e466e")
## plot 

p1 <- ggplot(comp_rain, aes(x = rain_sth, y = rain, color = year))+
  geom_point(shape = 16, size = 1)+
  theme_cowplot()+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  scale_color_manual(values = custom_palette)+
  labs(x = "Daily mean rain from local weather station (mm)", y = "Daily mean rain from ERA5 \n reanalysis data (mm)", title = "a")+
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40,50,60), limits = c(0,60)) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40,50,60), limits = c(0,60)) 
  # annotate("text", x = max(comp_rain$rain_sth, na.rm = TRUE) * 0.7, 
  #          y = max(comp_rain$rain, na.rm = TRUE) * 0.1, 
  #          label = paste0("RMSE = ", round(rmse_rain, 2), " mm"), size = 5)
p1




df_comp <- merge(df, era5_tw, by = "date", all = TRUE)
df_comp$year <- as.factor(df_comp$year)

df_comp$year <- factor(df_comp$year, levels = rev(levels(df_comp$year)))
df_comp$year <- factor(df_comp$year, levels(df_comp$year)[c(5, 2, 1, 3, 6, 7, 4)])

temp_comp <- df_comp %>% 
  select(-c(windavg, windavg_era))
temp_comp <- na.omit(temp_comp)

p2 <- ggplot(temp_comp, aes(x = airtemp, y = airtemp_era, color = year)) +
  geom_point(shape = 16, size = 1) +
  theme_cowplot() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  scale_color_manual(values = custom_palette) +
  scale_y_continuous(breaks = c(-10, 0, 10, 20, 30), limits = c(-10,30)) +
  scale_x_continuous(breaks = c(-10, 0, 10, 20, 30), limits = c(-10,30)) +
  labs(x = "Daily mean temperature from buoy weather station (°C)", 
       y = "Daily mean temperature from \n ERA5 reanalysis data (°C)", title = "b") 
  # annotate("text", x = 15, y = -5, 
  #          label = paste0("RMSE = ", round(rmse_temp, 2), " °C"), size = 5)
p2




wind_comp <- df_comp %>% 
  select(-c(airtemp, airtemp_era)) %>% 
  mutate(windera_adj = windavg_era * 0.53)
wind_comp <- na.omit(wind_comp)








p3 <- ggplot(wind_comp, aes(x = windavg, y = windera_adj, color = year))+
  geom_point(shape = 16, size = 1)+
  theme_cowplot()+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  scale_color_manual(values = custom_palette)+
   scale_y_continuous(breaks = c(0, 1, 2, 3, 4), limits = c(0, 4))+
   scale_x_continuous(breaks = c(0, 1, 2, 3, 4), limits = c(0, 4))+
  labs(x = "Daily mean wind speed from buoy weather station (m/s)", y = "Daily mean wind speed from \n ERA5 reanalysis data (m/s)", title = "c")
  # annotate("text", x = 3, y = 0.5, 
  #          label = paste0("RMSE = ", round(rmse_wind, 2), " m/s"), size = 5)
p3

# Calculate R² for rain
r2_rain <- summary(lm(rain ~ rain_sth, data = comp_rain))$r.squared

# Calculate R² for temperature
r2_temp <- summary(lm(airtemp_era ~ airtemp, data = temp_comp))$r.squared

# Calculate R² for wind
r2_wind <- summary(lm(windera_adj ~ windavg, data = wind_comp))$r.squared

combined_plot <- plot_grid(p1, p2, p3, ncol = 2, nrow = 2)


# Calculate RMSE for each dataset
rmse_rain <- calc_rmse(comp_rain$rain_sth, comp_rain$rain)
rmse_temp <- calc_rmse(temp_comp$airtemp, temp_comp$airtemp_era)
rmse_wind <- calc_rmse(wind_comp$windavg, wind_comp$windera_adj)

# Display the combined plot
combined_plot

ggsave("Data/Figures/comments/era5_weather_validation_3_plots_V2.jpg", combined_plot, dpi = 300, width = 10, height = 6, units = "in")
