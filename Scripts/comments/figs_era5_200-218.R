# Prepare workspace
rm(list = ls())

# Load libraries
library(tidyverse)

# Load data
era5 <- read.csv("Data/Processed/precip/era5_prep.csv")
pheno <- read.csv("Data/Processed/phenocam/pheno_clean.csv")

# Filter to exclude everything before DOY 121
era5 <- era5 %>%
  filter(doy >= 121, doy <= 307)

# Merge phenocam data
df <- merge(era5, pheno[, c("doy_start", "doy_end", "year")], by = "year")

# Add period classification and PAR conversion
df <- df %>%
  rowwise() %>%
  mutate(
    period_rel_leaf_fall = case_when(
      doy >= doy_start ~ "leaf_fall",
      doy < (doy_start - 14) ~ "summer",
      TRUE ~ "before_leaf_fall"
    ),
    PAR = global_rad.MJ.m2. * 0.48
  )

# Cumulative metrics
df <- df %>%
  group_by(year) %>%
  mutate(
    cumulative_rad = cumsum(global_rad.MJ.m2.),
    cumulative_air_temp = cumsum(air_temp.C.)
  )

# Subset for DOY 200–218
df_doy200_218 <- df %>%
  filter(doy >= 200, doy <= 218) %>%
  arrange(year, doy)  # Ensure chronological order

# Save raw data for DOY 200–218
write.csv(df_doy200_218, "Data/Processed/comments/raw_era5_doy200_218.csv", row.names = FALSE)

# Summarize DOY 200–218 data by year
summary_doy200_218 <- df_doy200_218 %>%
  group_by(year) %>%
  summarize(
    total_temp = sum(air_temp.C., na.rm = TRUE),
    average_temp = mean(air_temp.C., na.rm = TRUE),
    total_rain = sum(rain, na.rm = TRUE),
    total_rad = sum(global_rad.MJ.m2., na.rm = TRUE),
    mean_wind = mean(wind_sp.m.s., na.rm = TRUE),
    max_wind = max(wind_sp.m.s., na.rm = TRUE)
  ) %>%
  arrange(year)  # Chronological order

# Save summary table
write.csv(summary_doy200_218, "Data/Processed/comments/summary_era5_doy200_218.csv", row.names = FALSE)

# Print summary table
print(summary_doy200_218)



# Define custom color palette
col_ind <- c("#44adce", "#8ad4dd", "#f2393a", "#1f5486", "#fde0a1", "#fe691d", "#ffc047")


# Temperature plot
ggplot(df_doy200_218, aes(x = doy, y = air_temp.C., color = factor(year))) +
  geom_line(size = 1.1) +
  scale_color_manual(values = col_ind) +
  labs(title = "Daily Air Temperature (DOY 200–218)", x = "Day of Year", y = "Temperature (°C)", color = "Year") +
  theme_classic()

# Mean wind speed plot
ggplot(df_doy200_218, aes(x = doy, y = wind_sp.m.s., color = factor(year))) +
  geom_line(size = 1.1) +
  scale_color_manual(values = col_ind) +
  labs(title = "Daily Mean Wind Speed (DOY 200–218)", x = "Day of Year", y = "Wind Speed (m/s)", color = "Year") +
  theme_classic()


# Global radiation plot
ggplot(df_doy200_218, aes(x = doy, y = global_rad.MJ.m2., color = factor(year))) +
  geom_line(size = 1.1) +
  scale_color_manual(values = col_ind) +
  labs(title = "Daily Global Radiation (DOY 200–218)", x = "Day of Year", y = "Radiation (MJ/m²)", color = "Year") +
  theme_classic()

# Rainfall plot
ggplot(df_doy200_218, aes(x = doy, y = rain, color = factor(year))) +
  geom_line(size = 1.1) +
  scale_color_manual(values = col_ind) +
  labs(title = "Daily Rainfall (DOY 200–218)", x = "Day of Year", y = "Rainfall (mm)", color = "Year") +
  theme_classic()




# Filter data for DOY 200–243
df_doy200_243 <- df %>%
  filter(doy >= 200, doy <= 243) %>%
  arrange(year, doy)

# Save raw data for DOY 200–218
write.csv(df_doy200_243, "Data/Processed/comments/raw_era5_doy200_243.csv", row.names = FALSE)

# Summarize DOY 200–218 data by year
summary_doy200_243 <- df_doy200_243 %>%
  group_by(year) %>%
  summarize(
    total_temp = sum(air_temp.C., na.rm = TRUE),
    average_temp = mean(air_temp.C., na.rm = TRUE),
    total_rain = sum(rain, na.rm = TRUE),
    total_rad = sum(global_rad.MJ.m2., na.rm = TRUE),
    mean_wind = mean(wind_sp.m.s., na.rm = TRUE),
    max_wind = max(wind_sp.m.s., na.rm = TRUE)
  ) %>%
  arrange(year)  # Chronological order

# Save summary table
write.csv(summary_doy200_243, "Data/Processed/comments/summary_era5_doy200_243.csv", row.names = FALSE)

# Print summary table
print(summary_doy200_243)
# Temperature plot
ggplot(df_doy200_243, aes(x = doy, y = air_temp.C., color = factor(year))) +
  geom_line(size = 1.2) +
  scale_color_manual(values = col_ind) +
  labs(title = "Daily Air Temperature (DOY 200–243)", x = "Day of Year", y = "Temperature (°C)", color = "Year") +
  theme_classic()

# Mean wind speed plot
ggplot(df_doy200_243, aes(x = doy, y = wind_sp.m.s., color = factor(year))) +
  geom_line(size = 1.2) +
  scale_color_manual(values = col_ind) +
  labs(title = "Daily Mean Wind Speed (DOY 200–243)", x = "Day of Year", y = "Wind Speed (m/s)", color = "Year") +
  theme_classic()

# Global radiation plot
ggplot(df_doy200_243, aes(x = doy, y = global_rad.MJ.m2., color = factor(year))) +
  geom_line(size = 1.2) +
  scale_color_manual(values = col_ind) +
  labs(title = "Daily Global Radiation (DOY 200–243)", x = "Day of Year", y = "Radiation (MJ/m²)", color = "Year") +
  theme_classic()

# Rainfall plot
ggplot(df_doy200_243, aes(x = doy, y = rain, color = factor(year))) +
  geom_line(size = 1.2) +
  scale_color_manual(values = col_ind) +
  labs(title = "Daily Rainfall (DOY 200–243)", x = "Day of Year", y = "Rainfall (mm)", color = "Year") +
  theme_classic()