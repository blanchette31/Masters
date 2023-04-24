# Prepare workspace
rm(list = ls())

# Load libraries
library(tidyverse)
library(patchwork)  # For combining plots

# Load data
era5 <- read.csv("Data/Processed/precip/era5_prep.csv")
pheno <- read.csv("Data/Processed/phenocam/pheno_clean.csv")

# Filter to exclude everything before DOY 121
era5 <- era5 %>%
  filter(doy >= 121, doy <= 307)

# Merge phenocam data
df <- merge(era5, pheno[, c("doy_start", "doy_end", "year")], by = "year")# Load libraries
library(tidyverse)

# Define custom color palette
col_ind <- c("#44adce", "#8ad4dd", "#f2393a", "#1f5486", "#fde0a1", "#fe691d", "#ffc047")

# Create output directory if it doesn't exist
output_dir <- "Data/Figures/comments/era5_august/"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Define x-axis breaks and labels
breaks_200_218 <- c(200, 213, 228)
labels_200_218 <- c("Late July", "Early Aug", "Mid Aug")

breaks_200_243 <- c(200, 213, 228, 243)
labels_200_243 <- c("Late July", "Early Aug", "Mid Aug", "Late Aug")

# Filter data
df_doy200_218 <- df %>% filter(doy >= 200, doy <= 218) %>% arrange(year, doy)
df_doy200_243 <- df %>% filter(doy >= 200, doy <= 243) %>% arrange(year, doy)

### Temperature ###
p_temp_218 <- ggplot(df_doy200_218, aes(x = doy, y = air_temp.C., color = factor(year))) +
  geom_line(size = 1.2) +
  scale_color_manual(values = col_ind) +
  scale_x_continuous(breaks = breaks_200_218, labels = labels_200_218) +
  labs(title = "Temperature (DOY 200–218)", x = "Day of Year", y = "°C", color = "Year") +
  theme_classic()

p_temp_243 <- ggplot(df_doy200_243, aes(x = doy, y = air_temp.C., color = factor(year))) +
  geom_line(size = 1.2) +
  scale_color_manual(values = col_ind) +
  scale_x_continuous(breaks = breaks_200_243, labels = labels_200_243) +
  labs(title = "Temperature (DOY 200–243)", x = "Day of Year", y = "°C", color = "Year") +
  theme_classic()

ggsave(paste0(output_dir, "grid_temperature.png"), p_temp_218 + p_temp_243, width = 12, height = 5, dpi = 600)

### Wind Speed ###
p_wind_218 <- ggplot(df_doy200_218, aes(x = doy, y = wind_sp.m.s., color = factor(year))) +
  geom_line(size = 1.2) +
  scale_color_manual(values = col_ind) +
  scale_x_continuous(breaks = breaks_200_218, labels = labels_200_218) +
  labs(title = "Mean Wind Speed (DOY 200–218)", x = "Day of Year", y = "m/s", color = "Year") +
  theme_classic()

p_wind_243 <- ggplot(df_doy200_243, aes(x = doy, y = wind_sp.m.s., color = factor(year))) +
  geom_line(size = 1.2) +
  scale_color_manual(values = col_ind) +
  scale_x_continuous(breaks = breaks_200_243, labels = labels_200_243) +
  labs(title = "Mean Wind Speed (DOY 200–243)", x = "Day of Year", y = "m/s", color = "Year") +
  theme_classic()

ggsave(paste0(output_dir, "grid_wind_speed.png"), p_wind_218 + p_wind_243, width = 12, height = 5, dpi = 600)

### Radiation ###
p_rad_218 <- ggplot(df_doy200_218, aes(x = doy, y = global_rad.MJ.m2., color = factor(year))) +
  geom_line(size = 1.2) +
  scale_color_manual(values = col_ind) +
  scale_x_continuous(breaks = breaks_200_218, labels = labels_200_218) +
  labs(title = "Global Radiation (DOY 200–218)", x = "Day of Year", y = "MJ/m²", color = "Year") +
  theme_classic()

p_rad_243 <- ggplot(df_doy200_243, aes(x = doy, y = global_rad.MJ.m2., color = factor(year))) +
  geom_line(size = 1.2) +
  scale_color_manual(values = col_ind) +
  scale_x_continuous(breaks = breaks_200_243, labels = labels_200_243) +
  labs(title = "Global Radiation (DOY 200–243)", x = "Day of Year", y = "MJ/m²", color = "Year") +
  theme_classic()

ggsave(paste0(output_dir, "grid_radiation.png"), p_rad_218 + p_rad_243, width = 12, height = 5, dpi = 600)

### Rainfall ###
p_rain_218 <- ggplot(df_doy200_218, aes(x = doy, y = rain, color = factor(year))) +
  geom_line(size = 1.2) +
  scale_color_manual(values = col_ind) +
  scale_x_continuous(breaks = breaks_200_218, labels = labels_200_218) +
  labs(title = "Rainfall (DOY 200–218)", x = "Day of Year", y = "mm", color = "Year") +
  theme_classic()

p_rain_243 <- ggplot(df_doy200_243, aes(x = doy, y = rain, color = factor(year))) +
  geom_line(size = 1.2) +
  scale_color_manual(values = col_ind) +
  scale_x_continuous(breaks = breaks_200_243, labels = labels_200_243) +
  labs(title = "Rainfall (DOY 200–243)", x = "Day of Year", y = "mm", color = "Year") +
  theme_classic()

ggsave(paste0(output_dir, "grid_rainfall.png"), p_rain_218 + p_rain_243, width = 12, height = 5, dpi = 600)