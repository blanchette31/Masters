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

# Define x-axis breaks and labels for July and August
breaks_jul_aug <- c(182, 196, 213)  # July 1, July 15, August 1
labels_jul_aug <- c("Early July", "Mid July", "Early Aug")

# Filter data for DOY 180–220
df_doy180_220 <- df %>%
  filter(doy >= 180, doy <= 220) %>%
  arrange(year, doy)

# Temperature plot
p_temp <- ggplot(df_doy180_220, aes(x = doy, y = air_temp.C., color = factor(year))) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = 200, linetype = "dotted", color = "grey40", size = 0.8) +
  scale_color_manual(values = col_ind) +
  scale_x_continuous(breaks = breaks_jul_aug, labels = labels_jul_aug) +
  labs(title = "Daily Air Temperature (DOY 180–220)", x = "Day of Year", y = "Temperature (°C)", color = "Year") +
  theme_classic()
ggsave(filename = paste0(output_dir, "temperature_doy180_220.png"), plot = p_temp, width = 8, height = 5, dpi = 600)

# Wind speed plot
p_wind <- ggplot(df_doy180_220, aes(x = doy, y = wind_sp.m.s., color = factor(year))) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = 200, linetype = "dotted", color = "grey40", size = 0.8) +
  scale_color_manual(values = col_ind) +
  scale_x_continuous(breaks = breaks_jul_aug, labels = labels_jul_aug) +
  labs(title = "Daily Mean Wind Speed (DOY 180–220)", x = "Day of Year", y = "Wind Speed (m/s)", color = "Year") +
  theme_classic()
ggsave(filename = paste0(output_dir, "wind_speed_doy180_220.png"), plot = p_wind, width = 8, height = 5, dpi = 600)

# Radiation plot
p_rad <- ggplot(df_doy180_220, aes(x = doy, y = global_rad.MJ.m2., color = factor(year))) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = 200, linetype = "dotted", color = "grey40", size = 0.8) +
  scale_color_manual(values = col_ind) +
  scale_x_continuous(breaks = breaks_jul_aug, labels = labels_jul_aug) +
  labs(title = "Daily Global Radiation (DOY 180–220)", x = "Day of Year", y = "Radiation (MJ/m²)", color = "Year") +
  theme_classic()
ggsave(filename = paste0(output_dir, "radiation_doy180_220.png"), plot = p_rad, width = 8, height = 5, dpi = 600)

# Rainfall plot
p_rain <- ggplot(df_doy180_220, aes(x = doy, y = rain, color = factor(year))) +
  geom_line(size = 1.2) +
  geom_vline(xintercept = 200, linetype = "dotted", color = "grey40", size = 0.8) +
  scale_color_manual(values = col_ind) +
  scale_x_continuous(breaks = breaks_jul_aug, labels = labels_jul_aug) +
  labs(title = "Daily Rainfall (DOY 180–220)", x = "Day of Year", y = "Rainfall (mm)", color = "Year") +
  theme_classic()
ggsave(filename = paste0(output_dir, "rainfall_doy180_220.png"), plot = p_rain, width = 8, height = 5, dpi = 600)

