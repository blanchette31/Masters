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
# Define custom color palette
col_ind <- c("#44adce", "#8ad4dd", "#f2393a", "#1f5486", "#fde0a1", "#fe691d", "#ffc047")

# Create output directory if it doesn't exist
output_dir <- "Data/Figures/comments/era5_august/"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Define x-axis breaks and labels
breaks_200_218 <- c(200, 213, 228)  # Late July, Early Aug, Mid Aug
labels_200_218 <- c("Late July", "Early Aug", "Mid Aug")

breaks_200_243 <- c(200, 213, 228, 243)  # Late July, Early Aug, Mid Aug, Late Aug
labels_200_243 <- c("Late July", "Early Aug", "Mid Aug", "Late Aug")

### DOY 200–218 ###
df_doy200_218 <- df %>%
  filter(doy >= 200, doy <= 218) %>%
  arrange(year, doy)

ggsave(paste0(output_dir, "temperature_doy200_218.png"),
       ggplot(df_doy200_218, aes(x = doy, y = air_temp.C., color = factor(year))) +
         geom_line(size = 1.2) +
         scale_color_manual(values = col_ind) +
         scale_x_continuous(breaks = breaks_200_218, labels = labels_200_218) +
         labs(title = "Daily Air Temperature (DOY 200–218)", x = "Day of Year", y = "Temperature (°C)", color = "Year") +
         theme_classic(),
       width = 8, height = 5, dpi = 600
)

ggsave(paste0(output_dir, "mean_wind_doy200_218.png"),
       ggplot(df_doy200_218, aes(x = doy, y = wind_sp.m.s., color = factor(year))) +
         geom_line(size = 1.2) +
         scale_color_manual(values = col_ind) +
         scale_x_continuous(breaks = breaks_200_218, labels = labels_200_218) +
         labs(title = "Daily Mean Wind Speed (DOY 200–218)", x = "Day of Year", y = "Wind Speed (m/s)", color = "Year") +
         theme_classic(),
       width = 8, height = 5, dpi = 600
)

ggsave(paste0(output_dir, "radiation_doy200_218.png"),
       ggplot(df_doy200_218, aes(x = doy, y = global_rad.MJ.m2., color = factor(year))) +
         geom_line(size = 1.2) +
         scale_color_manual(values = col_ind) +
         scale_x_continuous(breaks = breaks_200_218, labels = labels_200_218) +
         labs(title = "Daily Global Radiation (DOY 200–218)", x = "Day of Year", y = "Radiation (MJ/m²)", color = "Year") +
         theme_classic(),
       width = 8, height = 5, dpi = 600
)

ggsave(paste0(output_dir, "rainfall_doy200_218.png"),
       ggplot(df_doy200_218, aes(x = doy, y = rain, color = factor(year))) +
         geom_line(size = 1.2) +
         scale_color_manual(values = col_ind) +
         scale_x_continuous(breaks = breaks_200_218, labels = labels_200_218) +
         labs(title = "Daily Rainfall (DOY 200–218)", x = "Day of Year", y = "Rainfall (mm)", color = "Year") +
         theme_classic(),
       width = 8, height = 5, dpi = 600
)

### DOY 200–243 ###
df_doy200_243 <- df %>%
  filter(doy >= 200, doy <= 243) %>%
  arrange(year, doy)

ggsave(paste0(output_dir, "temperature_doy200_243.png"),
       ggplot(df_doy200_243, aes(x = doy, y = air_temp.C., color = factor(year))) +
         geom_line(size = 1.2) +
         scale_color_manual(values = col_ind) +
         scale_x_continuous(breaks = breaks_200_243, labels = labels_200_243) +
         labs(title = "Daily Air Temperature (DOY 200–243)", x = "Day of Year", y = "Temperature (°C)", color = "Year") +
         theme_classic(),
       width = 8, height = 5, dpi = 600
)

ggsave(paste0(output_dir, "mean_wind_doy200_243.png"),
       ggplot(df_doy200_243, aes(x = doy, y = wind_sp.m.s., color = factor(year))) +
         geom_line(size = 1.2) +
         scale_color_manual(values = col_ind) +
         scale_x_continuous(breaks = breaks_200_243, labels = labels_200_243) +
         labs(title = "Daily Mean Wind Speed (DOY 200–243)", x = "Day of Year", y = "Wind Speed (m/s)", color = "Year") +
         theme_classic(),
       width = 8, height = 5, dpi = 600
)

ggsave(paste0(output_dir, "radiation_doy200_243.png"),
       ggplot(df_doy200_243, aes(x = doy, y = global_rad.MJ.m2., color = factor(year))) +
         geom_line(size = 1.2) +
         scale_color_manual(values = col_ind) +
         scale_x_continuous(breaks = breaks_200_243, labels = labels_200_243) +
         labs(title = "Daily Global Radiation (DOY 200–243)", x = "Day of Year", y = "Radiation (MJ/m²)", color = "Year") +
         theme_classic(),
       width = 8, height = 5, dpi = 600
)

ggsave(paste0(output_dir, "rainfall_doy200_243.png"),
       ggplot(df_doy200_243, aes(x = doy, y = rain, color = factor(year))) +
         geom_line(size = 1.2) +
         scale_color_manual(values = col_ind) +
         scale_x_continuous(breaks = breaks_200_243, labels = labels_200_243) +
         labs(title = "Daily Rainfall (DOY 200–243)", x = "Day of Year", y = "Rainfall (mm)", color = "Year") +
         theme_classic(),
       width = 8, height = 5, dpi = 600
)

