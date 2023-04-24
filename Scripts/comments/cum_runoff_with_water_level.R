# Prepare workspace
rm(list = ls())

# Load libraries
library(tidyverse)
library(lubridate)

# Load datasets
sth <- read.csv("Data/Processed/precip/precip_brief.csv")
sth$date <- as.Date(sth$date)
q <- read.csv("Data/Processed/debit/debit_2011-2022.csv")
q$date <- as.Date(q$date)
era5 <- read.csv("Data/Processed/precip/era5_cut_buoy.csv")
diff_wl <- read.csv("Data/Processed/comments/fall_waterlevel_calcs.csv")

# Merge precipitation and discharge data
df_era5 <- merge(era5[, c("doy", "year", "rain")],
                 q[, c('date', 'year', 'doy', 'debit_total_m3_jour')], 
                 by = c("year", "doy"))

# Ensure date format and order by date
df_era5 <- df_era5 %>%
  mutate(date = as.Date(date)) %>%
  arrange(date)

# Rename columns for clarity
colnames(df_era5)[c(3, 5)] <- c('rain', "q")

# Define lake surface area (m²)
lake_surface_area <- 62848  
watershed_area_m2 <- 1075472  # Watershed area in square meters

# Define start DOY thresholds per year
start_doy <- c("2015" = 264, "2016" = 265, "2017" = 272, 
               "2018" = 270, "2019" = 258, "2021" = 256, "2022" = 244)

# Filter data for relevant years and DOYs
df_filtered <- df_era5 %>%
  filter(year %in% names(start_doy)) %>%
  group_by(year) %>%
  filter(doy >= start_doy[as.character(year)])

# Calculate cumulative rain and discharge for each year
df_cumulative <- df_filtered %>%
  group_by(year) %>%
  summarise(
    total_q_m3 = sum(q, na.rm = TRUE),
    total_rain_m3 = sum(rain, na.rm = TRUE) * watershed_area_m2 / 1000,  # Convert mm to m³
    .groups = "drop"
  )

# Merge cumulative rain/discharge with water level changes
df_final <- merge(diff_wl, df_cumulative, by = "year")

# Compute runoff ratios with water level corrections
df_final <- df_final %>%
  mutate(
    runoff_ratio_original = (total_q_m3 / total_rain_m3) * 0.25,  # Without water level correction
    runoff_ratio_adj_firstlast = ((total_q_m3 + volume_changefirstlast_m3) / total_rain_m3)*0.25,  
    runoff_ratio_adj_maxwl = ((total_q_m3 + volume_change_maxwl_m3) / total_rain_m3) *0.25  
  )

# Print and export the final table
print(df_final)
write.csv(df_final, "Data/Processed/comments/cumulative_runoff_with_wl.csv", row.names = FALSE)
