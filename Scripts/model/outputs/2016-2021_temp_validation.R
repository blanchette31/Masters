# prepare workspace
rm(list = ls())

# Load necessary libraries
library(tidyverse)
library(lubridate)
library(cowplot)
library(Metrics)

# Prepare the data (already done in the code you provided)
obs_2016 <- read.csv("Data/Processed/lake/temp_2016.csv")
obs_2017 <- read.csv("Data/Processed/lake/temp_2017.csv")
obs_2018 <- read.csv("Data/Processed/lake/temp_2018.csv")
obs_2019 <- read.csv("Data/Processed/lake/temp_2019.csv")
obs_2021 <- read.csv("Data/Processed/lake/temp_2021.csv")
obs <- rbind(obs_2016, obs_2017, obs_2018, obs_2019, obs_2021)
model <- read.csv("Data/model/results/temp_all_years.csv")

# Convert from wide to long format
mod_long <- model %>%
  pivot_longer(cols = starts_with("wtr_"), 
               names_to = "depth", 
               values_to = "temperature") %>%
  mutate(
    depth = as.numeric(str_remove(depth, "wtr_")),  # Extract numeric depth
    year = year(date)  # Extract year
  )

mod_long_years <- mod_long %>% 
  filter(year %in% c(2016,2017, 2018, 2019, 2021))

obs_filtered <- obs %>% filter(Depth %in% c(0.5, 1, 4.5, 6.5, 8.5, 10.5))

mod_filtered <- mod_long_years %>%
  filter(depth %in% c(0, 1, 4, 5, 6, 7, 8, 9, 10, 11)) %>%
  bind_rows(
    # Calculate the average for 0-1m depth
    mod_long_years %>%
      filter(depth %in% c(0, 1)) %>%
      group_by(date, year) %>%
      summarise(depth = 0.5, temperature = mean(temperature, na.rm = TRUE)) %>%
      ungroup(),
    
    # Calculate the average for 4-5m depth
    mod_long_years %>%
      filter(depth %in% c(4, 5)) %>%
      group_by(date, year) %>%
      summarise(depth = 4.5, temperature = mean(temperature, na.rm = TRUE)) %>%
      ungroup(),
    
    # Calculate the average for 6-7m depth
    mod_long_years %>%
      filter(depth %in% c(6, 7)) %>%
      group_by(date, year) %>%
      summarise(depth = 6.5, temperature = mean(temperature, na.rm = TRUE)) %>%
      ungroup(),
    
    # Calculate the average for 8-9m depth
    mod_long_years %>%
      filter(depth %in% c(8, 9)) %>%
      group_by(date, year) %>%
      summarise(depth = 8.5, temperature = mean(temperature, na.rm = TRUE)) %>%
      ungroup(),
    
    # Calculate the average for 10-11m depth
    mod_long_years %>%
      filter(depth %in% c(10, 11)) %>%
      group_by(date, year) %>%
      summarise(depth = 10.5, temperature = mean(temperature, na.rm = TRUE)) %>%
      ungroup()
  ) %>%
  arrange(date, depth)


mod_filtered <- mod_filtered %>%
  filter(!depth %in% c(0, 4, 5, 6, 7, 8, 9, 10, 11))

obs_filtered <- obs_filtered %>%
  rename(depth = Depth, date = Date)

obs_filtered$doy <- yday(obs_filtered$date)
mod_filtered$doy <- yday(mod_filtered$date)


# Subset for 2021
mod_filtered_2021 <- mod_filtered %>%
  filter(year == 2021)

# Subset for other years (excluding 2021)
mod_filtered_other <- mod_filtered %>%
  filter(year != 2021)
# Subset for 2021
obs_filtered_2021 <- obs_filtered %>%
  filter(year == 2021)

# Subset for other years (excluding 2021)
obs_filtered_other <- obs_filtered %>%
  filter(year != 2021)


# Calculate RMSE and R^2 for each depth
metrics_2021 <- obs_filtered_2021 %>%
  inner_join(mod_filtered_2021, by = c("date", "depth")) %>%
  group_by(depth) %>%
  summarise(
    RMSE = rmse(temperature, Temperature),  # Calculate RMSE
    R2 = cor(temperature, Temperature)^2    # Calculate R^2 (square of the correlation)
  )
# Merge the calculated metrics with the data for plotting
mod_filtered_2021 <- mod_filtered_2021 %>%
  left_join(metrics_2021, by = "depth")

# Calculate RMSE and R^2 for each depth
metrics_other <- obs_filtered_other %>%
  inner_join(mod_filtered_other, by = c("date", "depth")) %>%
  group_by(depth) %>%
  summarise(
    RMSE = rmse(temperature, Temperature),  # Calculate RMSE
    R2 = cor(temperature, Temperature)^2    # Calculate R^2 (square of the correlation)
  )


# Merge the calculated metrics with the data for plotting
mod_filtered_other <- mod_filtered_other %>%
  left_join(metrics_other, by = "depth")

# Plot the data with RMSE and R^2 annotations

p2021 <- ggplot() +
  geom_line(data = mod_filtered_2021, aes(x = doy, y = temperature, group = depth, color = "Modeled"), linetype = "solid", linewidth = 1.5) +
  geom_point(data = obs_filtered_2021, aes(x = doy, y = Temperature, group = depth, color = "Buoy"), shape = 16, size = 1) +
  scale_x_continuous(
    breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),  # First day of each month
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")  # Custom labels for each month
  ) +
  facet_wrap(~depth, scales = "free_y", ncol = 1) +  # Facet by depth
  theme_classic(base_size = 16) +
  labs(x = "Date", y = "Temperature (°C)", color = "Data Source", title = "b") +
  scale_color_manual(values = c("Buoy" = "black", "Modeled" = "#85A1EF")) +  # Custom colors
  theme(strip.text = element_blank(),  # Remove titles from the facet panes
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(
    breaks = seq(0, 30, by = 10),
    limits = c(0, 30)
  ) +
  # Add depth text inside the pane
  geom_text(data = mod_filtered_2021, aes(x = min(doy), y = Inf, label = paste(depth, "m")), 
            inherit.aes = FALSE, hjust = -0.1, vjust = 1.2, color = "black", size = 7) +
  # Add RMSE and R^2 text annotations inside the facet panes
  geom_text(data = metrics_2021, aes(x = 325, y = 15, label = paste("rmse: ", round(RMSE, 2), "\nR^2: ", round(R2, 2))),
            inherit.aes = FALSE, hjust = 0, vjust = 0, color = "black", size = 7)

p2021

ggsave("Data/Figures/comments/2021_temp_validation_V2.jpg", p2021, dpi = 300 , width = 10, height = 6, units = "in")




# Convert date column to Date class if not already
obs_filtered_other$date <- as.Date(obs_filtered_other$date)
mod_filtered_other$date <- as.Date(mod_filtered_other$date)

# Plot the data with RMSE and R^2 annotations for other years (2016-2019)

p_all <- ggplot() +
  geom_line(data = mod_filtered_other, aes(x = date, y = temperature, group = depth, color = "Modeled"), linetype = "solid", linewidth = 1.5) +
  geom_point(data = obs_filtered_other, aes(x = date, y = Temperature, group = depth, color = "Buoy"), shape = 16, size = 1) +
  scale_x_date(
    breaks = seq(as.Date("2016-01-01"), as.Date("2019-12-31"), by = "12 month"),  # Breaks every month from 2016 to 2019
    labels = scales::date_format("%b %Y")  # Format the labels as Month Year
  ) +
  facet_wrap(~depth, scales = "free_y", ncol = 1) +  # Facet by depth
  theme_classic(base_size = 16) +
  labs(x = "", y = "Temperature (°C)", color = "Data Source") +
  scale_color_manual(values = c("Buoy" = "black", "Modeled" = "#85A1EF")) +  # Custom colors
  theme(strip.text = element_blank()) +
  scale_y_continuous(
    breaks = seq(0, 30, by = 10),
    limits = c(0, 30)
  ) +
  # Add depth text inside the pane
  geom_text(data = mod_filtered_other, aes(x = min(date), y = Inf, label = paste(depth, "m")), 
            inherit.aes = FALSE, hjust = -0.1, vjust = 1.2, color = "black", size = 7) 
  # Add RMSE and R^2 text annotations inside the facet panes
  # geom_text(data = metrics_other, aes(x = as.Date("2019-01-01"), y = 12, label = paste("RMSE: ", round(RMSE, 2), "\nR^2: ", round(R2, 2))),
  #           inherit.aes = FALSE, hjust = 0, vjust = 0, color = "black", size = 2)


p_all 

ggsave("Data/Figures/comments/2016-2019_temp_calibration_V3.jpg", p_all, dpi = 300 , width = 10, height = 6, units = "in")
