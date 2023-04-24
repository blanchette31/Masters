# prepare workspace
rm(list = ls())

# Load necessary libraries
library(tidyverse)
library(lubridate)
library(cowplot)
library(Metrics)

# Prepare the data (already done in the code you provided)

obs <- read.csv("Data/Processed/lake/temp_2022.csv")
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

mod_long_2022 <- mod_long %>% 
  filter(year == 2022)

obs_filtered <- obs %>% filter(Depth %in% c(0.65, 0.73, 4.58, 6.53, 8.53, 10.53))

mod_filtered <- mod_long_2022 %>%
  filter(depth %in% c(0, 1, 4, 5, 6, 7, 8, 9, 10, 11)) %>%
  bind_rows(
    # Calculate the average for 0-1m depth
    mod_long_2022 %>%
      filter(depth %in% c(0, 1)) %>%
      group_by(date, year) %>%
      summarise(depth = 0.65, temperature = mean(temperature, na.rm = TRUE)) %>%
      ungroup(),
    
    # Calculate the average for 4-5m depth
    mod_long_2022 %>%
      filter(depth %in% c(4, 5)) %>%
      group_by(date, year) %>%
      summarise(depth = 4.58, temperature = mean(temperature, na.rm = TRUE)) %>%
      ungroup(),
    
    # Calculate the average for 6-7m depth
    mod_long_2022 %>%
      filter(depth %in% c(6, 7)) %>%
      group_by(date, year) %>%
      summarise(depth = 6.53, temperature = mean(temperature, na.rm = TRUE)) %>%
      ungroup(),
    
    # Calculate the average for 8-9m depth
    mod_long_2022 %>%
      filter(depth %in% c(8, 9)) %>%
      group_by(date, year) %>%
      summarise(depth = 8.53, temperature = mean(temperature, na.rm = TRUE)) %>%
      ungroup(),
    
    # Calculate the average for 10-11m depth
    mod_long_2022 %>%
      filter(depth %in% c(10, 11)) %>%
      group_by(date, year) %>%
      summarise(depth = 10.53, temperature = mean(temperature, na.rm = TRUE)) %>%
      ungroup()
  ) %>%
  arrange(date, depth)


mod_filtered <- mod_filtered %>%
  filter(!depth %in% c(0, 4, 5, 6, 7, 8, 9, 10, 11))

mod_filtered <- mod_filtered %>%
  mutate(depth = ifelse(depth == 1, 0.73, depth)) 

obs_filtered <- obs_filtered %>%
  rename(depth = Depth, date = Date)

obs_filtered$doy <- yday(obs_filtered$date)
mod_filtered$doy <- yday(mod_filtered$date)

# Calculate RMSE and R^2 for each depth
metrics <- obs_filtered %>%
  inner_join(mod_filtered, by = c("date", "depth")) %>%
  group_by(depth) %>%
  summarise(
    RMSE = rmse(temperature, Temperature),  # Calculate RMSE
    R2 = cor(temperature, Temperature)^2    # Calculate R^2 (square of the correlation)
  )

# Merge the calculated metrics with the data for plotting
mod_filtered <- mod_filtered %>%
  left_join(metrics, by = "depth")


# Plot the data with RMSE and R^2 annotations
p2022 <- ggplot() +
  geom_line(data = mod_filtered, aes(x = doy, y = temperature, group = depth, color = "Modeled"), linetype = "solid", linewidth = 1.5) +
  geom_point(data = obs_filtered, aes(x = doy, y = Temperature, group = depth, color = "Buoy"), shape = 16, size = 1) +
  scale_x_continuous(
    breaks = c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335),  # First day of each month
    labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")  # Custom labels for each month
  ) +
  facet_wrap(~depth, scales = "free_y", ncol = 1) +  # Facet by depth
  theme_classic(base_size = 16) +
  labs(x = "Date", y = "Temperature (°C)", color = "Data Source", title = "c") +
  scale_color_manual(values = c("Buoy" = "black", "Modeled" = "#85A1EF")) +  # Custom colors
  theme(strip.text = element_blank(),  # Remove titles from the facet panes
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(
    breaks = seq(0, 30, by = 10),
    limits = c(0, 30)
  ) +
  # Add depth text inside the pane
  geom_text(data = mod_filtered, aes(x = min(doy), y = Inf, label = paste(depth, "m")), 
            inherit.aes = FALSE, hjust = -0.1, vjust = 1.2, color = "black", size = 7)
  # Add RMSE and R^2 text annotations inside the facet panes
  # geom_text(data = metrics, aes(x = 325, y = 15, label = paste("rmse: ", round(RMSE, 2), "\nR^2: ", round(R2, 2))),
  #           inherit.aes = FALSE, hjust = 0, vjust = 0, color = "black", size = 2.5)

p2022

ggsave("Data/Figures/comments/2022_temp_validation_V2.jpg", p2022, dpi = 300 , width = 10, height = 6, units = "in")
