#prepare workspace
rm(list =ls())
# Load necessary libraries
library(tidyverse)
library(lubridate)
library(cowplot)
library(Metrics)

# Prepare the data (already done in the code you provided)
obs <- read.csv("Data/Processed/lake/temp_2015.csv")
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

mod_long_2015 <- mod_long %>% 
  filter(year == 2015)

obs_filtered <- obs %>% filter(Depth %in% c(1.7, 4, 5, 6, 8, 10))

mod_filtered <- mod_long_2015 %>%
  filter(depth %in% c(1.0, 2.0, 4, 5, 6, 8, 10)) %>%
  bind_rows(
    mod_long_2015 %>%
      filter(depth %in% c(1.0, 2.0)) %>%
      group_by(date, year) %>%
      summarise(depth = 1.7, temperature = mean(temperature, na.rm = TRUE)) %>%
      ungroup()
  ) %>%
  arrange(date, depth)

mod_filtered <- mod_filtered %>%
  filter(!depth %in% c(1.0, 2.0))

obs_filtered <- obs_filtered %>%
  rename(depth = Depth, date = Date)

obs_filtered$doy <- yday(obs_filtered$date)
mod_filtered$doy <- yday(mod_filtered$date)

# Calculate RMSE and R^2 for each depth
metrics <- obs_filtered %>%
  inner_join(mod_filtered, by = c("doy", "depth")) %>%
  group_by(depth) %>%
  summarise(
    RMSE = rmse(temperature, Temperature),  # Calculate RMSE
    R2 = cor(temperature, Temperature)^2    # Calculate R^2 (square of the correlation)
  )

# Merge the calculated metrics with the data for plotting
mod_filtered <- mod_filtered %>%
  left_join(metrics, by = "depth")

# Plot the data with RMSE and R^2 annotations
p1 <- ggplot() +
  geom_line(data = mod_filtered, aes(x = doy, y = temperature, group = depth, color = "Modeled"), linetype = "solid", linewidth =1.5) +
  geom_point(data = obs_filtered, aes(x = doy, y = Temperature, group = depth, color = "Buoy"), shape = 16, size = 1) +
  scale_x_continuous(
    breaks = c(152, 182, 213, 244, 274, 305, 335),  # Day of year values for Jun 1 to Dec 1
    labels = c("Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")  # Custom labels
  ) +
  facet_wrap(~depth, scales = "free_y", ncol = 1) +  # Facet by depth
  theme_classic(base_size = 16) +
  labs(x = "Date", y = "Temperature (°C)", color = "Data Source", title = "a") +
  scale_color_manual(values = c("Buoy" = "black", "Modeled" = "#85A1EF")) +  # Custom colors
  theme(strip.text = element_blank(),  # Remove titles from the facet panes
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(
    breaks = seq(0, 30, by = 10),
    limits = c(0, 30)
  ) +
  # Add depth text inside the pane
  geom_text(data = mod_filtered, aes(x = min(doy), y = Inf, label = paste(depth, "m")), 
            inherit.aes = FALSE, hjust = -0.1, vjust = 1.2, color = "black", size = 7) +
  # Add RMSE and R^2 text annotations inside the facet panes
  geom_text(data = metrics, aes(x = 325, y = 15, label = paste("rmse: ", round(RMSE, 2), "\nR^2: ", round(R2, 2))),
            inherit.aes = FALSE, hjust = 0, vjust = 0, color = "black", size = 7)

p1 

ggsave("Data/Figures/comments/2015_temp_validation_V2.jpg", p1, dpi = 300 , width = 10, height = 6, units = "in")
