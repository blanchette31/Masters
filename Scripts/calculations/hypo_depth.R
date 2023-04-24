# Load necessary packages
library(dplyr)

# Load your temperature data (assuming it's in a data frame called 'temp_data')
# It should have columns for depth, temperature, and year.
# Sample data structure: depth (meters), temperature (°C), year
# temp_data <- read.csv("temperature_data.csv")

# Load your bathymetry data (assuming it's in a data frame called 'bathymetry_data')
# It should have columns for depth and area of each depth layer.
# Sample data structure: depth (meters), area (square meters)
# bathymetry_data <- read.csv("bathymetry_data.csv")

# Define the temperature change criterion (1 degree Celsius per meter)
temperature_change_criterion <- 1

# Function to estimate thermocline depth for each year
estimate_thermocline_depth <- function(year) {
  year_data <- temp_data %>% filter(year == year) %>% arrange(depth)
  depth_values <- year_data$depth
  temp_values <- year_data$temperature
  
  # Calculate temperature gradient (change per meter)
  temp_gradient <- c(NA, diff(temp_values) / diff(depth_values))
  
  # Find the depth at which the temperature change is less than the criterion
  thermocline_depth <- min(depth_values[temp_gradient < temperature_change_criterion])
  
  return(thermocline_depth)
}

# Calculate the thermocline depth for each year
start_year <- 2015
end_year <- 2022
thermocline_depth_data <- data.frame(year = start_year:end_year)
thermocline_depth_data$thermocline_depth <- sapply(thermocline_depth_data$year, estimate_thermocline_depth)

# Optionally, you can save the results to a CSV file
write.csv(thermocline_depth_data, "thermocline_depth.csv", row.names = FALSE)
