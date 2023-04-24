

library(tidyverse)
library(here)

# Set the working directory to the folder containing the CSV files
folder = here("test")

# Function to extract coordinates from filename
get_coordinates <- function(filename) {
  coords <- str_extract_all(filename, "[0-9.]+")
  lat <- as.numeric(coords[[1]][1])
  lon <- as.numeric(coords[[1]][2])
  return(list(lat = lat, lon = lon))
}

# Get a list of all CSV files in the directory
csv_files <- list.files(path = folder, pattern = "\\.csv$")

# Loop through each file, read and process the data
for (file in csv_files) {
  # Read the CSV file
  df <- read.csv(file)
  
  # Get the coordinates from the filename
  coordinates <- get_coordinates(file)
  lat <- coordinates$lat/10
  lon <- coordinates$lon/10
  
  # Add the coordinates as new columns in the dataframe
  df$latitude <- lat
  df$longitude <- lon
  
  # Write the updated dataframe back to a new CSV file
  write.csv(df, file)
}
