#### interpolated DO for all years ####

## prepare workspace ##
rm(list = ls())

## libraries

library(tidyverse)

## load data

df <- read.csv("Data//Processed//boue//delta_co2-2.csv", header = TRUE)

df$depth_0.5 <- df$do_mg_l
df$depth_5.5 <- df$do_mg_l_meta
df$depth_8.5 <- df$do_mg_l_hypo
interpolation_depths <- seq(0.5,8.5, by = 0.5)

# Columns for DO (dissolved oxygen) data
do_columns <- c("depth_0.5", "depth_5.5", "depth_8.5")

# Identify and remove rows with NAs only in the specified DO columns
df_filt <- df[complete.cases(df[do_columns]), ]


# Function to interpolate DO concentrations for each depth

interpolate_dissolved_oxygen <- function(data, depth) {
  interpolated_values <- approx(x = df$timestamp, y = df[[paste0("depth_", depth, "m")]], xout = data$timestamp)$y
  return(interpolated_values)
}

# Create a data frame with interpolated values for all depths
interpolated_data <- data.frame(timestamp = timestamp)
for (depth in interpolation_depths) {
  interpolated_data[paste0("depth_", depth, "m")] <- interpolate_dissolved_oxygen(df, depth)
}
