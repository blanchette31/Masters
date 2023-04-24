#This script is used to prepare the observed temperature data in lake Croche to calibrate MYLake

# Input files are cleaned temperature files from clean_temperature.R and output files are obs_temp_year_wide.txt in the inputs folder of model+



# Prepare workspace
rm(list = ls())

# Libraries
library(tidyverse)

## Read data ##

# Get a list of all CSV files in the folder
csv_files <- list.files(path = "Data//Processed//lake", pattern = "temp_.*\\.csv", full.names = TRUE)

# Filter and process files with the desired pattern
for (file in csv_files) {
  # Check if the file name matches the pattern "temp_year.csv"
  if (grepl("^temp_\\d+\\.csv$", basename(file))) {
    # Extract the year from the file name
    year <- gsub("^temp_(\\d+)\\.csv$", "\\1", basename(file))
    # Read the CSV file into a data frame
    df <- read.csv(file)
    # Assign a name to the data frame
    df_name <- paste0("df_", year)
    # Assign the data frame to an individual object with the assigned name
    assign(df_name, df)
  }
}

## extract year month day from date ##
## change the depth value of T02 for simplicity ##

years <- c(2015, 2016, 2017, 2018, 2019, 2021)  # Update with your specific years

# Loop through each year and apply the date extraction code
for (year in years) {
  # Get the data frame for the current year
  df <- get(paste0("df_", year))
  
  # Extract the year
  df$year <- year(df$Date)
  
  # Extract the month
  df$month <- month(df$Date)
  
  # Extract the day
  df$day <- day(df$Date)
  
  # Update the data frame in the environment
  assign(paste0("df_", year), df)
}

# Define the ID for which you want to change the depth value
id_to_change <- "waterT02_0.5m_oC"

for (year in c(2016:2019, 2021)) {
 
 
    # Get the data frame for the current year
    df_year <- get(paste0("df_", year))
    
    # Change the depth value for the specified ID
    df_year$Depth[df_year$id == id_to_change] <- 0.51
    
    # Assign the modified data frame back to its original name
    assign(paste0("df_", year), df_year)
  
}

## Long to wide dataframes ## 

# Loop through each year and reshape the data frame
for (year in years) {
  # Get the data frame for the current year
  df_name <- paste0("df_", year)
  df <- get(df_name)
  
  # Remove the id column, if present
  if ("id" %in% colnames(df)) {
    df <- df[, !colnames(df) %in% "id"]
  }
  
  # Reshape the data frame
  df_reshaped <- df %>%
    pivot_wider(names_from = Depth, values_from = Temperature, values_fill = NA)
  
  # Assign the reshaped data frame with a new name
  df_name_wide <- paste0(df_name, "_wide")
  assign(df_name_wide, df_reshaped)
}

## rename columns of wide dataframe example : 0.5 -> 0.5m ##

# Loop through each year
for (year in years) {
  # Get the data frame for the current year
  df_name <- paste0("df_", year, "_wide")
  df <- get(df_name)
  
  # Get the column names with numbers or decimals
  num_cols <- grep("\\d+(\\.\\d+)?", colnames(df), value = TRUE)
  
  # Generate the new column names
  new_col_names <- sub("(\\d+(\\.\\d+)?)", "\\1m", num_cols)
  
  # Rename the columns
  colnames(df)[colnames(df) %in% num_cols] <- new_col_names
  
  # Assign the modified data frame back to its original name
  assign(df_name, df)
}

## Rename columns 0.5 and 0.51 to 0.5A_m and 0.5B_m ##

# Loop through each year
for (year in years) {
  # Get the data frame for the current year
  df_name <- paste0("df_", year, "_wide")
  df <- get(df_name)
  
  # Check if the columns "0.5m" and "0.51m" exist
  if ("0.5m" %in% colnames(df)) {
    # Rename "0.5m" to "0.5_Am"
    colnames(df)[colnames(df) == "0.5m"] <- "0.5A_m"
  }
  if ("0.51m" %in% colnames(df)) {
    # Rename "0.51m" to "0.5B_m"
    colnames(df)[colnames(df) == "0.51m"] <- "0.5B_m"
  }
  
  # Assign the modified data frame back to its original name
  assign(df_name, df)
}


## Remove Date column from final dataframes and reorder columns ## 

# Loop through each year
for (year in years) {
  # Get the data frame for the current year
  df_name <- paste0("df_", year, "_wide")
  df <- get(df_name)
  
  # Remove the "date" column, if present
  if ("Date" %in% colnames(df)) {
    df <- df[, !colnames(df) %in% "Date"]
  }
  
  # Reorder the columns
  df <- df[, c("year", "month", "day", "airP_hPa", setdiff(colnames(df), c("year", "month", "day", "airP_hPa")))]
  
  # Assign the modified data frame back to its original name
  assign(df_name, df)
}

## Round columns to an appropriate amount of decimal places ## 

# Loop through each year
for (year in years) {
  # Get the data frame for the current year
  df_name <- paste0("df_", year, "_wide")
  df <- get(df_name)
  
  # Identify columns containing numbers in the name
  num_cols <- grep("\\d", colnames(df), value = TRUE)
  
  # Round the values in the identified columns to 4 decimal places
  df[, num_cols] <- round(df[, num_cols], 4)
  
  # Round the values of "airP_hPa" column to 3 decimal places
  df$airP_hPa <- round(df$airP_hPa, 3)
  
  # Assign the modified data frame back to its original name
  assign(df_name, df)
}

#### Export final dataframes ####

## 2015 ##

# Rename 12m column from 2015 to 11.4 since it is the max depth # 

df_2015_wide <- rename(df_2015_wide, "11.4m" = "12m")

#format column width for text output 
df_2015_wide[] <- lapply(df_2015_wide, function(x) format(x, width = 0))

#export dataframe
write_delim(df_2015_wide, "Data//model//inputs//b1//temperature//obs_temp_2015_wide.txt", quote = "none", delim = "\t")

## 2016 - 2021 ##

# Combine all the dataframes
df_2016_2021_wide <- rbind(df_2016_wide, df_2017_wide, df_2018_wide, df_2019_wide, df_2021_wide)

#format column width for text output
df_2016_2021_wide[] <- lapply(df_2016_2021_wide, function(x) format(x, width = 0))

#export dataframe
write_delim(df_2016_2021_wide, "Data//model//inputs//b1//temperature//obs_temp_2016_2021_wide.txt", quote = "none", delim = "\t")

## 2016 - 2019 ##

# Combine all the dataframes
df_2016_2019_wide <- rbind(df_2016_wide, df_2017_wide, df_2018_wide, df_2019_wide)

# format column width
df_2016_2019_wide[] <- lapply(df_2016_2019_wide, function(x) format(x, width = 0))

# export dataframe
write_delim(df_2016_2019_wide, "Data//model//inputs//b1//temperature//obs_temp_2016_2019_wide.txt", quote = "none", delim = "\t")
