# Create an empty dataframe to store the selected columns
combined_data <- data.frame()

# List the years for the dataframes
years <- c(2015:2019, 2021:2022)

# Loop through the years
for (year in years) {
  # Construct the name of the dataframe
  dataframe_name <- paste0("df_", year)
  
  # Check if the dataframe exists
  if (exists(dataframe_name)) {
    # Select the specific columns from the dataframe
    selected_columns <- get(dataframe_name)[c("timestamp", "airT_oC", "airP_hPa", "windspeedavg_m_per_s", "RH_perc")]
    
    # Bind the selected columns to the combined dataframe
    combined_data <- rbind(combined_data, selected_columns)
  }
}

# Print the combined dataframe
print(combined_data)

combined_data$year = year(combined_data$timestamp)

