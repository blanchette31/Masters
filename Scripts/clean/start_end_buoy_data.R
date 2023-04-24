## prepare workspace

rm(list = ls())

# Get a list of files in the folder
file_list <- list.files(path = "Data/Processed/lake", pattern = "temp_\\d{4}\\.csv", full.names = TRUE)

# Loop through each file, read it, and create a dataframe
for (file_path in file_list) {
  # Extract the year from the file name
  year <- gsub("temp_(\\d{4})\\.csv", "\\1", basename(file_path))
  
  # Read the CSV file into a dataframe
  df_name <- paste("df_", year, sep = "")
  assign(df_name, read.csv(file_path))
}
# Now, df_list contains individual dataframes named df_XXXX
# You can access them using df_list$df_XXXX where XXXX is the year.


# Loop through all dataframes in the environment
for (df_name in ls(pattern = "df_")) {
  # Get the dataframe
  current_df <- get(df_name)
  
  # Convert the "Date" column to a Date object
  current_df$Date <- as.Date(current_df$Date, format = "%Y-%m-%d")
  
  # Calculate the day of the year and add a new column
  current_df$doy <- format(current_df$Date, "%j")
  
  # Print the updated dataframe
  print(current_df)
  
  # If you want to save the updated dataframe back to the environment
  assign(df_name, current_df)
}

# Create an empty dataframe to store min and max values
summary_df <- data.frame()

# Loop through all dataframes in the environment
for (df_name in ls(pattern = "df_")) {
  # Get the dataframe
  current_df <- get(df_name)
  
  # Convert "doy" to numeric if it's not already
  current_df$doy <- as.numeric(current_df$doy)
  
  # Calculate min and max day of the year values
  min_doy <- min(current_df$doy, na.rm = TRUE)
  max_doy <- max(current_df$doy, na.rm = TRUE)
  
  # Create a new row for the summary dataframe
  summary_row <- data.frame(
    df = df_name,
    firstdoy = min_doy,
    lastdoy = max_doy
  )
  
  # Append the summary row to the summary dataframe
  summary_df <- rbind(summary_df, summary_row)
}

# Print the summary dataframe
print(summary_df)


