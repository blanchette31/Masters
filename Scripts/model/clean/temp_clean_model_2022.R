# prepare workspace
rm(list = ls())

# load libraries
library(tidyverse)


# load data
df <- read.csv("Data//Processed//lake//temp_2022.csv")

# Extract the year
df$year <- year(df$Date)

# Extract the month
df$month <- month(df$Date)

# Extract the day
df$day <- day(df$Date)

# Change the depth value for the specified ID
df$Depth[df$id == "TChain_Temp_2_Deg_C_Smp_0.65m"] <- 0.651

df <- df[, !colnames(df) %in% "id"]

# Reshape the data frame
df_reshaped <- df %>%
  pivot_wider(names_from = Depth, values_from = Temperature, values_fill = NA)

# Get the column names with numbers or decimals
num_cols <- grep("\\d+(\\.\\d+)?", colnames(df_reshaped), value = TRUE)

# Generate the new column names
new_col_names <- sub("(\\d+(\\.\\d+)?)", "\\1m", num_cols)

# Rename the columns
colnames(df_reshaped)[colnames(df_reshaped) %in% num_cols] <- new_col_names

colnames(df_reshaped)[colnames(df_reshaped) == "0.65m"] <- "0.65A_m"

colnames(df_reshaped)[colnames(df_reshaped) == "0.651m"] <- "0.65B_m"

df_reshaped <- df_reshaped[, !colnames(df_reshaped) %in% "Date"]

# Reorder the columns
df_reshaped <- df_reshaped[, c("year", "month", "day", "airP_hPa", "0.65A_m", "0.65B_m", "0.73m", "1.73m", "2.73m", "3.73m", "4.58m", "5.58m", "6.53m", "7.53m", "8.53m", "10.53m")]


#format column width for text output 
df_reshaped[] <- lapply(df_reshaped, function(x) format(x, width = 0))

#export dataframe
write_delim(df_reshaped, "Data//model//inputs//b1//temperature//obs_temp_2022_wide.txt", quote = "none", delim = "\t")
