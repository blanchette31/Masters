# Prepare workspace 
rm(list = ls())

# Load libraries
library(tidyverse)
library(lubridate)

# Load data
df <- read.csv("Data/Processed/lake/temp_2015.csv")
head(df)

# Extract depth from id and rename temperature column
df <- df %>%
  mutate(
    Depth = gsub(".*_(\\d+\\.\\d+)m.*", "\\1m", id),  # Extract depth
    Date = as.Date(Date),
    year = year(Date),
    month = month(Date),
    day = day(Date),
    Temperature = round(Temperature, 4),
    airP_hPa = round(airP_hPa, 3)
  ) %>%
  select(year, month, day, airP_hPa, Depth, Temperature)

# Reshape from long to wide
df_wide <- df %>%
  pivot_wider(names_from = Depth, values_from = Temperature)

# View result
print(df_wide)

# Save output
write.table(df_wide, "info/model/running_CRO/CRO_obstemp_daily_alldepths_2015.txt",
            sep = "\t", row.names = FALSE, quote = FALSE)
