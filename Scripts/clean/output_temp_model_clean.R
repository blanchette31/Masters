## prepare workspace ## 

rm(list = ls())

## load libraries 
library(tidyverse)

# load data
temp_init <- read.csv("model//running_CRO//3_output//temperature_data_model_combined.csv", header = TRUE)


temp_long <- pivot_longer(temp_init,
                          cols = -depth,
                          names_to = "date",
                          values_to = "temperature",
                          cols_vary = "slowest")

temp_long$date <- as.character(temp_long$date)

temp_long$date <- gsub("X", "", temp_long$date)

temp_long$date <- as.Date(temp_long$date, format = "%Y.%m.%d", tz = "EST")


temp_wide <- temp_long %>% 
  pivot_wider(names_from = depth,
              values_from = temperature)

colnames(temp_wide)[-1] <- paste0(colnames(temp_wide)[-1], "m")


write.csv(temp_wide, "Data//model//results//temp_all_years.csv")
