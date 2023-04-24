# Packages
rm(list = ls())
library(dplyr)
library(readr)


#load dataframe
df = list.files(path = "Data//Raw//precip//Saint-Hippolyte", pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%
  bind_rows

as.data.frame(df)
df$Year = as.factor(df$Year)
split(df, f = df$Year)

# DOY
#set columns as dates 
df$date = as.Date(with(df, paste(Year, Month, Day, sep = "-")), "%Y-%m-%d")

df$doy = as.numeric(strftime(df$date, "%j"))
df$doy

df_clean = df %>% select(date, Year, Month, Day, `Total Rain (mm)`,doy) %>% 
  rename(rain = `Total Rain (mm)`)

df_clean <- na.omit(df_clean)



write.csv(df_clean, "Data//Processed//precip//precip_2011-2023.csv", row.names = TRUE)


