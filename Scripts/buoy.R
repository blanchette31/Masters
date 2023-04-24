# Packages
library(dplyr)
library(readr)


#load dataframes
df = list.files(path = "Data//Raw//boue//Croche2016-2021", pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%
  bind_rows


as.data.frame(df)




df$timestamp <- as.POSIXct(df$timestamp, format = "%Y-%m-%d%H:%M:%S", tz = "EST")

df_2022 <- read.csv("Data//Raw//boue//2022CR6_QAQC3.csv", header = TRUE)

df_2022$timestamp <- as.POSIXct(df_2022$timestamp, format = "%Y-%m-%d%H:%M:%S", tz = "EST")

df_2015 <- read.csv("Data//Raw//boue//Crochefinal2015.csv", header = TRUE)

df_2015$timestamp <- as.POSIXct(df_2015$timestamp, format = "%Y-%m-%d%H:%M", tz = "EST")

#Extract date, year and doy from timestamp
df = df %>%
  mutate(
    date = as.Date(timestamp, format = "%Y%m%d"),
    doy = strftime(timestamp, format = "%j"),
    year = strftime(timestamp, format = "%Y"))

df_2015 = df_2015 %>%
  mutate(
    date = as.Date(timestamp, format = "%Y%m%d"),
    doy = strftime(timestamp, format = "%j"),
    year = strftime(timestamp, format = "%Y"))

df_2022 = df_2022 %>%
  mutate(
    date = as.Date(timestamp, format = "%Y%m%d"),
    doy = strftime(timestamp, format = "%j"),
    year = strftime(timestamp, format = "%Y"))

#ADD NOAA dataframe

noaa = read.csv("Data//Raw//climate//noaa_yearly_mean.csv", header = TRUE)

#change column names 
colnames(noaa)[2:3] = c("co2_atm", "co2_atm_unc")

noaa_2015 <- subset(noaa, year == 2015)
noaa_2022 <- subset(noaa, year == 2022)

#keep only years with data for buoy
years_to_keep = c(2016, 2017, 2018, 2019, 2021)

noaa_1621 <-  noaa[noaa$year %in% years_to_keep, ]
# Merge dataframes by year

df <- merge(df, noaa_1621, by = "year")
df_2015 <- merge(df_2015, noaa_2015, by = "year")
df_2022 <- merge(df_2022, noaa_2022, by = "year")




write.csv(df, "Data//Processed//boue//boue_2016-2021.csv", row.names = TRUE)
write.csv(df_2015, "Data//Processed//boue//boue_2015.csv", row.names = TRUE)
write.csv(df_2022, "Data//Processed//boue//boue_2022.csv", row.names = TRUE)



