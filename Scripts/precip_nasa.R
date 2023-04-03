# Libraries
library(plyr)
library(dplyr)
library(readr)
library(lubridate) #mutate year from date

rm(list = ls())

ts_11 = read.table("Data//Raw//precip//NASA//fixed//ts_2011.txt", row.names = NULL)
ts_12 = read.table("Data//Raw//precip//NASA//fixed//ts_2012.txt", row.names = NULL)
ts_13 = read.table("Data//Raw//precip//NASA//fixed//ts_2013.txt", row.names = NULL)
ts_14 = read.table("Data//Raw//precip//NASA//fixed//ts_2014.txt", row.names = NULL)
ts_15 = read.table("Data//Raw//precip//NASA//fixed//ts_2015.txt", row.names = NULL)
ts_16 = read.table("Data//Raw//precip//NASA//fixed//ts_2016.txt", row.names = NULL)
ts_17 = read.table("Data//Raw//precip//NASA//fixed//ts_2017.txt", row.names = NULL)
ts_18 = read.table("Data//Raw//precip//NASA//fixed//ts_2018.txt", row.names = NULL)
ts_19 = read.table("Data//Raw//precip//NASA//fixed//ts_2019.txt", row.names = NULL)
ts_20 = read.table("Data//Raw//precip//NASA//fixed//ts_2020.txt", row.names = NULL)
ts_21 = read.table("Data//Raw//precip//NASA//fixed//ts_2021.txt", row.names = NULL)


#Put dataframes into a list 
df_list = mget(ls(pattern = "ts_[11-21]"))

df = df_list %>%
  rbind.fill()

#rename columns
colnames(df)[1] = "Date"
colnames(df)[2] = "Time"
colnames(df)[3] = "rain_nasa"

#set columns as dates 
df$Date = as.Date(with(df, paste(Date, sep = "-")), "%Y-%m-%d")

df$doy = as.numeric(strftime(df$Date, "%j"))
df$doy

# extract year from date 
df$year = lubridate:: year(df$Date)
df$year = as.factor(df$year)
df$doy = as.factor(df$doy)

#Calculate daily precipitation for nasa
df$rain_nasa <- as.numeric(df$rain_nasa)
nasad <- df %>% 
  dplyr::group_by(year, doy) %>% 
  dplyr::summarise(rain_nasa = sum(rain_nasa))


#write csv 

write.csv(nasad, "Data//Processed//precip//precip_nasa_bound.csv", row.names = TRUE)

