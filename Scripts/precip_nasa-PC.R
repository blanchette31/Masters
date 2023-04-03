# Libraries
library(plyr)
library(dplyr)
library(readr)
library(lubridate) #mutate year from date

rm(list = ls())

df = list.files("Data//Raw//precip//NASA//fixed//", pattern = glob2rx("ts*.txt"), full.names = TRUE) %>%
  lapply(read_delim) %>%
  rbind.fill()

write.csv(df, "Data//Raw//precip//NASA//fixed//nasa_raw.csv")

#select columns 
df = df[, c(7,8,13)]

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
  group_by(year, doy) %>% 
  summarise(rain_nasa = sum(rain_nasa))



#write csv 

write.csv(nasad, "Data//Processed//precip//precip_nasa_bound.csv", row.names = TRUE)

