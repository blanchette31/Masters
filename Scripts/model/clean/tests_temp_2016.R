# Prepare workspace 
rm(list = ls())


#Ouvrir les librairies
library("ggplot2")
library("lubridate")
library("MBA")
library("reshape2")
library("colorRamps")
library("gridExtra")
library(tidyr)
library(dplyr)
library(tidyverse)
library(gridExtra)



# Préparer les données
profil <- read.csv("Data/Processed/lake/temp_2016.csv")
profil$Date <- as.Date(profil$Date)
profil$Date_dec <- decimal_date(profil$Date)
profil$Temperature <- as.numeric(as.character(profil$Temperature))


#Interpolations de la température
mba.temp <- mba.surf(profil[,c('Date_dec','Depth','Temperature')],100, 100)
dimnames(mba.temp$xyz.est$z) <- list(mba.temp$xyz.est$x, mba.temp$xyz.est$y)
df1 <- reshape2::melt(mba.temp$xyz.est$z, varnames = c('Date_dec','Depth'), value.name = 'Temperature') 

df1$Date_dec <- as.numeric(as.character(df1$Date_dec))

merged <- merge(df1, profil[, c("Date_dec", "Date")], by = "Date_dec", all.x = TRUE)



# Filter the dataframe to show only rows for June 15th, 2016
june_15_2016_data <- profil[profil$Date == as.Date("2016-05-15"), ]

# Print the filtered dataframe
print(june_15_2016_data)


