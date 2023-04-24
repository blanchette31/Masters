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
profil <- read.csv("Data/Processed/lake/temp_2015.csv")
profil$Date <- as.Date(profil$Date)
  profil$Date_dec <- decimal_date(profil$Date)
  profil$Temperature <- as.numeric(as.character(profil$Temperature))
  
  
  #Interpolations de la température
  mba.temp <- mba.surf(profil[,c('Date_dec','Depth','Temperature')],100, 100)
  dimnames(mba.temp$xyz.est$z) <- list(mba.temp$xyz.est$x, mba.temp$xyz.est$y)
  df1 <- reshape2::melt(mba.temp$xyz.est$z, varnames = c('Date_dec','Depth'), value.name = 'Temperature') 
  

  
df1$Date <- as.Date(format(date_decimal(df1$Date_dec),"%Y-%m-%d"))
df1$Date <- df1$Date + days(1)
  
profil[which.min(profil$Date),]
df1[which.min(df1$Date),]

  
  # Filter the dataframe to show only rows for June 15th, 2015
  may_14_2015_data <- profil[profil$Date == as.Date("2015-05-14"), ]
  
  # Print the filtered dataframe
  print(may_14_2015_data)
  
  
  
  
  intermediate_depths <- seq(2.5, 10.5, by = 1)
  
  # Interpolate temperature at intermediate depths over daily intervals
  interpolated_data <- profil %>%
    group_by(Date) %>%
    do(data.frame(Depth = intermediate_depths,
                  Temperature = (approx(.$Depth, .$Temperature, xout = intermediate_depths)$y))) %>%
    ungroup()
  
  # Print the interpolated data
view(interpolated_data)
  
  
  extrapolation_depths <- c(0, 0.5, 1.5)
  spline_fit <- smooth.spline(profil$Depth, profil$Temperature)
  
  
  extrapolation_data <- profil %>%
    group_by(Date) %>%
    do(data.frame(Depth = c(profil$Depth, extrapolation_depths),
                  Temperature = predict(spline_fit, xout = extrapolation_depths)$y)) %>%
    ungroup()
  
  # Print the interpolated data
  print(extrapolation_data)

  