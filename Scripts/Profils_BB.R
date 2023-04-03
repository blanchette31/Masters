##TEMPERATURE, OXYGEN PROFILE AND LIGHT ATTENUATION
##BETWEEN OCTOBER 2011 AND 2012

#Nettoyer l'espace de travail
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

# Préparer les données
profil = read.csv("Data/Processed/lake/temp_2021.csv")
profil$Date <- as.Date(profil$Date)
profil$Date_dec <- decimal_date(profil$Date)
profil$Temperature <- as.numeric(as.character(profil$Temperature))

#Interpolations de la température
mba.temp <- mba.surf(profil[,c('Date_dec','Depth','Temperature')],100, 100)
dimnames(mba.temp$xyz.est$z) <- list(mba.temp$xyz.est$x, mba.temp$xyz.est$y)
df1 <- melt(mba.temp$xyz.est$z, varnames = c('Date_dec','Depth'), value.name = 'Temperature') 

#Graphiques
p1 = ggplot(df1, aes(x = Date_dec, y = Depth)) +
  geom_raster(aes(fill = Temperature), interpolate = F, hjust = 0.5, vjust = 0.5) +
  geom_contour(aes(z = Temperature)) +
  geom_point(data = profil, aes(Date_dec, Depth), colour = 'white') +
  scale_y_reverse() +
  scale_fill_gradientn(colours = matlab.like2(10))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(y = 'depth (m)', fill = 'temperature (°C)') 
p1
ggsave("Data/export/profil_2021_test.png", p1, width = 5, units = "in", dpi = 300)

