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


profil <- read.csv("Data/Processed/lake/temp_2022.csv")
profil$Date <- as.Date(profil$Date)
profil$Date_dec <- decimal_date(profil$Date)
profil$Temperature <- as.numeric(as.character(profil$Temperature))


#Interpolations de la température
mba.temp <- mba.surf(profil[,c('Date_dec','Depth','Temperature')],100, 100)
dimnames(mba.temp$xyz.est$z) <- list(mba.temp$xyz.est$x, mba.temp$xyz.est$y)
df1 <- reshape2::melt(mba.temp$xyz.est$z, varnames = c('Date_dec','Depth'), value.name = 'Temperature') 

# Add custom x-axis tick marks
tick_dates <- ymd(profil$year, c("-05-01", "-06-01", "-07-01", "-08-01", "-09-01", "-10-01", "-11-01"))
tick_labels <- c("May", "June", "July", "Aug", "Sept", "Oct", "Nov")
tick_positions <- decimal_date(tick_dates)


#Graphiques
p1 <- ggplot(df1, aes(x = Date_dec, y = Depth)) +
  geom_raster(aes(fill = Temperature), interpolate = F, hjust = 0.5, vjust = 0.5) +
  geom_contour(aes(z = Temperature)) +
  geom_point(data = profil, aes(Date_dec, Depth), colour = 'white') +
  scale_y_reverse() +
  scale_fill_gradientn(colours = matlab.like2(10), limits = c(0, 27))+
  labs(y = 'Depth (m)', fill = 'Temp (°C)', x = "") +
  scale_x_continuous(breaks = tick_positions, labels = tick_labels)+  
  theme_bw() +  
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle("Temperature profile for all of 2022")

# Save the plot for the current year
ggsave("Data/export/profil_2022.png"), p1, width = 12, height = 7, units = "in", dpi = 300)
}