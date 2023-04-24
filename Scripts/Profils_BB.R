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
library(cowplot)


pheno = read.csv("Data//Processed//phenocam//pheno_clean.csv", header = TRUE)
pheno$Couleurs <-  as.Date(pheno$Couleurs)
pheno$Perte <- as.Date(pheno$Perte)
pheno$col_dec <-  decimal_date(pheno$Couleurs)
pheno$perte_dec <- decimal_date(pheno$Perte)
thermo <- read.csv("Data/Processed/lake/thermo_depth.csv")
thermo$date <- as.Date(thermo$datetime)
cut  <- read.csv("Data/Processed/precip/era5_cut_buoy.csv")

# Filter thermo to keep only dates present in profil
thermo <- thermo[thermo$date %in% cut$date,]




#filter for years with buoy data
pheno <- pheno[pheno$year %in% c(2015:2019,2021, 2022),]
# For the whole summer

# Define a list of years to process
years <- c(2015, 2016, 2017, 2018, 2019, 2021,2022)

plot_list = list()
plot_index = 1

for (year in years) {
  
  # Préparer les données
  profil <- read.csv(paste0("Data/Processed/lake/temp_", year, ".csv"))
  profil$Date <- as.Date(profil$Date)
  profil$Date_dec <- decimal_date(profil$Date)
  profil$Temperature <- as.numeric(as.character(profil$Temperature))

  
  
  #Interpolations de la température
  mba.temp <- mba.surf(profil[,c('Date_dec','Depth','Temperature')],100, 100)
  dimnames(mba.temp$xyz.est$z) <- list(mba.temp$xyz.est$x, mba.temp$xyz.est$y)
  df1 <- reshape2::melt(mba.temp$xyz.est$z, varnames = c('Date_dec','Depth'), value.name = 'Temperature') 
  # Subset thermo for the given year
  thermo_year <- thermo[year(thermo$date) == year,]  
  

  
  # Add custom x-axis tick marks
  tick_dates <- ymd(paste0(year, c("-05-01", "-06-01", "-07-01", "-08-01", "-09-01", "-10-01", "-11-01")))
  tick_labels <- c("May", "June", "July", "Aug", "Sept", "Oct", "Nov")
  tick_positions <- decimal_date(tick_dates)

  pheno_year <- pheno[pheno$year == year,]
  #Graphiques
  p1 <- ggplot(df1, aes(x = Date_dec, y = Depth)) +
    geom_raster(aes(fill = Temperature), interpolate = F, hjust = 0.5, vjust = 0.5) +
    geom_contour(aes(z = Temperature)) +
    geom_point(data = profil, aes(Date_dec, Depth), colour = 'white') +
    geom_line(data = thermo_year, aes(x = decimal_date(date), y = thermo.depth), color = "black", linewidth = 1.2) +
    # scale_y_reverse(breaks = c(0, 2.5, 5, 7.5, 10)) +
    scale_y_continuous(trans = "reverse", breaks = c(0, 2.5, 5, 7.5, 10), limits = c(10.5, 0), expand = c(0,0)) +
    scale_fill_gradientn(colours = matlab.like2(10), limits = c(0, 27))+
    labs(y = 'Depth (m)', fill = 'Temp (°C)', x = "") +
    scale_x_continuous(breaks = tick_positions, labels = tick_labels, expand = c(0,0))+  
    theme_bw() +  
    ggtitle(paste0(year))+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 16),
          plot.title = element_text(size = 18))

  
  # Add vertical lines for start and end of leaf fall
  if (nrow(pheno_year) > 0) {
    p1 <- p1 + geom_vline(data = pheno_year, aes(xintercept = decimal_date(Couleurs)), linetype = "dashed", colour = "black", linewidth = 1.5) +
      geom_vline(data = pheno_year, aes(xintercept = decimal_date(Perte)), linetype = "dashed", colour = "black", linewidth = 1.5)
  }
  # Save the plot for the current year
  ggsave(paste0("Data/export/profil_", year, "_all.png"), p1, width = 12, height = 7, units = "in", dpi = 300)
  
  # Add the plot to the list
  plot_list[[length(plot_list) + 1]] <- p1
  
}
# Arrange the plots in a grid
ps <- grid.arrange(grobs = plot_list, ncol = 3) 
# 
# # For fall only
# 
# # Define a list of years to process
# years <- c(2015,2016, 2017, 2018, 2019, 2021)
# 
# for (year in years) {
#   
#   # Préparer les données
#   profil <- read.csv(paste0("Data/Processed/lake/temp_", year, ".csv"))
#   profil$Date <- as.Date(profil$Date)
#   profil$Date_dec <- decimal_date(profil$Date)
#   profil$Temperature <- as.numeric(as.character(profil$Temperature))
#   
#   
#   # Select data after august 18th 
#   start_date <- ymd(paste0(year, "-08-18"))
#   profil <- profil %>% filter(Date >= start_date)
#   
#   #Interpolations de la température
#   mba.temp <- mba.surf(profil[,c('Date_dec','Depth','Temperature')],100, 100)
#   dimnames(mba.temp$xyz.est$z) <- list(mba.temp$xyz.est$x, mba.temp$xyz.est$y)
#   df1 <- reshape2::melt(mba.temp$xyz.est$z, varnames = c('Date_dec','Depth'), value.name = 'Temperature') 
#   
#   # Add custom x-axis tick marks
#   tick_dates <- ymd(paste0(year, c("-08-18","-08-25","-09-01", "-09-08", "-09-15", "-09-22", "-10-01", "-10-08", "-10-15", "-10-22", "-11-01", "-11-08", "-11-15", "-11-22", "-11-29")))
#   tick_labels <- c(rep("", 2),"Sept", rep("", 3), "Oct", rep("", 3), "Nov", rep("", 4))
#   tick_positions <- decimal_date(tick_dates)
# 
#   #Subset pheno for the given year
#   pheno_year <- pheno[pheno$year == year,]  
#   
#   #Graphiques
#   p1 <- ggplot(df1, aes(x = Date_dec, y = Depth)) +
#     geom_raster(aes(fill = Temperature), interpolate = F, hjust = 0.5, vjust = 0.5) +
#     geom_contour(aes(z = Temperature)) +
#     geom_point(data = profil, aes(Date_dec, Depth), colour = 'white') +
#     scale_y_reverse() +
#     scale_fill_gradientn(colours = matlab.like2(10), limits = c(0, 27))+
#     labs(y = 'Depth (m)', fill = 'Temp (°C)', x = "") +
#     scale_x_continuous(breaks = tick_positions, labels = tick_labels)+  
#     theme_bw() +  
#     theme(panel.grid.major = element_blank(),
#           panel.grid.minor = element_blank(),) +
#     ggtitle(paste0("Temperature profile for the fall season of ", year))
#   
#   # Add vertical lines for start and end of leaf fall
#   if (nrow(pheno_year) > 0) {
#     p1 <- p1 + geom_vline(data = pheno_year, aes(xintercept = decimal_date(Couleurs)), linetype = "dashed", colour = "black", linewidth = 2) +
#       geom_vline(data = pheno_year, aes(xintercept = decimal_date(Perte)), linetype = "dashed", colour = "black")
# 
#   }
#   
#   # Save the plot for the current year
#   ggsave(paste0("Data/export/profil_", year, "_fall.png"), p1, width = 12, height = 7, units = "in", dpi = 300)
# }
