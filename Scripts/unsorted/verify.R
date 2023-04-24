# Prepare workspace 
rm(list = ls())

# Load libraries
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

pheno <- read.csv("Data//Processed//phenocam//pheno_clean.csv", header = TRUE)
pheno$Couleurs <- as.Date(pheno$Couleurs)
pheno$Perte <- as.Date(pheno$Perte)
pheno$col_dec <- decimal_date(pheno$Couleurs)
pheno$perte_dec <- decimal_date(pheno$Perte)


pheno <- pheno[pheno$year %in% c(2015:2019, 2021, 2022),]
years <- c(2015, 2016, 2017, 2018, 2019, 2021, 2022)

# Create an environment to store the plots
plot_env <- new.env()

# Loop through each year
for (year in years) {
  
  # Read temperature profile data
  profil <- read.csv(paste0("Data/Processed/lake/temp_", year, ".csv"))
  profil$Date <- as.Date(profil$Date)
  profil$Date_dec <- decimal_date(profil$Date)
  profil$Temperature <- as.numeric(as.character(profil$Temperature))
  
  # Interpolate temperature
  mba.temp <- mba.surf(profil[,c('Date_dec','Depth','Temperature')],100, 100)
  dimnames(mba.temp$xyz.est$z) <- list(mba.temp$xyz.est$x, mba.temp$xyz.est$y)
  df1 <- reshape2::melt(mba.temp$xyz.est$z, varnames = c('Date_dec','Depth'), value.name = 'Temperature') 
  
  # Add custom x-axis tick marks
  tick_dates <- ymd(paste0(year, c("-05-01", "-06-01", "-07-01", "-08-01", "-09-01", "-10-01", "-11-01")))
  tick_labels <- c("May", "June", "July", "Aug", "Sept", "Oct", "Nov")
  tick_positions <- decimal_date(tick_dates)
  
  pheno_year <- pheno[pheno$year == year,]
  
  # Create plot
  p1 <- ggplot(df1, aes(x = Date_dec, y = Depth)) +
    geom_raster(aes(fill = Temperature), interpolate = F, hjust = 0.5, vjust = 0.5) +
    geom_contour(aes(z = Temperature)) +
    geom_point(data = profil, aes(Date_dec, Depth), colour = 'white') +
    scale_y_continuous(trans = "reverse", breaks = c(0, 2.5, 5, 7.5, 10), limits = c(10.5, 0)) +
    scale_fill_gradientn(colours = matlab.like2(10), limits = c(0, 27))+
    labs(y = 'Depth (m)', fill = 'Temp (°C)', x = "") +
    scale_x_continuous(breaks = tick_positions, labels = tick_labels)+  
    theme_bw() +  
    ggtitle(paste0(year))+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text = element_text(size = 14),
          axis.title = element_text(size = 16),
          plot.title = element_text(size = 18))
  
  if (nrow(pheno_year) > 0) {
    p1 <- p1 + geom_vline(data = pheno_year, aes(xintercept = decimal_date(Couleurs)), linetype = "dashed", colour = "black", linewidth = 1.5) +
      geom_vline(data = pheno_year, aes(xintercept = decimal_date(Perte)), linetype = "dashed", colour = "black", linewidth = 1.5)
  }
  
  # Store the plot as an object in the environment
  plot_env[[paste0("plot_", year)]] <- p1
}


plot_grid(plotlist = p1, ncol = 3)
