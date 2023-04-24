# profil 2015 model

rm(list = ls())

# library 
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

# Préparer les données
profil <- read.csv("Data/model/results/temp_all_years.csv")
profil$date <- as.Date(profil$date, format = "%Y-%m-%d")
cut <- read.csv("Data/Processed/precip/era5_cut_buoy.csv")
cut$date <- as.Date(cut$date, format = "%Y-%m-%d")

thermo <- read.csv("Data/Processed/lake/thermo_depth.csv")
thermo$date <- as.Date(thermo$datetime)


# Filter thermo to keep only dates present in profil
thermo <- thermo[thermo$date %in% cut$date,]


# Remove 2020 data from the dataframe
profil <- profil %>% 
  filter(year(date) != 2020)


pheno = read.csv("Data//Processed//phenocam//pheno_clean.csv", header = TRUE)
pheno$Couleurs <-  as.Date(pheno$Couleurs)
pheno$Perte <- as.Date(pheno$Perte)
pheno$col_dec <-  decimal_date(pheno$Couleurs)
pheno$perte_dec <- decimal_date(pheno$Perte)


# Merge to find common dates
profil_m <- merge(profil, cut[, c("date","doy"), drop = FALSE], by = "date")


# Assuming your merged dataframe is named profil_m
profil_long <- pivot_longer(profil_m,
                            cols = starts_with("wtr_"),
                            names_to = "Depth",
                            names_prefix = "wtr_",
                            values_to = "Temperature")

# Convert depth column to numeric (if necessary)
profil_long$Depth <- as.numeric(profil_long$Depth)

profil_long <- rename(profil_long, Date = date)



profil_long$Date <- as.Date(profil_long$Date)
profil_long$Date_dec <- decimal_date(profil_long$Date)
profil_long$Temperature <- as.numeric(as.character(profil_long$Temperature))



# Loop through each year and create plots
for (year in unique(year(profil_long$Date))) {
  
  # Subset the data for the current year
  df_year <- subset(profil_long, year(Date) == year)
  
  # Interpolate temperature
  mba.temp <- mba.surf(df_year[,c('Date_dec','Depth','Temperature')], 100, 100)
  dimnames(mba.temp$xyz.est$z) <- list(mba.temp$xyz.est$x, mba.temp$xyz.est$y)
  df <- reshape2::melt(mba.temp$xyz.est$z, varnames = c('Date_dec','Depth'), value.name = 'Temperature') 

  # Add custom x-axis tick marks
  tick_dates <- ymd(paste0(year, c("-05-01", "-06-01", "-07-01", "-08-01", "-09-01", "-10-01", "-11-01")))
  tick_labels <- c("May", "June", "July", "Aug", "Sept", "Oct", "Nov")
  tick_positions <- decimal_date(tick_dates)
  
  
  pheno_year <- pheno[pheno$year == year,]
  
  # Plot
  p <- ggplot(df, aes(x = Date_dec, y = Depth)) +
    geom_raster(aes(fill = Temperature), interpolate = FALSE, hjust = 0.5, vjust = 0.5) +
    geom_contour(aes(z = Temperature)) +
    geom_line(data = subset(thermo, year(date) == year), aes(x = decimal_date(date), y = thermo.depth), color = "black", linewidth = 1.2) +
    scale_y_continuous(trans = "reverse", breaks = c(0, 2.5, 5, 7.5, 10), limits = c(10, 0), expand = c(0,0)) +
    scale_fill_gradientn(colours = matlab.like2(10), limits = c(0, 27.5)) +
    labs(y = NULL, fill = 'Temp (°C)', x = NULL) +
    scale_x_continuous(breaks = tick_positions, labels = tick_labels, expand = c(0,0))+ 
    theme_bw() +  
    ggtitle("") +
    theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        legend.position = "none",
        axis.line = element_line(color = "black", size = 1.2),
        axis.ticks = element_line(color = "black", size = 1.2),
        axis.ticks.length = unit(0.15, "cm"),
        axis.text.x = element_text(size = 24), # Increase axis tick mark text size
        axis.text.y = element_text(size = 24)) # Increase axis tick mark text size
        # axis.title.x = element_text(size = 18), # Increase axis labels size
        # axis.title.y = element_text(size = 24)
  
  # Add vertical lines for start and end of leaf fall
  if (nrow(pheno_year) > 0) {
    p <- p + geom_vline(data = pheno_year, aes(xintercept = decimal_date(Couleurs)), linetype = "dashed", colour = "black", linewidth = 1.5)
  }
  #  +
  # geom_vline(data = pheno_year, aes(xintercept = decimal_date(Perte)), linetype = "dashed", colour = "black", linewidth = 1.5) add black line to end of leaf fall
  
  # Save the plot
  filename <- paste0("Data/Figures/profil_", year, "_model_V3.png")
  ggsave(filename, p, width = 12, height = 7, units = "in", dpi = 300)
}
