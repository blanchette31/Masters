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


# Remove 2020 data from the dataframe
profil <- profil %>% 
  filter(year(date) != 2020)

cut <- read.csv("Data/Processed/precip/era5_cut_buoy.csv")
cut$date <- as.Date(cut$date, format = "%Y-%m-%d")

pheno = read.csv("Data//Processed//phenocam//pheno_clean.csv", header = TRUE)
pheno$Couleurs <-  as.Date(pheno$Couleurs)
pheno$Perte <- as.Date(pheno$Perte)
pheno$col_dec <-  decimal_date(pheno$Couleurs)
pheno$perte_dec <- decimal_date(pheno$Perte)
pheno_year <- pheno[pheno$year == 2015, ]

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

long_15 <- subset(profil_long, year(Date) == 2015)


#Interpolations de la température
mba.temp <- mba.surf(long_15[,c('Date_dec','Depth','Temperature')],100, 100)
dimnames(mba.temp$xyz.est$z) <- list(mba.temp$xyz.est$x, mba.temp$xyz.est$y)
df1 <- reshape2::melt(mba.temp$xyz.est$z, varnames = c('Date_dec','Depth'), value.name = 'Temperature') 
# # Subset thermo for the given year
# thermo_year <- thermo[year(thermo$date) == year,]  


# Add custom x-axis tick marks
tick_dates <- ymd(paste0(2015, c("-05-01", "-06-01", "-07-01", "-08-01", "-09-01", "-10-01", "-11-01")))
tick_labels <- c("May", "June", "July", "Aug", "Sept", "Oct", "Nov")
tick_positions <- decimal_date(tick_dates)

# Graphiques
p1 <- ggplot(df1, aes(x = Date_dec, y = Depth)) +
  geom_raster(aes(fill = Temperature), interpolate = F, hjust = 0.5, vjust = 0.5) +
  geom_contour(aes(z = Temperature)) +
  geom_point(data = long_15, aes(Date_dec, Depth), colour = 'white') +
  # geom_line(data = thermo_year, aes(x = decimal_date(date), y = thermo.depth), color = "black", linewidth = 1.2) +
  # scale_y_reverse(breaks = c(0, 2.5, 5, 7.5, 10)) +
  scale_y_continuous(trans = "reverse", breaks = c(0, 2.5, 5, 7.5, 10), limits = c(10.5, 0), expand = c(0,0)) +
  scale_fill_gradientn(colours = matlab.like2(10), limits = c(0, 30)) +
  labs(y = 'Depth (m)', fill = 'Temp (°C)', x = "") +
  scale_x_continuous(breaks = tick_positions, labels = tick_labels, expand = c(0,0)) +  
  theme_bw() +  
  ggtitle("MYLAKE 2015") +
theme(panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), 
      # legend.position = "none",
      axis.line = element_line(color = "black", size = 1.2),
      axis.ticks = element_line(color = "black", size = 1.2),
      axis.ticks.length = unit(0.25, "cm"),
      axis.text.x = element_text(size = 16), # Increase axis tick mark text size
      axis.text.y = element_text(size = 16), # Increase axis tick mark text size
      axis.title.x = element_text(size = 18), # Increase axis labels size
      axis.title.y = element_text(size = 18))

# Add vertical lines for start and end of leaf fall if pheno data is available
if (nrow(pheno_year) > 0) {
  p1 <- p1 + geom_vline(data = pheno_year, aes(xintercept = decimal_date(Couleurs)), linetype = "dashed", colour = "black", linewidth = 1.5) +
    geom_vline(data = pheno_year, aes(xintercept = decimal_date(Perte)), linetype = "dashed", colour = "black", linewidth = 1.5)
}

p1
ggsave("Data/export/profil_2015_model.png", p1, width = 12, height = 7, units = "in", dpi = 300)