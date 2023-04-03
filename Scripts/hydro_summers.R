#Hydrograph for the summers of each year

#Prepare workspace
rm(list = ls())
library(ggplot2)
library(tidyverse)
library(ggridges)

#Check example: https://rpubs.com/cxiao/hydrograph-ggplot2-plot

#Open data
q <- read.csv("Data/Processed/debit/debit_merged.csv")
q$date <- as.Date(q$date)
pheno <- read.csv("Data/Raw/phenocam/Dates_phenocam_V2.csv")
pheno$Couleurs <- as.Date(pheno$Couleurs)
pheno$Perte <- as.Date(pheno$Perte)
pheno[pheno$year == 2012,'Couleurs'] <- '2012-10-03' #pick date in between Sept 26 and Oct 10
pheno$doy_start <- as.numeric(strftime(pheno$Couleurs, '%j'))
pheno$doy_end <- as.numeric(strftime(pheno$Perte, '%j'))
nasad = read.csv("Data/Processed/precip/precip_nasa_bound.csv")

# Create one data frame for discharge and rain 

df = merge(nasad[,c("doy","year","rain_nasa")],
                q[,c('date','year','doy','debit_total_m3_jour')], 
                by = c("year", "doy"))
df = df[order(as.Date(df$date, format="%Y-%m-%d")),]
colnames(df)[c(3,5)] = c('rain', "q")
df$year = as.factor(df$year)

# Subset dataframe from june 1st (arbitrary date) to 2 weeks before first leaf fall

df <- df[df$doy >= 152  &
           df$doy <= min(pheno$doy_start) - 14,]

#Calculate total volume of rain on watershed
df$vol_rain = 1075472 * df$rain
df$year = as.factor(df$year)
ggplot(df, aes(x = vol_rain, y = year)) +
  geom_density_ridges(aes(y = year))


#Check number of year for rain, discharge and pheno data
length(unique(df$year)) #2013 absent
length(unique(pheno$year)) 

#Create a pheno data frame with the same years as rain and discharge
pheno2 <- pheno[pheno$year != 2013,]

#Create list
dfl <- split(df, df$year)

#Vector for xaxis (month and weeks)
month_tick <- as.numeric(strftime(c('2011-06-01','2011-07-01','2011-08-01'),'%j'))
month_label <- c('June','July','Aug')
week_tick <- as.numeric(strftime(c('2011-06-08','2011-06-15','2011-06-22','2011-07-08', '2011-07-15','2011-07-22','2011-08-08','2011-08-15', '2011-08-22'), '%j'))

#Export graphic to pdf
pdf('Data/export/hydrographs_all_years_summer_nasa.pdf', width = 8, height = 10)

#Change margin to display text on the right (default is c(5, 4, 4, 2))
#And open panels to plot graphics
par(mfrow = c(5,2), mar = c(2, 4.5, 2, 4))


#Loop to display all years

for(i in 1:length(dfl)){
  
  #Vectors of maximum discharge and rain (to correctly display y axis)
  maxq <- max(dfl[[i]]$q, na.rm = T)
  paste(maxq)
  maxrain <- max(dfl[[i]]$rain, na.rm = T)
  
  #Open empty graph
  plot(dfl[[i]]$q ~ dfl[[i]]$doy, 
       ylim = c(0, 1.3*maxq), xlim = c(152,230),
       xlab = '', ylab = "", axes = F,type = "n")
  
  #Plot box of moment of leaf change (color to leaf out)
  rect(pheno2[i,"doy_start"],-600, pheno2[i,"doy_end"], maxq *1.7,density=NULL, col= "#FDDBC7" , border=NA) 
  
  #Add discharge
  par(new = T)
  plot(dfl[[i]]$q ~ dfl[[i]]$doy, 
       type = 'b', las = 1, #to display line only put 'l' instead of 'b'
       ylim = c(0, 1.3*maxq), xlim = c(152,230),
       ylab = expression('Discharge (m'^3*'/d)'), xlab = '',
       xaxs = 'i', yaxs = 'i', #prolonge line to graph borders
       xaxt = 'n', #dont display x labels
       main = names(dfl)[i]) 
  axis(1, at = month_tick, label = month_label) #add x axis labels (month)
  axis(1, at = week_tick, label = F, tck=-0.015) #add tick marks of weeks (from first day of the month)
  
  #Add precipitation
  par(new = TRUE)
  plot(x = dfl[[i]]$doy, y = rep(0, nrow(dfl[[i]])),
       type = "n", ylim = c(1.5*max(df$rain, na.rm = T), 0), #Take max rain for all year
       xaxs = "i", yaxs = "i",
       axes = FALSE, xlab = "", ylab = "")
  segments(x0 = dfl[[i]]$doy, y0 = rep(0, nrow(dfl[[i]])),
           x1 = dfl[[i]]$doy, y1 = dfl[[i]]$rain, 
           lend = 2, lwd =1, xlim = c(152,230), ylim = c(5 * max(df$rain, na.rm = T), 0))
  #yrAxis  <- seq(0, ceiling(maxrain), length.out = 5) #this is to have varying scales, to put in axis below 
  axis(4, at = c(0,20,40,60,80), las = 1)
  mtext("Rainfall (mm)", side = 4, line = 2, cex = 0.7)
  
}

dev.off() #Close pdf device

#Export graphic to pdf
pdf('Data/export/hydrographs_all_years_summer_nasa_fixed_test.pdf', width = 8, height = 10)

#Change margin to display text on the right (default is c(5, 4, 4, 2))
#And open panels to plot graphics
par(mfrow = c(5,2), mar = c(2, 4.5, 2, 4))


#Loop to display all years

for(i in 1:length(dfl)){
  
  #Vectors of maximum discharge and rain (to correctly display y axis)
  maxq_fixed = max(sapply(dfl, function(dfl) max(dfl$q, na.rm=TRUE)))
  maxrain <- max(dfl[[i]]$rain, na.rm = T)
  
  #Open empty graph
  plot(dfl[[i]]$q ~ dfl[[i]]$doy, 
       ylim = c(0, 1.3*maxq_fixed), xlim = c(152,230),
       xlab = '', ylab = "", axes = F,type = "n")
  
  #Plot box of moment of leaf change (color to leaf out)
  rect(pheno2[i,"doy_start"],-600, pheno2[i,"doy_end"], maxq_fixed *1.7,density=NULL, col= "#FDDBC7" , border=NA) 
  
  #Add discharge
  par(new = T)
  plot(dfl[[i]]$q ~ dfl[[i]]$doy, 
       type = 'b', las = 1, #to display line only put 'l' instead of 'b'
       ylim = c(0, 1.3*maxq_fixed), xlim = c(152,230),
       ylab = expression('Discharge (m'^3*'/d)'), xlab = '',
       xaxs = 'i', yaxs = 'i', #prolonge line to graph borders
       xaxt = 'n', #dont display x labels
       main = names(dfl)[i]) 
  axis(1, at = month_tick, label = month_label) #add x axis labels (month)
  axis(1, at = week_tick, label = F, tck=-0.015) #add tick marks of weeks (from first day of the month)
  
  #Add precipitation
  par(new = TRUE)
  plot(x = dfl[[i]]$doy, y = rep(0, nrow(dfl[[i]])),
       type = "n", ylim = c(1.5*max(df$rain, na.rm = T), 0), #Take max rain for all year
       xaxs = "i", yaxs = "i",
       axes = FALSE, xlab = "", ylab = "")
  segments(x0 = dfl[[i]]$doy, y0 = rep(0, nrow(dfl[[i]])),
           x1 = dfl[[i]]$doy, y1 = dfl[[i]]$rain, 
           lend = 2, lwd =1, xlim = c(152,230), ylim = c(5 * max(df$rain, na.rm = T), 0))
  #yrAxis  <- seq(0, ceiling(maxrain), length.out = 5) #this is to have varying scales, to put in axis below 
  axis(4, at = c(0,20,40,60,80), las = 1)
  mtext("Rainfall (mm)", side = 4, line = 2, cex = 0.7)
  
}

dev.off() #Close pdf device



#Return default graphic settings
par(mfrow = c(1,1), mar = c(5, 4, 4, 2))


#ridgeline plots 

pdf('Data/export/ridgeline_rain_summer.pdf', width = 8, height = 10)

ggplot(df, aes(x = rain, y = year)) +
  geom_density_ridges(aes( fill = year))+
  labs(y = "Year", x = "Rain (mm)", title = "Ridgeline density plots of rain during summer")
  
dev.off()

pdf('Data/export/ridgeline_discharge_summer.pdf', width = 8, height = 10)

ggplot(df, aes(x = q, y = year))+
  geom_density_ridges(aes(fill = year))+
  labs(y = "Year", x = "Discharge(m^3/day)", title = "Ridgeline density plots of discharge during summer")

dev.off()


ggplot(df, aes(y = q, x = vol_rain, color = as.factor(year))) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  scale_color_brewer(palette = 'Set1', name = '') +
  labs(x = 'Total volume of Rain (m^3)', y = 'Daily Discharge (m^3)', title = "Summer") +
  theme_minimal()
ggsave('Data/export/vol_rain_vs_q_summer.pdf', device = 'pdf', width = 5, height = 4)


ggplot(df, aes(y = q, x = vol_rain)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  scale_color_brewer(palette = 'Set1', name = '') +
  labs(x = 'Total volume of Rain (m^3)', y = 'Daily Discharge (m^3)', title = "Summer") +
  theme_minimal()+
  facet_wrap(~year)


#Base flow summers

df_flow = df[df$doy >= 195  &
                     df$doy <= 209,]


base_flow = df_flow %>% 
  dplyr::group_by(year) %>%
  dplyr::summarise(mean_base_flow = mean(q))
write.csv(base_flow, "Data//Processed//debit//base_flow_summers.csv")

df_flow_min = df[df$doy >= 195  &
                df$doy <= 226,]

flow_min = df_flow_min %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(min_flow = min(q, na.rm = TRUE),
                   max_flow = max(q, na.rm = TRUE))
write.csv(flow_min, "Data//Processed//debit/min_max_flow_summer.csv")
