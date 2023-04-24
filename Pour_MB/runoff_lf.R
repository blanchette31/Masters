#Runoff ratio

#Prepare workspace
rm(list = ls())
library(tidyverse)
library(stats)
library(ggridges)
library(khroma)
library(agricolae)
library(scales)

# data
rain <- read.csv("Data/Processed/precip/era5_cut_lf.csv")
q <- read.csv("Data/Processed/debit/debit_2011-2022.csv")

#Create one data frame for discharge and precipitation data
df <- merge(rain[,c('date','rain', 'doy', 'year')], 
            q[,c('date','year','doy','debit_total_m3_jour')], 
            by = c('date','year', 'doy'))

df <- subset(df, rain > 0)

df$vol_rain = (1075472+179000) * df$rain /1000
df$year = as.factor(df$year)


df = df %>%
  mutate(runoff_ratio = debit_total_m3_jour / (rain*1075472/1000))
df <- df %>% 
  mutate(ratio_perc = runoff_ratio * 100)


ggplot(df, aes(x= year, y = ratio_perc))+
  geom_boxplot()+
  scale_y_continuous(limits = c(0, 50))+
  theme_cowplot()+
  labs( x= "", y = "Runoff Ratio (%)")



