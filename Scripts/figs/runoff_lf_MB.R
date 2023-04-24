#Runoff ratio

#Prepare workspace
rm(list = ls())
library(tidyverse)
library(stats)
library(ggridges)
library(khroma)
library(agricolae)
library(scales)
library(MetBrewer)
library(cowplot)

# data
rain <- read.csv("Pour_MB/era5_cut_lf.csv")
q <- read.csv("Pour_MB/debit_2011-2022.csv")

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

#Order year
df$year <- factor(df$year, levels(df$year)[c(3,6,7,5,2,1,4)])
ggplot(df, aes(x= year, y = runoff_ratio, color = year))+
  geom_jitter(color = 'grey') +
  geom_boxplot(fill = NA)+
  scale_y_log10() + 
  # scale_y_continuous(limits = c(0, 50))+
  scale_color_manual(values = met.brewer("Hiroshige",8)) +
  theme_cowplot()+
  labs( x= "", y = "Runoff Ratio (%)")


ggplot(df, aes(x = doy, y = debit_total_m3_jour)) +
  geom_point() +
  facet_wrap(~year)
