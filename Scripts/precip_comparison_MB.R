#Compare precipitation at St-Hippolyte and SBL

#Prepare workspace
rm(list = ls())
library(tidyverse)

sth <- read.csv("../data/raw/precip_brief_sth.csv")
sth$date <- as.Date(sth$date)
sth$Year <- as.numeric(sth$Year)
sbl <- read.csv("../data/raw/precip_sbl_clean.csv")
sbl$doy <- as.numeric(as.character(sbl$doy))
sbl <- sbl[,c('year','doy','time','precip')]
sbl$year <- as.numeric(as.character(sbl$year))
pheno <- read.csv("../data/raw/Dates_phenocam_V2.csv")
pheno$Couleurs <- as.Date(pheno$Couleurs)
pheno$Perte <- as.Date(pheno$Perte)
pheno[pheno$year == 2012,'Couleurs'] <- '2012-10-03' #pick date in between Sept 26 and Oct 10
pheno$doy_start <- as.numeric(strftime(pheno$Couleurs, '%j'))
pheno$doy_end <- as.numeric(strftime(pheno$Perte, '%j'))

#Calculate daily precipitation for sbl
sbl$precip <- as.numeric(as.character(sbl$precip))
sbld <- sbl %>% 
  group_by(year, doy) %>% 
  summarise(rain_sbl = sum(precip))

#Merge data frame
colnames(sth)[3:6] <- c('year','month','day','rain_sth')
sth$doy <- as.numeric(strftime(sth$date, '%j'))
df <- merge(sth[,c('date','year','doy','rain_sth')], 
            sbld,
            by = c('year','doy'), all = T)
df$year <- as.numeric(df$year)
df <- df[!is.na(df$year),] #remove NA in year

#Keep only year with data from SBL
unique(sbl$year)
df <- df[df$year < 2016,]

#Subset data frame for 2 weeks before earliest color change and latest leaf fall
df <- df[df$doy >= min(pheno$doy_start) - 14 &
           df$doy <= max(pheno$doy_end, na.rm = T) + 14,]

#Look at data
ggplot(df, aes(y = rain_sbl, x = rain_sth, color = as.factor(year))) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  scale_color_brewer(palette = 'Set1', name = '') +
  labs(x = 'Rain SBL (mm)', y = 'Rain Saint-Hippolyte (mm)') +
  theme_minimal()
ggsave('../export/precip_sbl_vs_sth_scatterplot.pdf', device = 'pdf', width = 5, height = 4)

ggplot(df) +
  geom_bar(aes(x = doy, y = rain_sth), color = 'black', stat = 'identity') +
  geom_bar(aes(x = doy, y = rain_sbl), fill = 'red', stat = 'identity', alpha = 0.7) +
  labs(y = 'Rain (mm)') +
  facet_wrap(~year) +
  theme_minimal()
ggsave('../export/precip_sbl_vs_sth_temporalplot.pdf', device = 'pdf', width = 8, height = 5)

