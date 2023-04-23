# prepare workspace
rm(list= ls())

library(tidyverse)
library(ggplot2)
library(here)
library(lubridate)


#load data
df <- read.csv(here("Data/Processed/lake/delta_co2_LS_SM_merged.csv"), header = TRUE)

df$date <- strftime(df$date, format = "%Y-%m-%d")
df$doy <- format(as.Date(df$date), format = "%j")
df$year <- format(as.Date(df$date), format = "%Y")


doy <- df %>% 
  group_by(doy) %>% 
  summarise(mean = delta.CO2)
  
p1 <-   ggplot(doy, aes(x = as.numeric(doy), y = mean))+
  geom_line()+
  geom_point()+
  ylab(expression(paste("Average ", Delta, "CO"[2]," (", mu, "mol/L)")))+
xlab("Day of year")+
  theme_minimal()
ggsave("Data/export/delta_co2_LS_SM_full_year.png", p1, dpi = 300)

  
  #load pheno data to specify dataframe
  pheno <- read.csv("Data/Raw/phenocam/Dates_phenocam_V2.csv")
  pheno$Couleurs <- as.Date(pheno$Couleurs)
  pheno$Perte <- as.Date(pheno$Perte)
  pheno[pheno$year == 2012,'Couleurs'] <- '2012-10-03' #pick date in between Sept 26 and Oct 10
  pheno$doy_start <- as.numeric(strftime(pheno$Couleurs, '%j'))
  pheno$doy_end <- as.numeric(strftime(pheno$Perte, '%j'))
  
  #trim dataframe to study period
  df_study <- df[df$doy >= min(pheno$doy_start) - 14 &
             df$doy <= max(pheno$doy_end, na.rm = T) + 14,]
  
  
  doy_study <- df_study %>% 
    group_by(doy) %>% 
    summarise(mean = delta.CO2)
  
  p2 <- ggplot(doy_study, aes(x = as.numeric(doy), y = mean))+
    geom_point()+
    geom_line()
    ylab(expression(paste("Average ", Delta, "CO"[2]," (", mu, "mol/L)")))+
    xlab("Day of year")+
    theme_minimal()
  ggsave("Data/export/delta_co2_LS_SM_study_period.png", p2, dpi = 300)