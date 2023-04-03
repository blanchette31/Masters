#Comparing volume of water entering from precipitations during the summer and study period

#Prepare workspace
rm(list = ls())
library(tidyverse)
library(ggplot2)
library(plyr)


#Load data
q <- read.csv("Data/Processed/debit/debit_merged.csv")
q$date <- as.Date(q$date)
pheno <- read.csv("Data/Raw/phenocam/Dates_phenocam_V2.csv")
pheno$Couleurs <- as.Date(pheno$Couleurs)
pheno$Perte <- as.Date(pheno$Perte)
pheno[pheno$year == 2012,'Couleurs'] <- '2012-10-03' #pick date in between Sept 26 and Oct 10
pheno$doy_start <- as.numeric(strftime(pheno$Couleurs, '%j'))
pheno$doy_end <- as.numeric(strftime(pheno$Perte, '%j'))
nasad = read.csv("Data/Processed/precip/precip_nasa_bound.csv")


#Merge dataframes

df = merge(nasad[,c("doy","year","rain_nasa")],
           q[,c('date','year','doy','debit_total_m3_jour')], 
           by = c("year", "doy"))
df = df[order(as.Date(df$date, format="%Y-%m-%d")),]
colnames(df)[c(3,5)] = c('rain', "q")
df$year = as.factor(df$year)

df<- df %>%
  mutate(
 period = case_when(
  df$doy < 152 ~ "other",
  df$doy >= 230 ~ "study_period",
  TRUE ~ "summer"))


df = subset(df, df$period != "other")

pdf('Data/export/comp_rain.pdf', width = 8, height = 10)

ggplot(df, aes( x= year, y = rain, fill = period ))+
  geom_boxplot()+
  labs(y = "Rain (mm)", x = "Year")
dev.off()
  
pdf('Data/export/comp_discharge.pdf', width = 8, height = 10)
    
ggplot(df, aes(x = year, y = q, fill = period))+
  geom_boxplot()+
  labs(y = "Discharge (m^3/day)", x = "Year")

dev.off()
