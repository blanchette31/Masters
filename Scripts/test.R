#Prepare workspace
rm(list = ls())

#Libraries
library(tidyverse)
library(ggplot2)

#Load data
sth <- read.csv("Data//Processed//precip//precip_brief.csv")
sth$date <- as.Date(sth$date)
sth$Year <- as.numeric(sth$Year)
pheno <- read.csv("Data//Raw//phenocam//Dates_phenocam_V2.csv")
pheno$Couleurs <- as.Date(pheno$Couleurs)
pheno$Perte <- as.Date(pheno$Perte)
pheno[pheno$year == 2012,'Couleurs'] <- '2012-10-03' #pick date in between Sept 26 and Oct 10
pheno$doy_start <- as.numeric(strftime(pheno$Couleurs, '%j'))
pheno$doy_end <- as.numeric(strftime(pheno$Perte, '%j'))
nasa <- read.csv("Data//Processed//precip//precip_nasa_bound.csv")
nasa$doy <- as.numeric(strftime(nasa$date, '%j'))

#merge data frames
colnames(sth)[3:6] <- c('year','month','day','rain_sth')
sth$doy <- as.numeric(strftime(sth$date, '%j'))

df <- merge(df, nasa[,c("year", "doy", "rain_nasa")],
            by = c("year", "doy"), all = T)