#Runoff ratio

#Prepare workspace
rm(list = ls())
library(tidyverse)
library(stats)
library(ggridges)
library(khroma)
library(agricolae)
library(scales)


#Open data
sth <- read.csv("Data/Processed/precip/precip_brief.csv")
sth$date <- as.Date(sth$date)
q <- read.csv("Data/Processed/debit/debit_2011-2022.csv")
q$date <- as.Date(q$date)
pheno <- read.csv("Data/Raw/phenocam/Dates_phenocam_V2.csv")
pheno$Couleurs <- as.Date(pheno$Couleurs)
pheno$Perte <- as.Date(pheno$Perte)
pheno[pheno$year == 2012,'Couleurs'] <- '2012-10-03' #pick date in between Sept 26 and Oct 10
pheno$doy_start <- as.numeric(strftime(pheno$Couleurs, '%j'))
pheno$doy_end <- as.numeric(strftime(pheno$Perte, '%j'))
era5 <- read.csv("Data/Processed/precip/era5_cut_buoy.csv")




df_era5 = merge(era5[,c("doy","year","rain")],
                q[,c('date','year','doy','debit_total_m3_jour')], 
                by = c("year", "doy"))
df_era5 = df_era5[order(as.Date(df_era5$date, format="%Y-%m-%d")),]
colnames(df_era5)[c(3,5)] = c('rain', "q")


# Subset data for 2015
df_2015 <- df_era5[df_era5$year == 2015, ]
df_2015 <- df_2015 %>% filter(doy >= 264)


# Subset data for 2017
df_2017 <- df_era5[df_era5$year == 2017, ]
df_2017 <- df_2017 %>% filter(doy >= 272)

# Subset data for the required years
df_2016 <- df_era5 %>% filter(year == 2016)
df_2016 <- df_2016 %>% filter(doy >= 265)

df_2018 <- df_era5 %>% filter(year == 2018)
df_2018 <- df_2018 %>% filter(doy >= 270)

df_2019 <- df_era5 %>% filter(year == 2019)
df_2019 <- df_2019 %>% filter(doy >= 258)

df_2021 <- df_era5 %>% filter(year == 2021)
df_2021 <- df_2021 %>% filter(doy >= 256)

df_2022 <- df_era5 %>% filter(year == 2022)
df_2022 <- df_2022 %>% filter(doy >= 244)




# Calculate total runoff ratio for 2015 (after DOY 264)
total_q_2015 <- sum(df_2015$q, na.rm = TRUE)
total_rain_2015 <- sum(df_2015$rain, na.rm = TRUE)
runoff_ratio_2015 <- (total_q_2015 / (total_rain_2015 * 1075472 / 1000 )) * 0.25

# Calculate total runoff ratio for 2016 (after DOY 265)
total_q_2016 <- sum(df_2016$q, na.rm = TRUE)
total_rain_2016 <- sum(df_2016$rain, na.rm = TRUE)
runoff_ratio_2016 <- (total_q_2016 / (total_rain_2016 * 1075472 / 1000)) * 0.25

# Calculate total runoff ratio for 2017
total_q_2017 <- sum(df_2017$q, na.rm = TRUE)
total_rain_2017 <- sum(df_2017$rain, na.rm = TRUE)
runoff_ratio_2017 <- (total_q_2017 / (total_rain_2017 * 1075472 / 1000)) * 0.25

# Calculate total runoff ratio for 2018
total_q_2018 <- sum(df_2018$q, na.rm = TRUE)
total_rain_2018 <- sum(df_2018$rain, na.rm = TRUE)
runoff_ratio_2018 <- (total_q_2018 / (total_rain_2018 * 1075472 / 1000)) * 0.25

# Calculate total runoff ratio for 2019
total_q_2019 <- sum(df_2019$q, na.rm = TRUE)
total_rain_2019 <- sum(df_2019$rain, na.rm = TRUE)
runoff_ratio_2019 <- (total_q_2019 / (total_rain_2019 * 1075472 / 1000)) * 0.25

# Calculate total runoff ratio for 2021
total_q_2021 <- sum(df_2021$q, na.rm = TRUE)
total_rain_2021 <- sum(df_2021$rain, na.rm = TRUE)
runoff_ratio_2021 <- (total_q_2021 / (total_rain_2021 * 1075472 / 1000)) * 0.25

# Calculate total runoff ratio for 2022
total_q_2022 <- sum(df_2022$q, na.rm = TRUE)
total_rain_2022 <- sum(df_2022$rain, na.rm = TRUE)
runoff_ratio_2022 <- (total_q_2022 / (total_rain_2022 * 1075472 / 1000)) * 0.25

# Print results
runoff_ratio_2015
runoff_ratio_2016
runoff_ratio_2017
runoff_ratio_2018
runoff_ratio_2019
runoff_ratio_2021
runoff_ratio_2022


# Create a data frame to store the results
runoff_ratios <- data.frame(
  Year = c(2015, 2016, 2017, 2018, 2019, 2021, 2022),
  Runoff_Ratio = c(
    runoff_ratio_2015,
    runoff_ratio_2016,
    runoff_ratio_2017,
    runoff_ratio_2018,
    runoff_ratio_2019,
    runoff_ratio_2021,
    runoff_ratio_2022
  )
)

# View the table
runoff_ratios

write.csv(runoff_ratios,"Data/Processed/debit/cummulative_runoff_ratio.csv")
