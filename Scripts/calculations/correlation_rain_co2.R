# prepare workspace
rm(list = ls())

# load libraries
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(RColorBrewer)

# load data
era5 <- read.csv("Data/Processed/precip/era5_cut_lf.csv")
co2 <- read.csv("Data/Processed/lake/co2_o2_post_lf.csv")

mean_temperature_by_year <- era5 %>%
  group_by(year) %>%
  summarise(mean_temperature = mean(air_temp.C., na.rm = TRUE))

# Print the result
print(mean_temperature_by_year)

merge <- merge(era5[,c("year", "doy", "rain" )], co2[, c("date", "year", "doy","co2" )], by = c("year", "doy"))

# Calculate correlation between "rain" and "co2"
correlation <- cor(merge$rain, merge$co2, use = "complete.obs")

# Remove rows where "rain" is equal to 0
merge_filt <- merge %>% filter(rain > 5)

correlation_filt <- cor(merge_filt$rain, merge_filt$co2, use = "complete.obs")

# Print the correlation
cat("Correlation between rain and CO2:", correlation, "\n")
cat("Correlation between rain and CO2:", correlation_filt, "\n")
summary(correlation)


ggplot(merge, aes(x = rain, y = co2, , color = as.factor(year)))+
  geom_point()+
  theme_classic()

ggplot(merge_filt, aes(x = rain, y = co2, , color = as.factor(year)))+
  geom_point()+
  theme_classic()

  