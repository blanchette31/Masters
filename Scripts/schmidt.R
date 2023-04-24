# Prepare workspace
rm(list = ls())

library(tidyverse)
library(rLakeAnalyzer)
library(lubridate)


bathy <- load.bathy("Data/Processed/lake/bathy_test.csv")

wtr <-  read.csv("Data/Processed/lake/wtr_temp_for_schmidt.csv")

wtr$datetime <- as.POSIXct(wtr$datetime, format = "%Y-%m-%d %H:%M:%S")
avg_0.5_wtr <- c("wtr_0.5", "wtr_00.5")
wtr$wtr_0.5 <- rowMeans(wtr[, avg_0.5_wtr, drop = FALSE])
wtr$wtr_00.5 <- NULL

wtr_2022 <- read.csv("Data/Processed/lake/wtr_temp_for_schmidt_2022.csv")
wtr_2022$datetime <- as.POSIXct(wtr_2022$datetime, format = "%Y-%m-%d %H:%M:%S")
avg_0.65_wtr <- c("wtr_0.65", "wtr_00.65")
wtr$wtr_0.65 <- rowMeans(wtr_2022[, avg_0.65_wtr, drop = FALSE])
wtr$wtr_00.65 <- NULL

wtr_2015 <- read.csv("Data/Processed/lake/wtr_temp_for_schmidt_2015.csv")
wtr_2015$datetime <- as.POSIXct(wtr_2015$datetime, format = "%Y-%m-%d %H:%M:%S")

wtr_2021 <- subset(wtr, year(datetime) == 2021)
wtr_2019 <- subset(wtr, year(datetime) == 2019)
wtr_2018 <- subset(wtr, year(datetime) == 2018)
wtr_2017 <- subset(wtr, year(datetime) == 2017)
wtr_2016 <- subset(wtr, year(datetime) == 2016)

schmidt <-  ts.schmidt.stability(wtr,bathy, na.rm = TRUE)
schmidt <- schmidt %>%
  mutate(
    date = strftime(datetime, format = "%Y-%m-%d"),
    doy = strftime(datetime, format = "%j"),
    year = strftime(datetime, format = "%Y"))


schmidt_2022 <- ts.schmidt.stability(wtr_2022, bathy, na.rm = TRUE)
schmidt_2022 <- schmidt_2022 %>%
  mutate(
    date = strftime(datetime, format = "%Y-%m-%d"),
    doy = strftime(datetime, format = "%j"),
    year = strftime(datetime, format = "%Y"))
schmidt_2015 <- ts.schmidt.stability(wtr_2015, bathy, na.rm = TRUE)
schmidt_2015 <- schmidt_2015 %>%
  mutate(
    date = strftime(datetime, format = "%Y-%m-%d"),
    doy = strftime(datetime, format = "%j"),
    year = strftime(datetime, format = "%Y"))

schmidt_combined <- rbind(schmidt_2015, schmidt, schmidt_2022)

write.csv(schmidt_combined, "Data/Processed/lake/schmidt.csv")
#schmidt stability plot 

schmidt_2021 <- subset(schmidt, year(datetime) == 2021)
schmidt_2019 <- subset(schmidt, year(datetime) == 2019)
schmidt_2018 <- subset(schmidt, year(datetime) == 2018)
schmidt_2017 <- subset(schmidt, year(datetime) == 2017)
schmidt_2016 <- subset(schmidt, year(datetime) == 2016)

p1 = ggplot(schmidt_2021, aes(x = datetime, y = schmidt.stability))+
  geom_line()+
  labs(x = "", y = "Schmidt stability")
ggsave("Data/export/schmidt_2021.png", p1)

p2 = ggplot(schmidt_2019, aes(x = datetime, y = schmidt.stability))+
  geom_line()+
  labs(x = "", y = "Schmidt stability")
ggsave("Data/export/schmidt_2019.png", p2)

p3 = ggplot(schmidt_2018, aes(x = datetime, y = schmidt.stability))+
  geom_line()+
  labs(x = "", y = "Schmidt stability")
ggsave("Data/export/schmidt_2018.png", p3)

p4 = ggplot(schmidt_2017, aes(x = datetime, y = schmidt.stability))+
  geom_line()+
  labs(x = "", y = "Schmidt stability")
ggsave("Data/export/schmidt_2017.png", p4)

p5 = ggplot(schmidt_2016, aes(x = datetime, y = schmidt.stability))+
  geom_line()+
  labs(x = "", y = "Schmidt stability")
ggsave("Data/export/schmidt_2016.png", p5)

p6 = ggplot(schmidt, aes(x = datetime, y = schmidt.stability))+
  geom_line()+
  labs(x = "", y = "Schmidt stability")
ggsave("Data/export/schmidt_all_years.png", p6)

  


#thermo depth 
thermo_depth <- ts.thermo.depth(wtr, Smin = 0.1, na.rm = TRUE)
write.csv(thermo_depth, "Data/Processed/lake/thermo_depth.csv")

#meta depth 
meta_depth <- ts.meta.depths(wtr, slope = 0.1, na.rm = TRUE)
write.csv(meta_depth , "Data/Processed/lake/meta_depth.csv")

#water heatmap layers
png("Data/export/wtr_heatmap_layers.png", width = 12, height = 7, units = "in", res = 300)
wtr_layers <-  wtr.heatmap.layers(wtr)
dev.off()

mean <- schmidt %>% 
 group_by(date, doy, year) %>% 
  summarise(mean = mean(schmidt.stability))

png("Data/export/therm_2021.png", width = 12, height = 7, units = "in", res = 300)
therm_2021 <- wtr.plot.temp(wtr_2021)
dev.off()

png("Data/export/therm_2019.png", width = 12, height = 7, units = "in", res = 300)
therm_2019 <- wtr.plot.temp(wtr_2019)
dev.off()

png("Data/export/therm_2018.png", width = 12, height = 7, units = "in", res = 300)
therm_2018 <- wtr.plot.temp(wtr_2018)
dev.off()

png("Data/export/therm_2017.png", width = 12, height = 7, units = "in", res = 300)
therm_2017 <- wtr.plot.temp(wtr_2017)
dev.off()

png("Data/export/therm_2016.png", width = 12, height = 7, units = "in", res = 300)
therm_2016 <- wtr.plot.temp(wtr_2016)
dev.off()



## daily schmidt

schmidt_doy <- schmidt %>% 
  group_by(doy, year, date) %>%
  summarise(schmidt.stability = mean(schmidt.stability))

mean$doy <- as.numeric(mean$doy)
ggplot(mean, aes(x= doy, y = mean, color = factor(year)))+
  geom_smooth(method = "gam", se = FALSE) +
  labs(x = "", y = "Schmidt stability")





