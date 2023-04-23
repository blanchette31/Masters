# Prepare workspace
rm(list = ls())

library(tidyverse)
library(rLakeAnalyzer)
library(lubridate)


bathy <- load.bathy("Data/Processed/lake/bathy_area.csv")

wtr <-  read.csv("Data/Processed/lake/wtr_temp_for_schmidt.csv")
wtr$datetime <- as.POSIXct(wtr$datetime, format = "%Y-%m-%d %H:%M:%S")
wtr$wtr_00.5 <- NULL



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
write.csv(schmidt, "Data/Processed/lake/schmidt.csv")

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

p2 = ggplot(schmidt, aes(x = datetime, y = schmidt.stability))+
  geom_line()+
  labs(x = "", y = "Schmidt stability")
ggsave("Data/export/schmidt_all_years.png", p2)

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
 group_by(date, doy) %>% 
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
