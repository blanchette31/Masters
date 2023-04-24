# Prepare workspace
rm(list = ls())
library(rLakeAnalyzer)

Zmax = 11.4
zinterval = 0.5


bathy <-approx.bathy(Zmax = 11.4, lkeArea = 179000, Zmean = 4.7, method = "cone", zinterval = 0.5, depths = seq(0, Zmax, by = zinterval))

bathy$areas <- bathy$Area.at.z

write.csv(bathy, "Data/Processed/lake/bathy_area_2.csv")


