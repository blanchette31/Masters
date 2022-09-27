# Packages
library(dplyr)

# Data frames

debit = read.csv("Data//Processed//debit//debit_merged.csv", header = T, sep = ",")

precip = read.csv("Data//Processed//precip//precip_sth_merged.csv", header = T, sep = ",")

#year as factor 

debit$year = as.factor(debit$year)

precip$Year = as.factor(precip$Year)

#split dataframes by year
debit_split = split(debit, debit$year)

precip_split = split(precip, precip$Year)

plot(debit$doy, debit$debit_total_m3_jour)

#discharge by year

par(mfrow = c(3, 4))

for(i in levels(debit$year)) {
  plot(debit[debit$year == i, "debit_total_m3_jour"],
       col = "gray50",
       main = paste(i),
       type = "l",
       ylab ="")
}
