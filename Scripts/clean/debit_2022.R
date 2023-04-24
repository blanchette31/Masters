# prepare workspace

rm(list = ls())
library(tidyverse)
library(lubridate)

debit <-  read.csv("Data/Raw//debit//debit_journalier_2022.csv", header = TRUE, sep = " ")

debit$date <- as.POSIXct(debit$date, format = "%Y-%m-%d")

debit <- debit %>% 
  group_by(date) %>%
  summarise(daily_discharge = mean(m3_hr),
            level_mean = mean(mean_niveau_m))


#Extract year and doy from date
debit = debit %>%
  mutate(
    doy = strftime(date, format = "%j"),
    year = strftime(date, format = "%Y"))

write.csv(debit, "Data//Processed//debit//debit_journalier_2022.csv")
