# Packages
library(dplyr)
library(tidyverse)
library(ggplot2)

# Data frames

debit = read.csv("Data//Processed//debit//debit_merged.csv", header = T, sep = ",")

precip = read.csv("Data//Processed//precip//precip_sth_merged.csv", header = T, sep = ",")

pheno = read.csv("Data//Processed//phenocam//dates_phenocam.csv", header = T)


#fix year column for pheno 
pheno  = pheno %>%
  rename(year =ï..year )

debit$date = as.Date(debit$date, format, tryFormats = c("%Y-%m-%d"))


precip = precip %>%
  rename(year = Year)


#determine doy_bef and doy_aft 

pheno = pheno %>%
  mutate(doy_bef = doycol - 14) %>%
  mutate(doy_aft = doyper + 14) 

#year as factor 

debit$year = as.factor(debit$year)

precip$year = as.factor(precip$year)



debit_year = split(debit, debit$year)

debit_sel = for(i in list(debit_year)){
  
}

debit_sel = for(i in levels(pheno$year)){
    debit[debit$doy >= pheno[pheno$year == i],pheno$doy_bef]
  }

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


#select data in function of doy_bef and doy_aft

debit_bef = debit %>% 
  rowwise() %>% 
  mutate(present = any(doy >= pheno$doy_bef)) %>% 
  filter(present) %>% 
  data.frame()

debit_sel = debit_bef %>%
  rowwise() %>%
  mutate (present = any(doy <=pheno$doy_aft)) %>%
  filter(present) %>%
  data.frame()

par(mfrow = c(3, 4))

for(i in levels(debit_sel$year)) {
  plot(debit_sel[debit_sel$year == i, "debit_total_m3_jour"],
       col = "gray50",
       main = paste(i),
       type = "l",
       ylab ="")
}

# ajout des précipitations 
par(new = TRUE)

precip_bef = precip %>% 
  rowwise() %>%
  mutate(present = any(doy >= pheno$doy_bef)) %>%
  filter(present) %>%
  data.frame()
  
precip_sel = precip_bef %>%
  rowwise() %>%
  mutate(present = any(doy >= pheno$doy_aft)) %>%
  filter(present) %>%
  data.frame()

for(i in levels(precip_sel$year)){
  hist(precip_sel[precip_sel$year == i, "Total.Precip..mm."],
       main = paste(i),
       col = "gray80",
       ylab = "pluie totale (mm)",
       xlab = "doy")
}

par(mfrow= c(3,4))

for(i in levels(precip_sel$year)){
  plot(x = debit_sel$doy, y = debit_sel$year,
       type = "l", col = "red")
  par(new= TRUE)
  segments(x0 = precip$sel)
  
}
debit_omit = na.omit(debit_sel)
debit_omit$date = as.Date(debit_omit$date, "%Y-%m-%d")
par(new= TRUE)


plot(x = debit_omit$date, y = debit_omit$debit_total_m3_jour,
     type = "l", col = "red", axes = FALSE,
     ylim = c(0, 1.3 * max(debit_omit),
              xaxs = "i", yaxs = "i"))
precip$date = as.Date(precip$date, "%Y-%m-%d")
segments(x0 = precip$date, y0 = rep(0, nrow(precip)),
         x1 = precip$date, y1 = precip$Total.Rain..mm., lend = 2, lwd = 1)

maxPR   <- max(precip$Total.Rain..mm. , na.rm = T)

yrAxis  <- seq(0, ceiling(maxPR), length.out = 5)
axis(4, at = yrAxis, labels = paste0(yrAxis))
#       mtext(y = yrAxis, par(usr)[1], labels = yrAxis)
mtext("Precip.", side = 4, line = 2, adj = 1)


#precipitations ggplot 
ggplot(precip_bef, aes(date, Total.Rain..mm.))+
  geom_bar(stat = 'identity', fill = "blue")+ facet_wrap(precip_bef$year)
