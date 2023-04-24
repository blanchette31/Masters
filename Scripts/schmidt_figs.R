# prepare workspace
rm(list = ls())

library(tidyverse)
library(here)


# load data
schmidt <- read.csv(here("Data/Processed/lake/schmidt.csv"), header = TRUE)

# subset for each year
schmidt_2021 <- subset(schmidt, year(datetime) == 2021)
schmidt_2019 <- subset(schmidt, year(datetime) == 2019)
schmidt_2018 <- subset(schmidt, year(datetime) == 2018)
schmidt_2017 <- subset(schmidt, year(datetime) == 2017)
schmidt_2016 <- subset(schmidt, year(datetime) == 2016)

schmidt <- schmidt %>% 
  group_by(doy, year) %>% 
  summarise(schmidt.stability = mean(schmidt.stability))

#Vector for xaxis (month and weeks)
month_tick <- as.numeric(strftime(c('2011-05-01','2011-06-01','2011-07-01','2011-08-01', '2011-09-01', '2011-10-01', "2011-11-01"),'%j'))
month_label <- c('May','June','July','Aug','Sept', 'Oct', 'Nov')


ggplot(schmidt, aes(x = doy, y = schmidt.stability, color = factor(year)))+
        geom_smooth(method = "gam", se = FALSE) +
         ggtitle("Schmidt Stability by Year") +
         ylab("Schmidt Stability") +
  scale_x_continuous(breaks = month_tick, labels = month_label)+ 
         scale_color_discrete(name = "Year", labels = c("2016", "2017", "2018", "2019", "2021"))+
        theme_classic()


