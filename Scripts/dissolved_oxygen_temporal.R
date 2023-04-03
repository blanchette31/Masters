# Prepare workspace 
rm(list = ls())

# Load packages 
library(tidyverse)

# load data 
df = read.csv("Data//Processed//boue//delta_co2.csv", header = TRUE)



do_sat = df %>%
  select(timestamp, do_sat_perc, date, doy, year)

do_sat= na.omit(do_sat)

do_sat$date = as.Date(do_sat$date)
daily_do_sat = do_sat %>%
  group_by(date) %>%
  summarise(do_sat = mean(do_sat_perc),
            sd = sd(do_sat_perc),
            median(do_sat_perc),
            min(do_sat_perc),
            max(do_sat_perc))

daily_do_sat = daily_do_sat %>%
  mutate(
    doy = strftime(date, format = "%j"),
    year = strftime(date, format = "%Y"))

doy_do_sat = do_sat %>%
  group_by(doy) %>%
  summarise(do_sat = mean(do_sat_perc),
            sd = sd(do_sat_perc),
            median(do_sat_perc),
            min(do_sat_perc),
            max(do_sat_perc))
doy_do_sat = doy_do_sat %>%
  mutate(
    doy = strftime(date, format = "%j"),
    year = strftime(date, format = "%Y"))


doy_do_sat$doy = as.numeric(doy_do_sat$doy)
ggplot(doy_do_sat, aes(x = doy, y = do_sat))+
  geom_line()+
  geom_rect(aes(xmin = 254, xmax = 270, ymin = 0, ymax = Inf), alpha = 0.01, fill = "#FFC107")+
  geom_rect(aes(xmin = 297, xmax = 307, ymin = 0, ymax = Inf), alpha =0.01, fill = "#FFC107")+
  geom_ribbon(aes(ymin = do_sat - sd, ymax = do_sat + sd), fill = "gray70", alpha = 0.5)+
  theme_minimal()+
  ylab(expression(paste("Average surface dissolved oxygen saturation level (%)")))+
  xlab("Day of year")+
  ggtitle(expression(paste(" Surface daily average dissolved oxygen saturation level (%)")))+
  theme(text = element_text(family = "Helvetica"))
ggsave("Data/export/do_sat_doy.jpg", width = 5, height = 7)

daily_do_sat$doy = as.numeric(daily_do_sat$doy)
ggplot(daily_do_sat, aes(x = doy, y = do_sat))+
  #geom_line()+
  geom_point()+
  theme_bw()+
  facet_wrap(~year)+
  geom_vline(xintercept = 300, col = "red")+
  geom_vline(xintercept = 260, col = "red")+
  ylab(expression(paste("Average surface dissolved oxygen saturation level (%)")))+
  xlab("Day of year")+
  ggtitle(expression(paste(" Surface daily average dissolved oxygen saturation level (%)")))+
  theme(text = element_text(family = "Helvetica"))
ggsave("Data/export/do_sat_daily_wrapped.jpg")  


##  Dissolved oxygen deviation ##
do_dev = df %>%
  select(timestamp, do_deviation, date, doy, year)

do_dev= na.omit(do_dev)

do_dev$date = as.Date(do_dev$date)
daily_do_dev = do_dev %>%
  group_by(date) %>%
  summarise(do_dev = mean(do_deviation),
            sd = sd(do_deviation),
            median(do_deviation),
            min(do_deviation),
            max(do_deviation))

daily_do_dev = daily_do_dev %>%
  mutate(
    doy = strftime(date, format = "%j"),
    year = strftime(date, format = "%Y"))

doy_do_dev = do_dev %>%
  group_by(doy) %>%
  summarise(do_dev = mean(do_deviation),
            sd = sd(do_deviation),
            median(do_deviation),
            min(do_deviation),
            max(do_deviation))
doy_do_dev = doy_do_dev %>%
  mutate(
    doy = strftime(date, format = "%j"),
    year = strftime(date, format = "%Y"))




doy_do_dev$doy = as.numeric(doy_do_dev$doy)
ggplot(doy_do_dev, aes(x = doy, y = do_dev))+
  geom_line()+
  geom_rect(aes(xmin = 254, xmax = 270, ymin = -Inf, ymax = Inf), alpha = 0.01, fill = "#FFC107")+
  geom_rect(aes(xmin = 297, xmax = 307, ymin = -Inf, ymax = Inf), alpha =0.01, fill = "#FFC107")+
  geom_ribbon(aes(ymin = do_dev - sd, ymax = do_dev + sd), fill = "gray70", alpha = 0.5)+
  theme_minimal()+
  ylab(expression(paste("Average surface dissolved oxygen deviation ( ", mu, "mol/L)")))+
  xlab("Day of year")+
  ggtitle(expression(paste(" Surface daily average dissolved oxygen deviation (  ", mu, " mol/L)")))+
  theme(text = element_text(family = "Helvetica"))
ggsave("Data/export/do_dev_doy.jpg", width = 5, height = 7)

daily_do_dev$doy = as.numeric(daily_do_dev$doy)
ggplot(daily_do_dev, aes(x = doy, y = do_dev))+
  #geom_line()+
  geom_point()+
  theme_bw()+
  facet_wrap(~year)+
  geom_vline(xintercept = 300, col = "red")+
  geom_vline(xintercept = 260, col = "red")+
  ylab(expression(paste("Average surface dissolved oxygen deviation (" , mu, "mol/L)")))+
  xlab("Day of year")+
  ggtitle(expression(paste(" Surface daily average dissolved oxygen deviation ( ", mu, "mol/L)")))+
  theme(text = element_text(family = "Helvetica"))
ggsave("Data/export/do_dev_daily_wrapped.jpg") 
