# Prepare workspace 
rm(list = ls())

# Load packages 
library(tidyverse)
library(showtext)

# Choose a font that supports the degree symbol
font_add("DegreeSymbol", "Arial Unicode MS")
showtext_auto()

# load data 
df = read.csv("Data//Processed//boue//delta_co2.csv", header = TRUE)



temp.in = df %>%
  select(timestamp, temp.in, date, doy, year)

temp.in= na.omit(temp.in)

temp.in$date = as.Date(temp.in$date)
daily_temp.in = temp.in %>%
  group_by(date) %>%
  summarise(temp.in = mean(temp.in),
            sd = sd(temp.in),
            median(temp.in),
            min(temp.in),
            max(temp.in))

daily_temp.in = daily_temp.in %>%
  mutate(
    doy = strftime(date, format = "%j"),
    year = strftime(date, format = "%Y"))

doy_temp.in = temp.in %>%
  group_by(doy) %>%
  summarise(temp.in = mean(temp.in),
            sd = sd(temp.in),
            median(temp.in),
            min(temp.in),
            max(temp.in))
doy_temp.in = doy_temp.in %>%
  mutate(
    doy = strftime(date, format = "%j"),
    year = strftime(date, format = "%Y"))


doy_temp.in$doy = as.numeric(doy_temp.in$doy)
ggplot(doy_temp.in, aes(x = doy, y = temp.in))+
  geom_line()+
  geom_rect(aes(xmin = 254, xmax = 270, ymin = 0, ymax = Inf), alpha = 0.01, fill = "#FFC107")+
  geom_rect(aes(xmin = 297, xmax = 307, ymin = 0, ymax = Inf), alpha =0.01, fill = "#FFC107")+
  geom_ribbon(aes(ymin = temp.in - sd, ymax = temp.in + sd), fill = "gray70", alpha = 0.5)+
  theme_minimal()+
  ylab(expression(paste("Average temperature (", degree ~ "C)")))+
  xlab("Day of year")+
  ggtitle(expression(paste("Daily average water surface temperature from 2015-2021 (  ",degree ~ "C)")))+
  theme(text = element_text(family = "Helvetica"))
ggsave("Data/export/temp.in_doy.jpg", width = 5, height = 7)

daily_temp.in$doy = as.numeric(daily_temp.in$doy)
ggplot(daily_temp.in, aes(x = doy, y = temp.in))+
  #geom_line()+
  geom_point()+
  theme_bw()+
  facet_wrap(~year)+
  geom_vline(xintercept = 300, col = "red")+
  geom_vline(xintercept = 260, col = "red")+
  ylab(expression(paste("Average temperature (", degree ~ "C)")))+
  xlab("Day of year")+
  ggtitle(expression(paste("Daily average water surface temperature from 2015-2021 (", degree ~ "C)")))+
  theme(text = element_text(family = "Helvetica"))
ggsave("Data/export/temp.in_daily_wrapped.jpg")  
