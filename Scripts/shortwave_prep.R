# Prepare workspace 
rm(list = ls())

# Libraries
library(tidyr)
library(tidyverse)

# load data 

df <- read.csv("Data//Raw//climate//shortwave_clean.csv")


# split Date.Time column into separate date and time columns
df_split <- separate(df, col = Date.Time, into = c("date", "time"), sep = " ")



df_split$date <- as.Date(df_split$date, format = "%Y-%m-%d")


df_split <- df_split %>% 
  group_by(date) %>% 
  summarise(shortwave = mean(Data))


ggplot(df_split, aes(x = date, y = shortwave))+
  geom_line()+
  theme_minimal()


df_split$qlambda <- df_split$shortwave * 4.57

qlambda <- df_split %>% 
  select(date, qlambda)

qlambda_2015 <- subset(qlambda, date >= as.Date("2015-05-14"))

qlamba_2015 <- 


write.csv(qlambda_2015, "Data")


