# prepare workspace
rm(list = ls())

library(tidyverse)
library(here)


# load data
schmidt <- read.csv(here("Data/Processed/lake/schmidt.csv"), header = TRUE)
pheno <- read.csv("Data//Processed//phenocam//pheno_clean.csv", header = TRUE)
air_temp <- read.csv("Data//Processed//boue//boue_merged.csv")

air_temp <- airtemp %>% 
  select()

df = merge(schmidt, pheno[, c("doy_start", "doy_end", "year", "duration")],
           by = "year")




df <- df %>%
  rowwise() %>%
  mutate( period_rel_leaf_fall = case_when(
    doy >= doy_start ~ "after",
    doy < (doy_start - 14) ~ "summer",
    TRUE ~ "before_leaf_fall"))

df_daily  <- df %>% 
  group_by(date,year,doy,period_rel_leaf_fall, doy_start, doy_end) %>% 
  summarise(schmidt_stability = mean(schmidt.stability, na.rm = TRUE))

df_leaf <- df[df$period_rel_leaf_fall == "after",]

df_leaf_daily <- df_leaf %>% 
  group_by(date, year, doy) %>% 
  summarise(schmidt.stability = mean(schmidt.stability, na.rm = TRUE))

lm <- lm(schmidt.stability ~ duration, data = df)


ggplot(df, aes(x = date, y = schmidt.stability))+
  geom_line()+
  facet_wrap(~year, ncol = 3)+
  theme_minimal()
