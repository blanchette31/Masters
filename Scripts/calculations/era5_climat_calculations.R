# prepare workspace
rm(list = ls())

# load libraries 
library(tidyverse)

# load data 
era5 <- read.csv("Data/Processed/precip/era5_prep.csv")
pheno <- read.csv("Data/Processed/phenocam/pheno_clean.csv")

df = merge(era5, pheno[, c("doy_start", "doy_end", "year")],
           by = "year")

df <- df %>%
  rowwise() %>%
  mutate( period_rel_leaf_fall = case_when(
    doy >= doy_start ~ "leaf_fall",
    doy < (doy_start - 14) ~ "summer",
    TRUE ~ "before_leaf_fall"),
    PAR = global_rad.MJ.m2. * 0.48)

# Filter to exclude everything before doy 121 and after doy 307
df_filtered <- df %>%
  filter(doy >= 213, doy <= 307)

# Subset into two new dataframes
df_summer <- df_filtered %>%
  filter(doy >= 213, period_rel_leaf_fall %in% c("summer", "before_leaf_fall"))

df_after_leaf_fall <- df_filtered %>%
  filter(period_rel_leaf_fall == "leaf_fall")

# Calculate cumulative sum of global radiation for each year
df_filtered <- df_filtered %>%
  group_by(year) %>%
  mutate(cum = cumsum(PAR))
df_after_leaf_fall<- df_after_leaf_fall %>%
  group_by(year) %>%
  mutate(cum_par_lf = cumsum(PAR))

# Plot global radiation by day of year
ggplot(df_filtered, aes(x = doy, y = global_rad.MJ.m2.)) +
  geom_line(aes(color = period_rel_leaf_fall)) +
  theme_classic() +
  facet_wrap(~year, ncol = 3)

# Plot accumulated global radiation by day of year
ggplot(df_filtered, aes(x = doy, y = cumulative_PAR)) +
  geom_line(aes(color = period_rel_leaf_fall)) +
  theme_classic() +
  facet_wrap(~year, ncol = 3)

# Display total sum of global radiation for each year in a table
summary_table <- df_filtered %>%
  group_by(year) %>%
  summarize(total_PAR = sum(PAR))

print(summary_table)

ggplot(df_summer, aes(x = doy, y = rain))+
  geom_line()+
  theme_classic()+
  facet_wrap(~year, ncol = 3)

ggplot(df_summer, aes(x = doy, y = wind_sp.m.s.))+
  geom_line()+
  theme_classic()+
  facet_wrap(~year, ncol = 3)

summer_stats <- df_summer %>% 
  group_by(year) %>% 
  summarise(rain_mean = mean(rain),
            rain_sum = sum(rain),
            wind_mean = mean(wind_sp.m.s.),
            wind_sum = sum(wind_sp.m.s.),
            rad_sum = sum(global_rad.MJ.m2.))

fall_stats <- df_after_leaf_fall %>% 
  group_by(year) %>% 
  summarise(rain_mean = mean(rain),
            rain_sum = sum(rain),
            wind_mean = mean(wind_sp.m.s.),
            wind_sum = sum(wind_sp.m.s.),
            rad_sum = sum(global_rad.MJ.m2.))

summer_stats$year <- as.factor(summer_stats$year)

ggplot(summer_stats, aes(x = year, y = rain_sum))+
  geom_col()+
  theme_classic()+
  scale_x_discrete(breaks = c(2015,2016,2017,2018,2019,2021,2022), labels = c(2015,2016,2017,2018,2019, 2021,2022))+
  labs( y = "cummulative rain (mm)", x = "")

ggplot(summer_stats, aes(x = year, y = wind_sum))+
  geom_col()+
  theme_classic()+
  scale_x_discrete(breaks = c(2015,2016,2017,2018,2019,2021,2022), labels = c(2015,2016,2017,2018,2019, 2021,2022))+
  labs( y = "cummulative wind (m/s)", x = "")
