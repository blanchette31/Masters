#prepare workspace
rm(list = ls())

#libraries
library(tidyverse)

#data 
era5 <- read.csv("Data/Processed/precip/era5_prep.csv")
pheno <- read.csv("Data/Processed/phenocam/pheno_clean.csv")

# Filter to exclude everything before doy 121
era5 <- era5 %>%
  filter(doy >= 121, doy <=307)


df = merge(era5, pheno[, c("doy_start", "doy_end", "year")],
           by = "year")

df <- df %>%
  rowwise() %>%
  mutate( period_rel_leaf_fall = case_when(
    doy >= doy_start ~ "leaf_fall",
    doy < (doy_start - 14) ~ "summer",
    TRUE ~ "before_leaf_fall"),
    PAR = global_rad.MJ.m2. * 0.48)

# Calculate cumulative sum of global radiation for each year
df <- df %>%
  group_by(year) %>%
  mutate(cumulative_rad = cumsum(global_rad.MJ.m2.),
         cumulative_air_temp = cumsum(air_temp.C.))

sum_bef_df <- subset(df, period_rel_leaf_fall == c("summer", "before_leaf_fall"))
lf <- subset(df, period_rel_leaf_fall == "leaf_fall")

lf <- lf %>%
  group_by(year) %>%
  mutate(cumulative_rad = cumsum(global_rad.MJ.m2.),
         cumulative_temp = cumsum(air_temp.C.),
         cumulative_wind = cumsum(wind_sp.m.s.),
         max_wind = max(wind_sp.m.s.),
         mean_wind = mean(wind_sp.m.s.))


ggplot(df, aes(x = doy, y = global_rad.MJ.m2.))+
  geom_line(aes(color = period_rel_leaf_fall))+
  theme_classic()+
  facet_wrap(~year, ncol = 3)

ggplot(df, aes(x = doy, y = cumulative_rad))+
  geom_line(aes(color = period_rel_leaf_fall))+
  theme_classic()+
  facet_wrap(~year, ncol = 3)

ggplot(sum_bef_df, aes(x = doy, y = cumulative_rad))+
  geom_line(aes(color = period_rel_leaf_fall))+
  theme_classic()+
  facet_wrap(~year, ncol = 3)

ggplot(lf, aes(x = doy, y = global_rad.MJ.m2.))+
  geom_line()+
  theme_classic()+
  facet_wrap(~year, ncol = 3)

ggplot(df, aes(x = doy, y = air_temp.C.))+
  geom_line(aes(color = period_rel_leaf_fall))+
  theme_classic()+
  facet_wrap(~year, ncol = 3)

ggplot(sum_bef_df, aes(x = doy, y = air_temp.C.))+
  geom_line()+
  theme_classic()+
  facet_wrap(~year, ncol = 3)

ggplot(sum_bef_df, aes(x = doy, y = cumulative_air_temp))+
  geom_line(aes(color = period_rel_leaf_fall))+
  theme_classic()+
  facet_wrap(~year, ncol = 3)

# Display total sum of global radiation for each year in a table
summary_table <- df %>%
  group_by(year) %>%
  summarize(total_rad = sum(air_temp.C.),
            average_temp = mean(air_temp.C.),
            total_rain = sum(rain))

print(summary_table)

sum_table <- sum_bef_df %>%
  group_by(year) %>%
  summarize(total_temp = sum(air_temp.C.),
            average_temp = mean(air_temp.C.),
            total_rain = sum(rain),
            total_rad = sum(global_rad.MJ.m2.))

print(sum_table)

# summary table leaf fall

sum_table_lf <- lf %>%
  group_by(year) %>%
  summarize(total_temp = sum(air_temp.C.),
            average_temp = mean(air_temp.C.),
            total_rain = sum(rain),
            total_rad = sum(global_rad.MJ.m2.),
            total_wind = sum(wind_sp.m.s.),
            mean_wind = mean(wind_sp.m.s.),
            max_wind = max(wind_sp.m.s.))

print(sum_table_lf)


write.csv(sum_table_lf, "Data/Processed/combined/summary_era5_leaf_fall.csv")

# summary table leaf fall 2017 - 2022 modified dates 

# Apply filtering for 2017 and 2022 separately
lf_cut1722 <- lf %>%
  filter(
    !(year == 2017 & doy < 273),  # Exclude rows before DOY 273 in 2017
    !(year == 2022 & doy < 257)   # Exclude rows before DOY 257 in 2022
  )

# Generate a summary table for the filtered data
summary_table_cutlf_1722 <- lf_cut1722 %>%
  group_by(year) %>%
  summarize(
    total_temp = sum(air_temp.C., na.rm = TRUE),
    average_temp = mean(air_temp.C., na.rm = TRUE),
    total_rain = sum(rain, na.rm = TRUE),
    total_rad = sum(global_rad.MJ.m2., na.rm = TRUE),
    mean_wind = mean(wind_sp.m.s.),
    max_wind = max(wind_sp.m.s.)
  )

# Print and save the summary table
print(summary_table_cutlf_1722)
write.csv(summary_table_cutlf_1722, "Data/Processed/combined/summary_filtered_lf_cut1722_era5.csv")

