## prepare workspace

rm(list = ls())

## libraries
library(dplyr)
library(ggplot2)
library(ggpubr)

## data
pheno <- read.csv("Data//Processed//phenocam//pheno_clean.csv", header = TRUE)
df <- read.csv("Data//Processed//precip//precip_2011-2023.csv", header = TRUE)
air_temp <- read.csv("Data//Processed//boue//boue_merged.csv")
era5 <- read.csv("Data/Processed/precip/era5_prep.csv")
era5_buoy <- read.csv("Data/Processed/precip/era5_cut_buoy.csv", header = TRUE)

air_temp <- air_temp %>% 
  select(timestamp, airT_oC,airP_hPa)

air_temp <- air_temp %>% 
  mutate(date = strftime(timestamp, format = "%Y%m%d", tz = "EST"),
         doy = strftime(timestamp, format = "%d", tz = "EST"),
         year = strftime(timestamp, format = "%Y", tz = "EST"))

# 
# df <- df[,-1]
# 
# df_2105 <- subset(df, year == 2015)
# df <- df %>% 
#   mutate(year = strftime(df$date, format = "%Y"),
#          doy = strftime(df$date, format = "%j"))
# 
# df <- merge(df, pheno[, c("doy_start", "doy_end", "year")], by = "year")

df_ebp <- merge(era5_buoy, pheno[,c("year", 'doy_start', "doy_end")], by = "year")


df_ebp$area_ws <- 1075472+179000
df_ebp$vol_lake <- 836000

df_ebp <- df_ebp %>%
  rowwise() %>%
  mutate( period_rel_leaf_fall = case_when(
    doy >= doy_start ~ "lf",
    TRUE ~ "summer"))

sep1 <- subset(df_ebp, doy >= 244)
df_258 <- subset(df_ebp, doy >= 258)

# Calculate total volume of rain during leaf fall period for each year
total_rainfall_258 <- df_258 %>%
  group_by(year) %>%
  summarise(total_rain = sum(rain),
            total_water = sum(rain)*(1075472+179000)/1000,
            perc_lake =sum(rain)*(1075472+179000)/1000/836000*100 )

write.csv(total_rainfall_258, "Data/Processed/precip/total_rain_doy_258.csv")

# Calculate total volume of rain during leaf fall period for each year
total_rainfall_sep1 <- sep1 %>%
  group_by(year) %>%
  summarise(total_rain = sum(rain),
            total_water = sum(rain)*(1075472+179000)/1000,
            perc_lake =sum(rain)*(1075472+179000)/1000/836000*100 )

write.csv(total_rainfall_sep1, "Data/Processed/precip/total_rain_sept_1.csv")

lf <- subset(df_ebp, period_rel_leaf_fall == "lf")
summer <- subset(df_ebp, period_rel_leaf_fall == "summer")



# Calculate total volume of rain during leaf fall period for each year
total_rainfall <- lf %>%
  group_by(year) %>%
  summarise(total_rain = sum(rain),
            total_water = sum(rain)*(1075472+179000)/1000,
            perc_lake =sum(rain)*(1075472+179000)/1000/836000*100 )

p1 <- ggplot(total_rainfall, aes(x = as.factor(year), y = perc_lake))+
  geom_bar(stat = "identity", fill = "orange")+
  labs(title = "During leaf fall",
       x = "",
       y = "Total water volume (% lake volume)")+
  scale_y_continuous()+
  theme_classic()
p1

# Calculate total volume of rain during leaf fall period for each year
total_rainfall_before <- summer %>%
  group_by(year) %>%
  summarise(total_rain = sum(rain),
            total_water = sum(rain)*(1075472+179000)/1000,
            perc_lake =sum(rain)*(1075472+179000)/1000/836000*100 )

total_rainfall_before <- read.csv("Data//Processed//precip//total_rain_fall_before.csv", header =TRUE)

p2 <- ggplot(total_rainfall_before, aes(x = as.factor(year), y = perc_lake))+
  geom_bar(stat = "identity", fill = "green")+
  labs(title = "Before leaf fall",
       x = "",
       y = "Total water volume (% lake volume)")+
  scale_y_continuous()+
  theme_classic()
p2
# 
# # Calculate total volume of rain during leaf fall period for each year
# total_rainfall_after <- aft_df %>%
#   group_by(year) %>%
#   summarise(total_rain = sum(rain),
#             total_water = sum(rain)*(1075472+179000)/1000,
#             perc_lake =sum(rain)*(1075472+179000)/1000/836000*100 )
# 
# p3 <- ggplot(total_rainfall_after, aes(x = as.factor(year), y = perc_lake))+
#   geom_bar(stat = "summary", fun = "max", fill = "red")+
#   labs(title = "After leaf fall",
#        x = "Year",
#        y = "")+
#   scale_y_continuous(limits = c(0,25))+
#   theme_classic()

p4 <- ggarrange(p2, p1 ,p3, ncol = 1)
p4

ggsave("Data//Figures//rain_volume_period.jpg", p4, dpi = 300, width = 7, height = 8, units = "in")

