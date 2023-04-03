#Carbon

#Prepare workspace
rm(list = ls())
library(tidyverse)
library(stats)
library(ggridges)
library(khroma)
library(lubridate)

#load dataframe
df = list.files(path = "Data//Raw//boue//Croche2016-2021", pattern = "*.csv", full.names = TRUE) %>% 
  lapply(read_csv) %>%
  bind_rows



#Set timestamp to correct format (still not appearing in EST though)
df$timestamp <- as.POSIXct(df$timestamp, format = "%Y-%m-%d%H:%M:%S", tz = "America/New_York")


#Extract date, year and doy from timestamp
df = df %>%
  mutate(
    date = as.Date(timestamp, format = "%Y%m%d"),
    doy = strftime(timestamp, format = "%j"),
    year = strftime(timestamp, format = "%Y"))

#ADD NOAA dataframe

noaa = read.csv("Data//Raw//climate//noaa_yearly_mean.csv", header = TRUE)

#change column names 
colnames(noaa)[2:3] = c("co2_atm", "co2_atm_unc")

#keep only years with data for buoy
years_to_keep = c(2015, 2016, 2017, 2018, 2019, 2021)

noaa = noaa[noaa$year %in% years_to_keep, ]
# Merge dataframes by year

df <- merge(df, noaa, by = "year")

#Convert year to factor levels for loops
#df$year = as.factor(df$year)


#load pheno data to specify dataframe
pheno <- read.csv("Data/Raw/phenocam/Dates_phenocam_V2.csv")
pheno$Couleurs <- as.Date(pheno$Couleurs)
pheno$Perte <- as.Date(pheno$Perte)
pheno[pheno$year == 2012,'Couleurs'] <- '2012-10-03' #pick date in between Sept 26 and Oct 10
pheno$doy_start <- as.numeric(strftime(pheno$Couleurs, '%j'))
pheno$doy_end <- as.numeric(strftime(pheno$Perte, '%j'))

#trim dataframe to study period
df <- df[df$doy >= min(pheno$doy_start) - 14 &
                df$doy <= max(pheno$doy_end, na.rm = T) + 14,]

write.csv(df, "Data//Processed//boue//boue_cut.csv", row.names = TRUE)


delta = read.csv("Data//Processed//boue//delta_co2.csv", header = TRUE)
delta = na.omit(delta)
#Select only desired columns in dataframe 
cdom = df %>%
  select(timestamp, CDOM_0.5m_ppb, date, doy, year)
cdom = na.omit(cdom)

# For loop CDOM by year
#cdom = na.omit(cdom)

levels = unique(cdom$year)

for( i in levels) {
  plot_data = cdom[cdom$year == i, ]
  print(ggplot(plot_data, aes(x = timestamp, y = CDOM_0.5m_ppb))+
    geom_line()+
    theme_minimal()+
      ggtitle(paste(i)))
}

#Select only desired columns in dataframe 
co2 = df %>%
  select(timestamp, CO2_0.5m_ppm, date, doy, year)
co2 = na.omit(co2)

# For loop CDOM by year
#co2 = na.omit(co2)


levels = unique(co2$year)

for( i in levels) {
  plot_data = co2[co2$year == i, ]
  print(ggplot(plot_data, aes(x = timestamp, y = CO2_0.5m_ppm))+
          geom_line()+
          theme_minimal()+
          ggtitle(paste(i)))
}

ggplot(df, aes(x = doy, y = CDOM_0.5m_ppb))+
  geom_line()+
  geom_point()+
  theme_bw()+
  facet_wrap(~year)+
  geom_vline(xintercept = 300, col = "red")+
  geom_vline(xintercept = 260, col = "red")

ggplot(df, aes(x = doy, y = CO2_0.5m_ppm))+
  geom_line()+
  geom_point()+
  theme_bw()+
  facet_wrap(~year)+
  geom_vline(xintercept = 300, col = "red")+
  geom_vline(xintercept = 260, col = "red")

daily_cdom = cdom %>% 
  group_by(date) %>%
  summarise(cdom = mean(CDOM_0.5m_ppb))

daily_cdom = daily_cdom %>%
  mutate(
    doy = strftime(date, format = "%j"),
    year = strftime(date, format = "%Y"))

daily_co2 = co2 %>% 
  group_by(date) %>%
  summarise(co2 = mean(CO2_0.5m_ppm),
            sd = sd(CO2_0.5m_ppm),
            median(CO2_0.5m_ppm),
            min(CO2_0.5m_ppm),
            max(CO2_0.5m_ppm))


daily_co2 = daily_co2 %>%
 mutate(
    doy = strftime(date, format = "%j"),
    year = strftime(date, format = "%Y"))


daily_cdom$doy = as.numeric(daily_cdom$doy)
daily_cdom = na.omit(daily_cdom)
ggplot(daily_cdom, aes(x = doy, y = cdom))+
  geom_line()+
  geom_point()+
  theme_bw()+
  facet_wrap(~year)+
  geom_vline(xintercept = 300, col = "red")+
  geom_vline(xintercept = 260, col = "red")

ggplot(daily_cdom, aes(x = doy, y = cdom))+
  geom_line()+
  geom_point()+
  theme_bw()+
  facet_wrap(~year)+
  geom_vline(xintercept = 300, col = "red")+
  geom_vline(xintercept = 260, col = "red")



doy_cdom = cdom %>%
  group_by(doy) %>%
  summarise(cdom = mean(CDOM_0.5m_ppb),
            min = min(CDOM_0.5m_ppb),
            max = max(CDOM_0.5m_ppb),
            median = median(CDOM_0.5m_ppb),
            sd = sd(CDOM_0.5m_ppb))
doy_cdom = doy_cdom %>%
  mutate(
    doy = strftime(date, format = "%j"),
    year = strftime(date, format = "%Y"))

doy_cdom$doy = as.numeric(doy_cdom$doy)
ggplot(doy_cdom, aes(x = doy, y = cdom))+
  geom_point()+
  geom_line()+
  geom_rect(aes(xmin = 254, xmax = 270, ymin = 0, ymax = Inf), alpha = 0.01, fill = "#FFC107")+
  geom_rect(aes(xmin = 297, xmax = 307, ymin = 0, ymax = Inf), alpha =0.01, fill = "#FFC107")+
  geom_ribbon(aes(ymin = cdom - sd, ymax = cdom + sd), fill = "gray70", alpha = 0.5)+
  theme_minimal()+
  ylab(expression(Average~CDOM~concentration~(ppb)))+
  xlab("Day of year")



co2$doy = as.numeric(co2$doy)
doy_co2 = co2 %>%
  group_by(doy) %>%
  summarise(co2 = mean(CO2_0.5m_ppm),
            min = min(CO2_0.5m_ppm),
            max = max(CO2_0.5m_ppm),
            median = median(CO2_0.5m_ppm),
            sd = sd(CO2_0.5m_ppm))

doy_co2$doy = as.numeric(doy_co2$doy)
ggplot(doy_co2, aes(x = doy, y = co2))+
       #geom_point()+
         geom_line()+
  geom_rect(aes(xmin = 254, xmax = 270, ymin = 0, ymax = Inf), alpha = 0.01, fill = "#FFC107")+
  geom_rect(aes(xmin = 297, xmax = 307, ymin = 0, ymax = Inf), alpha =0.01, fill = "#FFC107")+
  geom_ribbon(aes(ymin = co2 - sd, ymax = co2 + sd), fill = "gray70", alpha = 0.5)+
  theme_minimal()+
  ylab(expression(Average~CO[2]~concentration~(ppm)))+
  xlab("Day of year")
ggsave("Data/export/co2_doy.png")

daily_co2$doy = as.numeric(daily_co2$doy)
ggplot(daily_co2, aes(x = doy, y = co2))+
  geom_line()+
  geom_point()+
  theme_bw()+
  facet_wrap(~year)+
  geom_vline(xintercept = 300, col = "red")+
  geom_vline(xintercept = 260, col = "red")+
  ylab(expression(Average~CO[2]~concentration~(ppm)))+
  xlab("Day of year")
ggsave("Data//export/co2_conc_wrapped.jpg")

ggplot(doy_co2, aes(x = doy, y = median))+
  #geom_point()+
  geom_line()+
  geom_rect(aes(xmin = 254, xmax = 270, ymin = 0, ymax = Inf), alpha = 0.005, fill = "#FFC107")+
  geom_rect(aes(xmin = 297, xmax = 307, ymin = 0, ymax = Inf), alpha =0.005, fill = "#FFC107")+
  theme_minimal()

o2 = df %>%
  select(timestamp, DOa_0.5m_uM, date, doy, year)

o2 = na.omit(o2)

o2$date = as.Date(o2$date)
daily_o2 = o2 %>%
  group_by(date) %>%
  summarise(o2 = mean(DOa_0.5m_uM),
            sd = sd(DOa_0.5m_uM),
            median(DOa_0.5m_uM),
            min(DOa_0.5m_uM),
            max(DOa_0.5m_uM))

daily_o2 = daily_o2 %>%
  mutate(
    doy = strftime(date, format = "%j"),
    year = strftime(date, format = "%Y"))

doy_o2 = o2 %>%
  group_by(doy) %>%
  summarise(o2 = mean(DOa_0.5m_uM),
            sd = sd(DOa_0.5m_uM),
            median(DOa_0.5m_uM),
            min(DOa_0.5m_uM),
            max(DOa_0.5m_uM))
doy_o2 = doy_o2 %>%
  mutate(
    doy = strftime(date, format = "%j"),
    year = strftime(date, format = "%Y"))





doy_o2$doy = as.numeric(doy_o2$doy)
ggplot(doy_o2, aes(x = doy, y = o2))+
  geom_line()+
  geom_rect(aes(xmin = 254, xmax = 270, ymin = 0, ymax = Inf), alpha = 0.01, fill = "#FFC107")+
  geom_rect(aes(xmin = 297, xmax = 307, ymin = 0, ymax = Inf), alpha =0.01, fill = "#FFC107")+
  geom_ribbon(aes(ymin = o2 - sd, ymax = o2 + sd), fill = "gray70", alpha = 0.5)+
  theme_minimal()+
  ylab(expression(Average~O[2]~concentration~(micro~molar)))+
  xlab("Day of year")+
  ggtitle("Average surface oxygen concentration as a function of day of year")
ggsave("Data/export/o2_doy.jpg", width = 5, height = 7)

daily_o2$doy = as.numeric(daily_o2$doy)
ggplot(daily_o2, aes(x = doy, y = o2))+
  #geom_line()+
  geom_point()+
  theme_bw()+
  facet_wrap(~year)+
  geom_vline(xintercept = 300, col = "red")+
  geom_vline(xintercept = 260, col = "red")+
  ggtitle("Daily average surface oxygen concentration")+
  ylab(expression(Average~O[2]~concentration~(micro~molar)))
ggsave("Data/export/o2_daily_wrapped.jpg")  


delta$date = as.Date(delta$date)
daily_delta = delta %>%
  group_by(date) %>%
  summarise(
    delta = mean(delta.CO2),
    median = median(delta.CO2), 
    min = min(delta.CO2), 
    max = max(delta.CO2), 
    sd = sd(delta.CO2)) %>%
  mutate(
    doy = strftime(date, format = "%j"),
    year = strftime(date, format = "%Y"))

doy_delta = delta %>%
  group_by(doy) %>%
  summarise(
    delta = mean(delta.CO2),
    median = median(delta.CO2), 
    min = min(delta.CO2), 
    max = max(delta.CO2), 
    sd = sd(delta.CO2))

daily_delta$doy = as.numeric(daily_delta$doy)
ggplot(daily_delta, aes(x = doy, y = delta))+
  #geom_line()+
  geom_point()+
  theme_bw()+
  facet_wrap(~year)+
  geom_vline(xintercept = 300, col = "red")+
  geom_vline(xintercept = 260, col = "red")+
  ggtitle(expression(paste("Average ", Delta, "CO"[2]," in function of day of year")))+
  ylab(expression(paste("Average ", Delta, "CO"[2]," (", mu, "mol/L)")))
ggsave("Data//export//delta_wrapped.jpg")
doy_delta$doy = as.numeric(doy_delta$doy)
ggplot(doy_delta, aes(x = doy, y = delta))+
  geom_point()+
  geom_line()+
  geom_rect(aes(xmin = 254, xmax = 270, ymin = 0, ymax = Inf), alpha = 0.01, fill = "#FFC107")+
  geom_rect(aes(xmin = 297, xmax = 307, ymin = 0, ymax = Inf), alpha =0.01, fill = "#FFC107")+
  geom_ribbon(aes(ymin = delta - sd, ymax = delta + sd), fill = "gray70", alpha = 0.5)+
  theme_minimal()+
  ggtitle(expression(paste("Average ", Delta, "CO"[2]," in function of day of year")))+
  ylab(expression(paste("Average ", Delta, "CO"[2]," (", mu, "mol/L)")))
ggsave("Data//export//average_delta_doy.jpg")
