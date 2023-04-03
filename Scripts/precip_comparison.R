#Compare precipitation at St-Hippolyte and SBL

#Prepare workspace
rm(list = ls())
library(tidyverse)
library(zoo)
library(RcppRoll)

sth <- read.csv("Data//Processed//precip//precip_brief.csv")
sth$date <- as.Date(sth$date)
sth$Year <- as.numeric(sth$Year)
sbl <- read.csv("Data//Processed//precip//precip_sbl_clean.csv")
sbl$doy <- as.numeric(as.character(sbl$doy))
sbl <- sbl[,c('year','doy','time','precip')]
sbl$year <- as.numeric(as.character(sbl$year))
pheno <- read.csv("Data//Raw//phenocam//Dates_phenocam_V2.csv")
pheno$Couleurs <- as.Date(pheno$Couleurs)
pheno$Perte <- as.Date(pheno$Perte)
pheno[pheno$year == 2012,'Couleurs'] <- '2012-10-03' #pick date in between Sept 26 and Oct 10
pheno$doy_start <- as.numeric(strftime(pheno$Couleurs, '%j'))
pheno$doy_end <- as.numeric(strftime(pheno$Perte, '%j'))
nasa <- read.csv("Data//Processed//precip//precip_nasa_bound.csv")

#Calculate daily precipitation for sbl
sbl$precip <- as.numeric(as.character(sbl$precip))
sbld <- sbl %>% 
  dplyr::group_by(year, doy) %>% 
  dplyr::summarise(rain_sbl = sum(precip))

#Merge data frame
colnames(sth)[3:6] <- c('year','month','day','rain_sth')
sth$doy <- as.numeric(strftime(sth$date, '%j'))
df <- merge(sth[,c('date','year','doy','rain_sth')], 
            sbld,
            by = c('year','doy'), all = T)

df <- merge(df, nasa[,c("year", "doy", "rain_nasa")],
            by = c("year", "doy"), all = T)

df$year <- as.numeric(df$year)
df <- df[!is.na(df$year),] #remove NA in year

#Split dataframe into list of dataframes split by year 
dfl = df %>%
  group_split(year)

#dataframe with all years 
dfa = df[!(df$year %in% "2022"),]

dfa = na.omit(dfa)

#Keep only year with data from SBL
unique(sbl$year)

df <- df[df$year < 2016,]

df <- df[!(df$year %in% "2022"),]

df = na.omit(df)

#Subset data frame for 2 weeks before earliest color change and latest leaf fall
df <- df[df$doy >= min(pheno$doy_start) - 14 &
           df$doy <= max(pheno$doy_end, na.rm = T) + 14,]

#Look at data
ggplot(df, aes(y = rain_sth, x = rain_nasa, color = as.factor(year))) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  scale_color_brewer(palette = 'Set1', name = '') +
  labs(x = "Rain Nasa", y = 'Rain Saint-Hippolyte (mm)') +
  theme_minimal()
ggsave('../export/precip_nasa_vs_sth_scatterplot.pdf', device = 'pdf', width = 5, height = 4)

ggplot(df, aes(y = rain_nasa, x = rain_sth, color = as.factor(year))) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  scale_color_brewer(palette = 'Set1', name = '') +
  labs(x = 'Rain Saint-Hippolyte (mm)', y = 'Rain Data Rod Nasa (mm)') +
  theme_minimal()
ggsave('../export/precip_sbl_vs_sth_scatterplot.pdf', device = 'pdf', width = 5, height = 4)

ggplot(df, aes(y = rain_nasa, x = rain_sbl, color = as.factor(year))) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  scale_color_brewer(palette = 'Set1', name = '') +
  labs(x = 'Rain SBL (mm)', y = 'Rain Data Rod Nasa (mm)') +
  theme_minimal()
ggsave('Data/export/precip_sbl_vs_nasa_scatterplot.jpg', device = 'jpeg', width = 5, height = 4)


ggplot(df) +
  geom_bar(aes(x = doy, y = rain_sbl), color = 'black', stat = 'identity') +
  geom_bar(aes(x = doy, y = rain_nasa), fill = 'red', stat = 'identity', alpha = 0.7) + 
  labs(y = 'Rain (mm)') +
  facet_wrap(~year) +
  theme_minimal()
ggsave('Data/export/precip_sbl_vs_nasa_temporalplot.jpg', device = 'jpeg', width = 8, height = 5)


for(i in 1:length(dfl)){
  rowsum(dfl[[i]]$rain_nasa,rep(1:4, each=3 ))
}
base::rowsum(df,rep(1:5,each=4))

grp = (1:nrow(df)-1)%/%3
data.frame(range = aggregate(df$doy, list(grp), 
                             function(x) paste(range(x), collapse=" -- "))$x,
           sum = aggregate(df$rain_nasa, list(grp), sum$x),
           stringsAsFactors = FALSE)

vect_nasa = c()

for(i in 4:nrow(df)){
  vect_nasa[i] = sum(df[(i-3),6], df[(i-2),6], df[(i-1),6], df[i,6])
}

df$roll_nasa = vect_nasa



vec = vect

nth_element <- function(vector, starting_position, n) { 
  vector[seq(starting_position, length(vector), n)] 
}

nasa_nth = nth_element(vec, 4, 4)

test = sum(df$rain_nasa[1],df$rain_nasa[2], df$rain_nasa[3], df$rain_nasa[4])
test

date_vec = dplyr::pull(df, date)

vec = date_vec

date_nth = nth_element(vec, 4, 4)

vect_sbl = c()

for(i in 4:nrow(df)){
  vect_sbl[i] = sum(df[(i-3),5], df[(i-2),5], df[(i-1),5], df[i,5])
}

df$roll_sbl = vect_sbl
vec = vect

sbl_nth = nth_element(vec, 4, 4)

doy_vec = dplyr::pull(df, doy)
vec = doy_vec

doy_nth = nth_element(vec, 4, 4)

df_roll= na.omit(df)

df_nth = data.frame(date_nth, doy_nth,  nasa_nth, sbl_nth)

df_nth$year = format(df_nth$date_nth, format= "%Y")

ggplot(df_nth) +
  geom_bar(aes(x = doy_nth, y = sbl_nth), color = 'black', stat = 'identity') +
  geom_bar(aes(x = doy_nth, y = nasa_nth), fill = 'red', stat = 'identity', alpha = 0.7) + 
  labs(y = 'Rain (mm)', x = "Day of year") +
  facet_wrap(~year) +
  theme_minimal()
 
ggplot(df_nth, aes(y = nasa_nth, x = sbl_nth, color = as.factor(year))) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  scale_color_brewer(palette = 'Set1', name = '') +
  labs(x = 'Rain SBL (mm)', y = 'Rain Data Rod Nasa (mm)') +
  theme_minimal()

ggplot(df_roll) +
  geom_bar(aes(x = doy, y = roll_sbl), color = 'black', stat = 'identity') +
  geom_bar(aes(x = doy, y = roll_nasa), fill = 'red', stat = 'identity', alpha = 0.7) + 
  labs(y = 'Rain (mm)') +
  facet_wrap(~year) +
  ggtitle("Rolling sum") +
  theme_minimal()
ggsave('Data/export/temporal_rolling_sum.jpg', device = 'jpeg', width = 5, height = 4)

ggplot(df_roll, aes(y = roll_nasa, x = roll_sbl, color = as.factor(year))) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, lty = 2) +
  scale_color_brewer(palette = 'Set1', name = '') +
  labs(x = 'Rain SBL (mm)', y = 'Rain Data Rod Nasa (mm)') +
  ggtitle("Rolling sum")+ 
  scale_y_continuous(limits = c(0,75), breaks = c(0, 25, 50, 75, 100), labels = c(0, 25, 50, 75, 100))+
  scale_x_continuous(limits = c(0,75), breaks = c(0, 25, 50, 75, 100), labels = c(0, 25, 50, 75, 100))+
  theme_minimal()
ggsave('Data/export/scatter_rolling_sum.jpg', device = 'jpeg', width = 5, height = 4)



cor(df_roll$roll_nasa, df_roll$roll_sbl)
cor(df$rain_nasa, df$rain_sbl)

require(devtools)
install_github("MichelNivard/gptstudio")

Sys.setenv(OPENAI_API_KEY = "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")

require(usethis)
edit_r_environ(scope = "user")

