#Runoff ratio

#Prepare workspace
rm(list = ls())
library(tidyverse)
library(stats)
library(ggridges)
library(khroma)
library(agricolae)
library(scales)


#Open data
sth <- read.csv("Data/Processed/precip/precip_brief.csv")
sth$date <- as.Date(sth$date)
q <- read.csv("Data/Processed/debit/debit_2011-2022.csv")
q$date <- as.Date(q$date)
pheno <- read.csv("Data/Raw/phenocam/Dates_phenocam_V2.csv")
pheno$Couleurs <- as.Date(pheno$Couleurs)
pheno$Perte <- as.Date(pheno$Perte)
pheno[pheno$year == 2012,'Couleurs'] <- '2012-10-03' #pick date in between Sept 26 and Oct 10
pheno$doy_start <- as.numeric(strftime(pheno$Couleurs, '%j'))
pheno$doy_end <- as.numeric(strftime(pheno$Perte, '%j'))
nasad = read.csv("Data/Processed/precip/precip_2011-2023.csv")


#Create one data frame for discharge and precipitation data
df <- merge(sth[,c('date','Total.Rain..mm.')], 
            q[,c('date','year','doy','debit_total_m3_jour')], 
            by = c('date'))

df <- df[,c(1,3,4,2,5)]
colnames(df)[c(4,5)] <- c('rain','q')

df_nasa = merge(nasad[,c("doy","year","rain")],
                q[,c('date','year','doy','debit_total_m3_jour')], 
                by = c("year", "doy"))
df_nasa = df_nasa[order(as.Date(df_nasa$date, format="%Y-%m-%d")),]
colnames(df_nasa)[c(3,5)] = c('rain', "q")

nasa_rain <- df_nasa %>% 
  select(date, rain)

write.csv(nasa_rain, "Data//Processed//precip//nasa_daily.csv")

#Subset data frame for 2 weeks before earliest color change and latest leaf fall
#df <- df[df$doy >= min(pheno$doy_start) - 14 &
#df$doy <= max(pheno$doy_end, na.rm = T) + 14,]

df <- df_nasa[df_nasa$doy >= min(pheno$doy_start) - 14 &
                df_nasa$doy <= max(pheno$doy_end, na.rm = T) + 14,]

df = merge(df[,], pheno[, c("doy_start", "doy_end", "year")],
           by = "year")


df$vol_rain = (1075472+179000) * df$rain /1000
df$year = as.factor(df$year)

#NEW factor level column before during and after leaf fall

df = df %>% 
 rowwise() %>%
  mutate(period  = 
        case_when(doy < doy_start ~ "before", 
                               doy > doy_end ~  "after",
                               TRUE ~ "during"))

df = df[!(df$rain == 0),]

df = df %>%
  mutate(runoff_ratio = q / (rain*1075472/1000)*0.25)

df$period = factor(df$period, levels = c("before", "during", "after"))

df$year = as.factor(df$year)

df = na.omit(df)

ggplot(df, aes(x = year, y = runoff_ratio, fill = period))+
  geom_boxplot()+ 
  #facet_wrap(~year)+
  theme_minimal()+
  scale_y_log10()

ggplot(df, aes(x = year, y = rain, fill = period))+
  geom_boxplot()+ 
  #facet_wrap(~year)+
  theme_minimal()
  
ggplot(df, aes(x = year, y = q, fill = period))+
  geom_boxplot()+ 
  #facet_wrap(~year)+
  theme_minimal()

ggplot(df, aes(x = period, y = q))+
  geom_boxplot()+
  facet_wrap(~year)+
  theme_minimal()

#Runoff Ratio wrapped by year
ggplot(df, aes(x = period, y = runoff_ratio, fill = period))+
  geom_boxplot()+
  facet_wrap(~year)+
  theme_minimal()


#All years combined
options(scipen = 999)
df$runoff_ratio_perc <- df$runoff_ratio * 100
ggplot(df, aes(x = period, y = runoff_ratio_perc, fill = period))+
  geom_violin()+
  geom_boxplot(width = 0.35)+
  theme_classic()+
  scale_y_log10(breaks = c(0.0001, 0.001, 0.01, 0.1, 1, 10, 100),
    labels = c("0.0001", "0.001", "0.01", "0.1", "1", "10", "100"))+
  annotation_logticks(sides = "l")+
  scale_fill_manual(values = c("#009688","#FFC107","#FFA000" ), guide = FALSE)+
  scale_x_discrete(labels = c("Before", "During", "After"))+
  labs(y = "Runoff Ratio (%)", x = "Period relative to leaf fall")+
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16))

  
ggsave("Presentations/GRIL-SCAS/figures/runoff_ratio.png", dpi = 300 ,bg = "transparent", width = 10, height = 7)
ggsave("Data//Figures//runoff_ratio.png", dpi = 300, width = 10, height = 7, units = "in")

# Enlever 2 valeurs les plus élevées pour mieux voir

df_test = df[!(df$runoff_ratio == max(df$runoff_ratio)),]
df_test2 = df_test[!(df_test$runoff_ratio == max(df_test$runoff_ratio)),]

ggplot(df_test2, aes(year, y = runoff_ratio, fill = period))+
  geom_boxplot()+
  theme_minimal()

ggplot(df_test2, aes(year, y = q, fill = period))+
  geom_boxplot()+
  theme_minimal()

ggplot(df_test2, aes(year, y = rain, fill = period))+
  geom_boxplot()+
  theme_minimal()

ggplot(df_test2, aes(period, y = runoff_ratio, fill = period))+
  geom_boxplot()+
  theme_minimal()+
  scale_color_manual("004488", "DDAA33", "BB5566")

ggplot(df_test2, aes(period, y = runoff_ratio))+
  geom_boxplot()+
  theme_minimal()+
  facet_wrap(~year)

ggplot(df, aes(x = runoff_ratio, y = period))+
  geom_density_ridges()+
  theme_minimal()+
  facet_wrap(~year)

anova_runoff = aov(runoff_ratio ~ period + year, data = df)
summary(anova_runoff)

# test assumptions of ANOVA

shapiro.test(log(df$runoff_ratio)) # ok 
bartlett.test(log(runoff_ratio) ~ period, data = df) # ok 

# Anova test for runoff per period

runoff_per = aov(log(runoff_ratio_perc) ~ period, data = df)
summary(runoff_per)
TukeyHSD(runoff_per)

by(df, df$year, function(x){
  anova(lm(log(runoff_ratio) ~ period, data = x))$"Pr(>F)"[1]
  })


by(df, df$year, function(x){
  anova(lm(q ~ period, data = x))$"Pr(>F)"[1]
})


by(df, df$year, function(x){
  anova(lm(rain ~ period, data = x))$"Pr(>F)"[1]
})



# average runoff ratio percentages per period
average <- df %>% 
  group_by(period) %>% 
  summarize(runoff_ratio_perc = mean(runoff_ratio_perc),
            max = max(runoff_ratio_perc),
            min = min(runoff_ratio_perc))
print(average)


