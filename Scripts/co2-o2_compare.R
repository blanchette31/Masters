# Prepare workspace 
rm(list = ls())

# Libraries 
library(tidyverse)
library(ggpubr)

# Load data

df = read.csv("Data//Processed//boue//delta_co2.csv", header = TRUE)
pheno = read.csv("Data//Processed//phenocam//pheno_clean.csv", header = TRUE)


#Merge dataframes 

df = merge(df, pheno[, c("doy_start", "doy_end", "year")],
           by = "year")

#NEW factor level column before during and after leaf fall

df = df %>% 
  rowwise() %>%
  mutate(period  = 
           case_when(doy < doy_start ~ "before", 
                     doy >= doy_start ~  "after"))

df$period = factor(df$period, levels = c("before", "after"))




df_daily = df %>%
  group_by(date, year, doy, period) %>%
summarise(co2 = mean(delta.CO2),
          o2 = mean(do_deviation))

levels = unique(df_daily$period)

for (i in levels){
  
plot_data = df_daily[df_daily$period == i, ]
  
  print(ggplot(plot_data, aes(y = o2 , x = co2, color = as.factor(year))) +
    geom_point() +
    scale_color_brewer(palette = 'Set1', name = '') +
    labs(x = "CO2 departure", y = 'O2 departure') +
    scale_x_continuous(limits = c(-50, 150))+
    scale_y_continuous(limits = c(-150, 50))+
    geom_abline(intercept = 0, slope = -1, lty = 2) +
    geom_vline(xintercept = 0)+
    geom_hline(yintercept = 0)+
    annotate("text", x = 125, y = -120, hjust = 0, label = "1:1")+
    ggtitle(paste("CO2-O2 departure", i, "start of leaf colour change"))+
    theme_classic())
  
  ggsave(paste0("Data/export/co2-o2_departure_", i, ".png"), width = 6, height = 6, units = "in")
  
}



ggplot(df_daily, aes(x = doy, y = o2))+
  geom_point(color = "#FFC107")+
  geom_point(aes(y = co2), color = "#009688")+
  ggtitle("daily delta CO2 and O2")+
  ylab("delta")+
  geom_hline(yintercept = 0)+
  guides(color = guide_legend(title = "Gas"))+
  facet_wrap(~as.factor(year))+
  theme_bw()
ggsave("Data/export/daily_delta_co2_o2.png", width = 7, height = 5, units = "in")

df = na.omit(df)
df_doy = df %>%
  group_by(doy) %>%
  summarise(co2 = mean(delta.CO2),
            o2 = mean(do_deviation),
            co2_sd = sd(delta.CO2),
            o2_sd = sd(do_deviation))


ggplot(df_doy, aes(x = as.numeric(doy), y = co2))+
  geom_rect(aes(xmin = 254, xmax = 270, ymin = -Inf, ymax = Inf), alpha = 0.01, fill = "#FFC107")+
  geom_rect(aes(xmin = 297, xmax = 307, ymin = -Inf, ymax = Inf), alpha =0.01, fill = "#FFC107")+
  geom_line(linewidth = 2)+
  geom_line(aes(y = o2), linewidth = 2)+
  geom_ribbon(aes(ymin = co2- co2_sd, ymax = co2 + co2_sd), fill = "#757575", alpha = 0.5)+
  geom_ribbon(aes(ymin = o2 - o2_sd, ymax = o2 + o2_sd), fill = "#757575", alpha = 0.5)+
  geom_text(aes(x = 240, y = 50), label = expression(paste(Delta*CO[2])), size = 7)+
  geom_text(aes(x = 240, y = -75), label = expression(paste(Delta*O[2])), size = 7)+
  #geom_text(aes(x= 262, y = 85), label = "Range of start of \n leaf colour change")+
  #geom_text(aes(x = 302, y = 85), label = "Range of end \n of leaf fall")+
  scale_y_continuous(limits = c(-125, 100))+
  scale_x_continuous(limits = c(230, 315),
                     expand = c(0,0),
                     breaks = c(244, 274, 305),
                     labels = c("September","October","November"))+
  geom_hline(yintercept = 0)+
  ylab(expression(paste("Aaverage deviation from saturation ( ", mu,"mol/L)")))+
  xlab("Day of the year")+ 
  theme_classic()+
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))
ggsave("Presentations/GRIL-SCAS/figures/co2_o2_compare.png", dpi = 600, bg = "transparent", width = 7, height = 5 )
