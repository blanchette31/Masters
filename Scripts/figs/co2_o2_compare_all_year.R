#prepare workspace
rm(list = ls())

# load libraries
library(tidyverse)
library(ggpubr)

#load data
df <- read.csv("Data//Processed//boue//delta_co2-2_model_2015.csv", header = TRUE)
pheno <- read.csv("Data//Processed//phenocam//pheno_clean.csv", header = TRUE)

df = merge(df, pheno[, c("doy_start", "doy_end", "year")],
           by = "year")

df <- df %>%
  rowwise() %>%
  mutate( period_rel_leaf_fall = case_when(
    doy >= doy_start ~ "after",
    doy < (doy_start - 14) ~ "summer",
    TRUE ~ "before_leaf_fall"))

df_date <- df %>% 
  group_by(date, year, doy, period_rel_leaf_fall) %>% 
summarise(co2 = mean(delta.CO2),
          o2 = mean(do_deviation),
          o2_meta = mean(do_deviation_meta),
          o2_hypo = mean(do_deviation_hypo))

date_filt <- df_date %>% 
  filter(!is.na(co2) | !is.na(o2))

post_lf <- df_date[df_date$period_rel_leaf_fall == "after",]
# Calculate slope after leaf fall by year for CO2 and O2
slopes <- df_date %>%
  filter(period_rel_leaf_fall == "after") %>%
  group_by(year) %>%
  summarise(co2_slope = lm(co2 ~ doy)$coefficients[2], o2_slope = lm(o2 ~ doy)$coefficients[2])

slopes <- merge(slopes, df[,c("doy_start", "doy_end", "year")], by = "year")


summer_lf <- df_date[df_date$period_rel_leaf_fall == "summer",]

filt_doy <- date_filt %>% 
  group_by(doy) %>% 
  summarise(co2 = mean(co2, na.rm = TRUE),
            o2 = mean(o2, na.rm = TRUE))
df_june <- df_date %>% 
  filter(doy >= 152)
df_june <- merge(df_june, df[, c("doy_start", "doy_end", "year")], by = "year")


p1 <- ggplot(df_june, aes(x= doy, y = co2))+ 
  geom_line(linewidth =1.3)+
    geom_line(aes(y = o2), linewidth = 1.3)+
  geom_segment(data = df %>% filter(period_rel_leaf_fall %in% c("before_leaf_fall", "after")),
               aes(x = doy_start, xend = doy_start, y = -500, yend = 100), linetype = "dashed", color = "#FFC107", linewidth = 1.5) +
  geom_segment(data = df %>% filter(period_rel_leaf_fall %in% c("before_leaf_fall", "after")),
               aes(x = doy_end, xend = doy_end), linetype = "dashed", color = "#FFC107", linewidth = 1.5) +
  # geom_rect(aes(xmin = doy_start, xmax = doy_end, ymin = -700, ymax = 200),
            # fill = "#FFC107", alpha = 0.2)+
  scale_x_continuous(breaks = c(152, 182, 213, 244, 274, 305), labels = c("June", "July", "Aug", "Sept", "Oct", "Nov")) +
  geom_text(aes(x = 175, y = 50), label = expression(paste(Delta*CO[2])), size = 4)+
  geom_text(aes(x = 175, y = -100), label = expression(paste(Delta*O[2])), size = 4)+
  geom_hline(yintercept = 0)+
  ylab("")+
  xlab("")+
  facet_wrap(~year, ncol = 3, nrow= 3)+
theme_classic()
# Add lines representing slopes after leaf fall for CO2 and O2
# Add lines representing slopes after leaf fall for CO2 and O2
# Add lines representing slopes after leaf fall for CO2 and O2
# p1 <- p1 + geom_smooth(data = post_lf, aes(y = co2), method = "lm", se = FALSE, color = "red", linewidth = 0.8)+
#   geom_smooth(data = post_lf, aes(y = o2), method = "lm", se = FALSE, color = "blue", linewidth = 0.8)


p1

ggsave("Data//Figures//co2_o2_all_years_fix3.jpg", p1, dpi = 300, width = 10, height = 7, units = "in")

p2 <- ggplot(df_date, aes(x = co2, y = o2, color = as.factor(year)))+
  geom_point()+
  scale_color_brewer(palette = 'Set1', name = '') +
  labs(x = "CO2 departure", y = 'O2 departure') +
  scale_x_continuous(limits = c(-50, 150))+
  scale_y_continuous(limits = c(-150, 50))+
  geom_abline(intercept = 0, slope = -1, lty = 2) +
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  theme_classic()
p2

ggsave("Data//Figures//departures_all_years_fix2.jpg", p2, dpi = 300, width = 7, height = 5, units = "in")

p3 <- ggplot(df_date, aes(x= doy, y = o2, color = "red"))+
  geom_line(linewidth =1.3)+
  geom_line(aes(y = o2_meta), linewidth = 1.3, color = "blue")+
  geom_line(aes(y = o2_hypo), linewidth = 1.3, color = "green")+
  geom_rect(aes(xmin = 254, xmax = 270, ymin = -Inf, ymax = Inf), alpha = 0.01, fill = "#FFC107")+
  geom_rect(aes(xmin = 297, xmax = 307, ymin = -Inf, ymax = Inf), alpha =0.01, fill = "#FFC107")+
  # geom_text(aes(x = 175, y = 75), label = expression(paste(Delta*CO[2])), size = 3)+
  # geom_text(aes(x = 175, y = -100), label = expression(paste(Delta*O[2])), size = 3)+
  geom_hline(yintercept = 0)+
  ylab("")+
  facet_wrap(~year, ncol = 3, nrow= 3)+
  theme_classic()+
  theme(legend.position = "none")
p3

ggsave("Data//Figures//o2_departures_all_depths_fix2.jpg", p3, dpi = 300, width = 7, height = 5, units = "in")



p4 <- ggplot(date_filt, aes(x= doy, y = co2))+
  geom_line(linewidth =1.3)+
  geom_line(aes(y = o2), linewidth = 1.3)+
  geom_rect(aes(xmin = 254, xmax = 270, ymin = -Inf, ymax = Inf), alpha = 0.01, fill = "#FFC107")+
  geom_rect(aes(xmin = 297, xmax = 307, ymin = -Inf, ymax = Inf), alpha =0.01, fill = "#FFC107")+
  geom_text(aes(x = 175, y = 75), label = expression(paste(Delta*CO[2])), size = 3)+
  geom_text(aes(x = 175, y = -100), label = expression(paste(Delta*O[2])), size = 3)+
  geom_hline(yintercept = 0)+
  ylab("")+
  facet_wrap(~year, ncol = 3, nrow= 3)+
  theme_classic()
p4

ggsave("Data//Figures//co2_o2_departures_filtered_na_fix2.jpg", p4, dpi = 300, width = 7, height = 5, units = "in")


p5 <- ggplot(df_date, aes(x = co2, y = o2, color = as.factor(period_rel_leaf_fall)))+
  geom_point()+
  scale_color_brewer(palette = 'Set1', name = '') +
  labs(x = "CO2 departure", y = 'O2 departure') +
  scale_x_continuous(limits = c(-50, 150))+
  scale_y_continuous(limits = c(-150, 50))+
  geom_abline(intercept = 0, slope = -1, lty = 2) +
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  facet_wrap(~year, ncol = 3, nrow = 3)+
  theme_classic()
p5
ggsave("Data//Figures//co2_departures_by_period_fix2.jpg", p5, dpi = 300, width = 7, height = 5, units = "in")


p6 <- ggplot(date_filt, aes(x = co2, y = o2))+
  geom_point()+
  geom_smooth(method = "lm",se = FALSE)+
  scale_color_brewer(palette = 'Set1', name = '') +
  labs(x = "CO2 departure", y = 'O2 departure') +
  scale_x_continuous(limits = c(-50, 150))+
  scale_y_continuous(limits = c(-150, 50))+
  geom_abline(intercept = 0, slope = -1, lty = 2) +
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  facet_wrap(~year, ncol = 3, nrow = 3)+
  theme_classic()

slope_text <- date_filt %>%
  group_by(year) %>%
  do(slope = coef(lm(o2 ~ co2, data = .))[2]) %>%
  mutate(label = paste("Slope =", round(slope, 3)))

p6 <- p6 + geom_text(data = slope_text, aes(label = label), x = 50, y = 25)
p6

ggsave("Data//Figures//co2_o2_departures_slope_fix2.jpg", p6, dpi = 300, width = 10, height = 7, units = "in")



p7 <- ggplot(post_lf, aes(x = co2, y = o2, color = as.factor(year)))+
  geom_point()+
  scale_color_brewer(palette = 'Set1', name = '') +
  geom_smooth(method = "lm", se = FALSE,color = "grey40")+
  labs(x = "CO2 departure", y = 'O2 departure') +
  scale_x_continuous(limits = c(-50, 150))+
  scale_y_continuous(limits = c(-150, 50))+
  geom_abline(intercept = 0, slope = -1, lty = 2) +
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  theme_classic()+
  labs(title = "Departures after start of leaf fall")+
  facet_wrap(~year, ncol = 3, nrow=3)


slope_text_post_lf <- post_lf %>%
  group_by(year) %>%
  do(slope = coef(lm(o2 ~ co2, data = .))[2]) %>%
  mutate(label = paste("Slope =", round(slope, 3)))

p7 <- p7 + geom_text(data = slope_text_post_lf, aes(label = label), x = 50, y = 25, color = "black")
p7

ggsave("Data//Figures//co2_o2_departures_after_leaf_fall_fix2.jpg", p7, dpi = 300, width = 10, height = 7, units = "in")




p8 <- ggplot(summer_lf, aes(x = co2, y = o2, color = as.factor(year)))+
  geom_point()+
  scale_color_brewer(palette = 'Set1', name = '') +
  geom_smooth(method = "lm", se = FALSE, color = "grey40")+
  labs(x = "CO2 departure", y = 'O2 departure') +
  scale_x_continuous(limits = c(-50, 150))+
  scale_y_continuous(limits = c(-150, 50))+
  geom_abline(intercept = 0, slope = -1, lty = 2) +
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  theme_classic()+
  labs(title = "Departures during summer")+
  facet_wrap(~year, ncol = 3, nrow=3)


slope_text_summer_lf <- summer_lf %>%
  group_by(year) %>%
  do(slope = coef(lm(o2 ~ co2, data = .))[2]) %>%
  mutate(label = paste("Slope =", round(slope, 3)))

p8 <- p8 + geom_text(data = slope_text_summer_lf, aes(label = label), x = 50, y = 25, color = "black")
p8

ggsave("Data//Figures//co2_o2_departures_summer_fix2.jpg", p8, dpi = 300, width = 10, height = 7, units = "in")

p9 <- ggarrange(p8,p7, ncol = 2, nrow = 1)
p9
ggsave("Data//Figures//co2_o2_departures_summer_leaf_fall_compare_fix2.jpg", p9, dpi = 300, width = 14, height = 10, units = "in")



p10 <- ggplot(date_filt, aes(x = co2, y = o2, color = as.factor(period_rel_leaf_fall)))+
  geom_point()+
  scale_color_brewer(palette = 'Set1', name = '') +
  labs(x = "CO2 departure", y = 'O2 departure') +
  scale_x_continuous(limits = c(-50, 150))+
  scale_y_continuous(limits = c(-150, 50))+
  geom_abline(intercept = 0, slope = -1, lty = 2) +
  geom_vline(xintercept = 0)+
  geom_hline(yintercept = 0)+
  theme_classic()
p10
ggsave("Data//Figures//co2_departures_by_period_combined_fix2.jpg", p10, dpi = 300, width = 7, height = 5, units = "in")



p11 <- ggplot(filt_doy, aes(x = as.numeric(doy), y = co2))+
  geom_rect(aes(xmin = 244, xmax = 270, ymin = -Inf, ymax = Inf), alpha = 0.01, fill = "#FFC107")+
  geom_rect(aes(xmin = 297, xmax = 307, ymin = -Inf, ymax = Inf), alpha =0.01, fill = "#FFC107")+
  geom_line(linewidth = 2)+
  geom_line(aes(y = o2), linewidth = 2)+
  # geom_ribbon(aes(ymin = co2- co2_sd, ymax = co2 + co2_sd), fill = "#757575", alpha = 0.5)+
  # geom_ribbon(aes(ymin = o2 - o2_sd, ymax = o2 + o2_sd), fill = "#757575", alpha = 0.5)+
  geom_text(aes(x = 240, y = 50), label = expression(paste(Delta*CO[2])), size = 7)+
  geom_text(aes(x = 240, y = -75), label = expression(paste(Delta*O[2])), size = 7)+
  geom_text(aes(x= 256, y = 85), label = "Range of start of \n leaf colour change")+
  geom_text(aes(x = 302, y = 85), label = "Range of end \n of leaf fall")+
  scale_y_continuous(limits = c(-125, 100))+
  scale_x_continuous(limits = c(230, 315),
                     expand = c(0,0),
                     breaks = c(244, 274, 305),
                     labels = c("September","October","November"))+
  geom_hline(yintercept = 0)+
  ylab(expression(paste("Average daily deviation from saturation ( ", mu,"mol/L)")))+
  xlab("Day of the year")+ 
  theme_classic()+
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

# Calculate slopes for co2 and o2 from day 270 to day 297
co2_slope <- lm(co2 ~ doy, data = filter(filt_doy, doy >= 270, doy <= 304))$coefficients[2]
o2_slope <- lm(o2 ~ doy, data = filter(filt_doy, doy >= 270, doy <= 304))$coefficients[2]

p11
ggsave("Data//Figures//co2_o2_departures_doy.jpg", p11, dpi = 300, width = 7, height = 5, units = "in")

write.csv(post_lf, "Data//Processed//lake//co2_o2_post_lf.csv")
