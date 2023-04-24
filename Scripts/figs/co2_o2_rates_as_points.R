# prepare workspace 
rm(list = ls())

# load libraries 
library(tidyverse)
library(scales)

# load data 

df <- read.csv("Data/Processed/combined/comp_years.csv", header = TRUE)
lf <- read.csv("Data/Processed/lake/co2_o2_post_lf.csv", header = TRUE)

# Convert 'year' to a factor for grouping in the model
lf$year <- as.factor(lf$year)

comp <- df %>% 
  select(year, delta_co2_rate, delta_o2_rate)
comp$year <- as.factor(comp$year)

labels_df <- data.frame(
  doy = c(244, 274, 305),
  label = c("September", "October", "November")
)

# Custom color palette
custom_palette <- c("#D1BBD7","#F6C141", "#CAE0AB", "#AE76A3", "#90C987","#5289C7","#7BAFDE")

p1 <- ggplot(lf, aes(x = doy, y = co2, color = year))+
  geom_smooth(method = lm, se = FALSE)+
  theme_classic()+
  scale_color_manual(values = custom_palette, name = "Year")+
  scale_x_continuous(breaks = labels_df$doy, labels = labels_df$label)+
  labs(title = "", y = expression(paste("Daily average surface water ", Delta, "CO"[2]~(mu*mol*L^-1))), x = "")+
  annotate("text", x = 257, y = Inf, label = "A", hjust = 1.5, vjust = 1.0, fontface = "bold", size = 6)



# slope of O2 over time grouped by year

p2 <- ggplot(lf, aes(x = doy, y = o2, color = year))+
  geom_smooth(method = lm, se = FALSE)+
  theme_classic()+
  scale_color_manual(values = custom_palette)+
  scale_x_continuous(breaks = labels_df$doy, labels = labels_df$label)+
  labs(y = expression(paste("Daily average surface water ", Delta, "O"[2]~(mu*mol*L^-1))), x = "", title = "")+
  annotate("text", x = 257, y = Inf, label = "B", hjust = 1.5, vjust = 1.0, fontface = "bold", size = 6)


p3 <- ggplot(comp, aes(x = year, y = delta_co2_rate)) +
  stat_summary(aes(color = "CO2")) +
  stat_summary(aes(y = delta_o2_rate, color = "O2")) +
  scale_x_discrete(labels = c("2015", "2016", "2017", "2018", "2019", "2021", "2022")) +
  scale_color_manual(values = c("CO2" = "red", "O2" = "green"),
                     name = "Gas",
                     labels = c(expression("CO"[2]~accumulation), expression("O"[2]~depletion))) +
  labs(y = expression(paste("Rate of surface gases concentration change ", (mu*mol*L^-1*day^-1))), x = "") +
  theme_classic()+
  theme(legend.text.align = 0)

p5 <- ggarrange(p1,p2, ncol = 2, common.legend = TRUE, legend = "right")
ggsave("Data/Figures/co2_o2_2_panel_v3.jpg",p5, height = 6, width = 10, units = "in", dpi = 300)


p4 <- ggarrange(p1,p2,p3, ncol = 1, nrow = 3,  common.legend = FALSE, legend = "right")

p4


ggsave("Data/Figures/co2_o2_3_panel_v2.jpg",p4, height = 11, width = 12, units = "in", dpi = 300)                   
