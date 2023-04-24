# prepare workspace 
rm(list = ls())

# load libraries 
library(tidyverse)
library(scales)
library(ggpubr)
library(MetBrewer)
library(patchwork)

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
  label = c("Sept", "Oct", "Nov")
)
lf$year <- factor(lf$year, levels = rev(levels(lf$year)))
lf$year <- factor(lf$year, levels(lf$year)[c(5,2,1,3,6,7,4)])

# Custom color palette
# custom_palette <- c("#44adce", "#8ad4dd", "#f2393a", "#1f5486", "#fde0a1", "#fe691d", "#ffc047")

p1_1 <- ggplot(lf, aes(x = doy, y = co2, color = year))+
  geom_smooth(method = lm, se = FALSE)+
  geom_point(aes(color = year), alpha = 0.3)+
  theme_classic()+
  scale_color_manual(values = met.brewer("Hiroshige", 8), name = "Year")+
  scale_x_continuous(breaks = labels_df$doy, labels = labels_df$label)+
  labs(title = "", y = expression(paste("Daily average surface water ", Delta, "CO"[2]~(mu*mol*L^-1))), x = "")+
  theme(axis.text.x = element_text(size = 16), # Increase axis tick mark text size
        axis.text.y = element_text(size = 16), # Increase axis tick mark text size
        axis.title.x = element_text(size = 18), # Increase axis labels size
        axis.title.y = element_text(size = 18),
        legend.title = element_text(size = 18), # Increase legend title size
        legend.text = element_text(size = 16))
  # annotate("text", x = 257, y = Inf, label = "A", hjust = 1.5, vjust = 1.0, fontface = "bold", size = 6)



# slope of O2 over time grouped by year

p2_1 <- ggplot(lf, aes(x = doy, y = o2, color = year))+
  geom_smooth(method = lm, se = FALSE)+
  geom_point(aes(color = year), alpha = 0.2)+
  theme_classic()+
  scale_color_manual(values = met.brewer("Hiroshige", 8))+
  scale_x_continuous(breaks = labels_df$doy, labels = labels_df$label)+
  labs(y = expression(paste("Daily average surface water ", Delta, "O"[2]~(mu*mol*L^-1))), x = "", title = "")+
  theme(axis.text.x = element_text(size = 16), # Increase axis tick mark text size
        axis.text.y = element_text(size = 16), # Increase axis tick mark text size
        axis.title.x = element_text(size = 18), # Increase axis labels size
        axis.title.y = element_text(size = 18),
        legend.title = element_text(size = 18), # Increase legend title size
        legend.text = element_text(size = 16))
  # annotate("text", x = 257, y = Inf, label = "B", hjust = 1.5, vjust = 1.0, fontface = "bold", size = 6)

p5 <- ggarrange(p1_1,p2_1, ncol = 2, common.legend = TRUE, legend = "right")

p5
ggsave("Data/Figures/presentations/co2_o2_2_panel_v6.jpg",p5, height = 6, width = 10, units = "in", dpi = 300)


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




p4 <- ggarrange(p1,p2,p3, ncol = 1, nrow = 3,  common.legend = FALSE, legend = "right")

p4


ggsave("Data/Figures/co2_o2_3_panel_v2.jpg",p4, height = 11, width = 12, units = "in", dpi = 300)                   
