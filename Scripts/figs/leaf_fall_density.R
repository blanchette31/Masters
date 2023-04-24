# prepare workspace
rm(list = ls())


# Packages 

library(tidyverse)
library(gridExtra)
library(ggpubr)
library(cowplot)

# data

df = read.csv("Data//Processed//phenocam//pheno_clean.csv")

sub <- df %>% 
  select(c(year, doy_start, doy_end))

long <- sub %>% 
  pivot_longer(cols = starts_with("doy"),
               names_to = "moment")

long <- long %>% 
  rename(
    doy = value)

df$temps_perte = (df$doy_end - df$doy_start)
df$temps_perte = as.numeric(df$temps_perte)


#Ordonner le facteur année selon la date de perte de feuille complete
df$year <- as.factor(df$year)
df$year <- factor(df$year, levels(df$year)[order(df$year, decreasing = T)])

# Plot density
p1 <- ggplot(long, aes(x = doy, color = moment, fill = moment)) +
  geom_density(alpha = 0.5) +
  labs(title = "",
       x = "",
       y = "Density",
       color = "Moment",
       fill = "Moment") +
  scale_x_continuous(limits = c(230, 315),breaks = c(244, 274, 305), labels = c("Sep","Oct","Nov"))+
  scale_color_manual(labels = c("doy_start" = "Start of leaf fall", "doy_end" = "End of leaf fall"),
                     values = c("doy_start" = "#009688", "doy_end" = "#FFC107")) +
  scale_fill_manual(labels = c("doy_start" = "Start of leaf fall", "doy_end" = "End of leaf fall"),
                    values = c("doy_start" = "#009688", "doy_end" = "#FFC107")) +
  theme_cowplot()+
  theme(legend.position = "bottom")

p2 <- ggplot(df, aes(x=year, y = temps_perte)) +
  #geom_linerange(aes(ymin=0, ymax=temps_perte), linetype=1, color= "#009688")+
  #geom_point(aes(y=temps_perte), size = 3, color = "#FFC107")+ 
  geom_crossbar(aes(ymin = 0, ymax= temps_perte), color = "#FFC107", fill = "#FFC107", width = 0.65)+
  theme_cowplot() + coord_flip()+
  labs(y= "Duration of leaf fall (days)", x  = "")+
  scale_y_continuous(limits =c(0,70), breaks = c(0,10,20,30,40,50,60,70))+
  scale_x_discrete()+
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12))

p3 <- ggarrange(p1,p2, ncol = 2)
p3

ggsave("Data/Figures/lf_density.jpg",p3,  dpi = 300, height = 6, width = 10, units = "in")
