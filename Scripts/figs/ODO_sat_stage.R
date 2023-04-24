## prepare workspace
rm(list = ls())

# libraries 
library(tidyverse)
library(MetBrewer)
library(cowplot)
library(readxl)


#data

df <- read_csv("Data/Processed/profil/profils_o2_stages.csv")
df$year <- as.factor(df$year)


col_ind <- c("#44adce", "#8ad4dd", "#f2393a", "#1f5486", "#fde0a1", "#fe691d", "#ffc047")

ggplot(df, aes(x = `ODOsat (%)`, y = `Profondeur (m)`, color = year, group = year))+
  geom_path()+
scale_y_continuous(trans = "reverse")+
  scale_color_manual(values = col_ind)+
  theme_cowplot()+
  labs(x = "Dissolved oxygen (% saturation)", y = "Depth (m)")+
  theme(legend.position = "none")

ggsave("Data/Figures/presentations/o2_sat_august.jpg", dpi = 300, width = 5, height = 7, units = "in")
