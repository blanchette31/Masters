## Code for ~6 panel environmental data to differentiate years ## 


# prepare workspace
rm(list = ls())

# libraries
library(tidyverse)
library(cowplot)
library(viridis)


# load data
df <- read.csv("Data/Processed/combined/comp_years.csv")
# era5 <- read.csv("Data/Processed/precip/era5_cut_buoy.csv") #era5 climate data cut to fit buoy data
era5_lf <- read.csv("Data/Processed/precip/era5_cut_lf.csv") # era5 climate data cut for leaf fall 
# era5_sum <- read.csv("Data/Processed/precip/era5_cut_summer.csv") # era5 climate data cut for summer

# sum_df <- df %>% 
  
df$year <- as.factor(df$year)

# Reorder levels of year variable
df$year <- factor(df$year, levels = c(2022:2015))

era5_lf$year <- as.factor(era5_lf$year)
era5_lf$year <- factor(era5_lf$year, levels = c(2022:2015))

# Define fill colors 
fill_colors <- rainbow(length(unique(df$year)))


# Plot loss of thermal stability
p1 <- ggplot(df, aes(x = year, y = schmidt_rate_leaf_fall)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = schmidt_rate_leaf_fall)) +
  geom_point(shape = 21, color = "black", fill = fill_colors, size = 4) + 
  coord_flip() +
  theme_cowplot() +
  labs(title = "", y = "Rate of loss of thermal stability (J/m^2/d)", x = "") +
  scale_y_continuous(limits = c(0, 2), expand = expansion(mult = c(0, 0.05)))+
  theme(axis.line = element_line(color = "black", size = 1.2),
        axis.ticks = element_line(color = "black", size = 1.2),
        axis.ticks.length = unit(0.25, "cm"))

p1


# Plot hypoxic duration
p2 <- ggplot(df, aes(x = year, y = hypoxic_dur)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = hypoxic_dur)) +
  geom_point(shape = 21, color = "black", fill = fill_colors, size = 4) + 
  coord_flip() +
  theme_cowplot() +
  labs(title = "", y = "Hypoxic duration (days)", x = "") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))+
  theme(axis.line = element_line(color = "black", size = 1.2),
        axis.ticks = element_line(color = "black", size = 1.2),
        axis.ticks.length = unit(0.25, "cm"))

p2


# Plot volume exchanged
p3 <- ggplot(df, aes(x = year, y = vol_hypo_exch)) +
  geom_segment(aes(x = year, xend = year, y = 40000, yend = vol_hypo_exch)) +
  geom_point(shape = 21, color = "black", fill = fill_colors, size = 4) + 
  coord_flip() +
  theme_cowplot() +
  labs(title = "", y = "Mixing volume (m^3)", x = "") +
  scale_y_continuous(limits = c(40000, 160000), expand = expansion(mult = c(0, 0.05)))+
  theme(axis.line = element_line(color = "black", size = 1.2),
        axis.ticks = element_line(color = "black", size = 1.2),
        axis.ticks.length = unit(0.25, "cm"))

p3

# Plot cummulative precipitations
p4 <- ggplot(df, aes(x = year, y = vol_rain_fall_perc)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = vol_rain_fall_perc)) +
  geom_point(shape = 21, color = "black", fill = fill_colors, size = 4) + 
  coord_flip() +
  theme_cowplot() +
  labs(title = "", y = "Cum. volume of rain (% lake volume)", x = "") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))+
  theme(axis.line = element_line(color = "black", size = 1.2),
        axis.ticks = element_line(color = "black", size = 1.2),
        axis.ticks.length = unit(0.25, "cm"))

p4


# Plot temperature during fall

p5 <- ggplot(era5_lf, aes(x = year, y = air_temp.C., group = year))+
  geom_boxplot(fill = fill_colors)+
  coord_flip() +
  theme_cowplot() +
  labs(title = "", y = "Daily temperature (C)", x = "") +
  theme(axis.line = element_line(color = "black", size = 1.2),
        axis.ticks = element_line(color = "black", size = 1.2),
        axis.ticks.length = unit(0.25, "cm"))
p5  

# Plot wind during fall 

p6 <- ggplot(era5_lf, aes(x = year, y = wind_sp.m.s.,))+
  geom_boxplot(aes(group = year), fill = fill_colors)+
  coord_flip() +
  theme_cowplot() +
  labs(title = "", y = "Wind speed (m/s)", x = "") +
  theme(axis.line = element_line(color = "black", size = 1.2),
        axis.ticks = element_line(color = "black", size = 1.2),
        axis.ticks.length = unit(0.25, "cm"))
p6  


#Grid for export

p7 <- plot_grid(p1, p2, p3, p4, p5, p6, ncol = 3,labels = "auto")
p7

ggsave("Data/Figures/6_panel_external_v1.jpg", dpi = 300, width = 10, height = 6, units = "in")
