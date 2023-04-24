## Code for ~6 panel environmental data to differentiate years ## 


# prepare workspace
rm(list = ls())

# libraries
library(tidyverse)
library(cowplot)
library(viridis)
library(GGally)
library(MetBrewer)
theme_set(theme_minimal())

# load data
df <- read.csv("Data/Processed/combined/comp_years.csv")
# era5 <- read.csv("Data/Processed/precip/era5_cut_buoy.csv") #era5 climate data cut to fit buoy data
era5_lf <- read.csv("Data/Processed/precip/era5_cut_lf.csv") # era5 climate data cut for leaf fall 
# era5_sum <- read.csv("Data/Processed/precip/era5_cut_summer.csv") # era5 climate data cut for summer

#Put year as a factor
df$year <- as.factor(df$year)
era5_lf$year <- as.factor(era5_lf$year)
era5_lf$year <- factor(era5_lf$year, levels = c(2022:2015))
df <- df[df$year != 2020, ]

# ##CHECK CORRELATION PATTERN##
# library(GGally)
# ggpairs(df, columns = c(8:11,13:14))
# 
# #Check temp and rain
# ggplot(df, aes(x=avg_temp_fall, y = vol_rain_fall_perc, color = year, label=year)) +
#   geom_point() +
#   geom_text(hjust=0,vjust=0) +
#   scale_color_manual(values = met.brewer("Hiroshige",8)) # This is awesome!
# #*Color years by temperature during fall*#

cor.test(df$avg_temp_fall, df$vol_rain_fall_perc)

df$year <- droplevels(df$year)
# df$year <- factor(df$year, levels = c(2022:2015))
df$year <- factor(df$year, levels = rev(levels(df$year)))
df$year <- factor(df$year, levels(df$year)[c(5,2,1,3,6,7,4)])



##PANNEL 1: TEMP & RAIN CORRELATION
p0 <- ggplot(df, aes(x=avg_temp_fall, y = vol_rain_fall_perc, fill = year, label=year)) +
  geom_point(pch=21, size = 7) +
  geom_text(aes(color = year), hjust=0.5,vjust=-1, fontface = 'bold', size = 12) +
  scale_fill_manual(values = met.brewer("Hiroshige",8)) + # This is awesome!
  scale_color_manual(values = met.brewer("Hiroshige",8)) +
  scale_y_continuous(limits = c(NA,32))+
  labs(x = 'Mean fall air temperature (°C)', y = 'Cumulative fall rain (% lake volume)') +
  theme_classic() +
  theme(legend.position = "none",
        axis.line = element_line(color = "black", size = 1.2),
        axis.ticks = element_line(color = "black", size = 1.2),
        axis.ticks.length = unit(0.25, "cm"),
        axis.text.x = element_text(size = 30), # Increase axis tick mark text size
        axis.text.y = element_text(size = 30), # Increase axis tick mark text size
        axis.title.x = element_text(size = 30), # Increase axis labels size
        axis.title.y = element_text(size = 30))

p0


# ggsave("Data/Figures/presentations/temp_rain_corr_v2.jpg", p0, dpi = 300, width = 10, height = 8, units = "in")


# Plot loss of thermal stability
p1 <- ggplot(df, aes(x = year, y = schmidt_rate_leaf_fall,fill = year)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = schmidt_rate_leaf_fall)) +
  geom_point(shape = 21, color = "black",  size = 4) + 
  coord_flip() +
  theme_cowplot() +
  labs(title = "", y = expression(paste("Thermal stability loss (J m"^{-2}, " d"^{-1}, ")")), x = "") +
  scale_y_continuous(limits = c(0, 2), expand = expansion(mult = c(0, 0.05)))+
  scale_fill_manual(values = met.brewer("Hiroshige",8)) +
  theme(legend.position = "none",
        axis.line = element_line(color = "black", size = 1.2),
        axis.ticks = element_line(color = "black", size = 1.2),
        axis.ticks.length = unit(0.25, "cm"),
        axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 30), # Increase axis tick mark text size
        axis.title.x = element_text(size = 30, margin = margin(t = 10, r = 0, b = 0, l = -20), hjust = 0),
        axis.title.y = element_text(size = 30))

p1


# Plot hypoxic duration
p2 <- ggplot(df, aes(x = year, y = hypoxic_dur, fill = year)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = hypoxic_dur)) +
  geom_point(shape = 21, color = "black", size = 4) + 
  coord_flip() +
  theme_cowplot() +
  labs(title = "", y = "Hypoxic duration (days)", x = "") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))+
  scale_fill_manual(values = met.brewer("Hiroshige",8)) +
  theme(legend.position = "none",
        axis.line = element_line(color = "black", size = 1.2),
        axis.ticks = element_line(color = "black", size = 1.2),
        axis.ticks.length = unit(0.25, "cm"),
        axis.text.x = element_text(size = 30), # Increase axis tick mark text size
        axis.text.y = element_text(size = 30), # Increase axis tick mark text size
        axis.title.x = element_text(size = 30), # Increase axis labels size
        axis.title.y = element_text(size = 30))

p2


# Plot cumulative radiation
p8 <- ggplot(df, aes(x = year, y = cum_rad_fall, fill = year)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = cum_rad_fall)) +
  geom_point(shape = 21, color = "black", size = 4) + 
  coord_flip() +
  theme_cowplot() +
  labs(title = "", y = expression("Cum. radiation (MJ m"^{-2} * ")"), x = "") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))+
  scale_fill_manual(values = met.brewer("Hiroshige",8)) +
  theme(legend.position = "none",
        axis.line = element_line(color = "black", size = 1.2),
        axis.ticks = element_line(color = "black", size = 1.2),
        axis.ticks.length = unit(0.25, "cm"),
        axis.text.x = element_text(size = 30), # Increase axis tick mark text size
        axis.text.y = element_text(size = 30), # Increase axis tick mark text size
        axis.title.x = element_text(size = 30), # Increase axis labels size
        axis.title.y = element_text(size = 30))

p8

# Plot volume exchanged
p3 <- ggplot(df, aes(x = year, y = vol_hypo_exch, fill = year)) +
  geom_segment(aes(x = year, xend = year, y = 40000, yend = vol_hypo_exch)) +
  geom_point(shape = 21, color = "black", size = 4) + 
  coord_flip() +
  theme_cowplot() +
  labs(title = "", y = "Mixing volume (m^3)", x = "") +
  scale_y_continuous(limits = c(40000, 160000), expand = expansion(mult = c(0, 0.05)))+
  scale_fill_manual(values = met.brewer("Hiroshige",8)) +
  theme(legend.position = "none",
        axis.line = element_line(color = "black", size = 1.2),
        axis.ticks = element_line(color = "black", size = 1.2),
        axis.ticks.length = unit(0.25, "cm"))

p3

# Plot cummulative precipitations
p4 <- ggplot(df, aes(x = year, y = vol_rain_fall_perc, fill = year)) +
  geom_segment(aes(x = year, xend = year, y = 0, yend = vol_rain_fall_perc)) +
  geom_point(shape = 21, color = "black", size = 4) + 
  coord_flip() +
  theme_cowplot() +
  labs(title = "", y = "Cum. volume of rain (% lake volume)", x = "") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))+
  scale_fill_manual(values = met.brewer("Hiroshige",8)) +
  theme(legend.position = "none",
        axis.line = element_line(color = "black", size = 1.2),
        axis.ticks = element_line(color = "black", size = 1.2),
        axis.ticks.length = unit(0.25, "cm"))

p4


# Plot temperature during fall
era5_lf$year <- droplevels(era5_lf$year)
era5_lf$year <- factor(era5_lf$year, levels(era5_lf$year)[c(5,2,1,3,6,7,4)])

p5 <- ggplot(era5_lf, aes(x = year, y = air_temp.C., fill = year))+
  geom_boxplot()+
  coord_flip() +
  theme_cowplot() +
  labs(title = "", y = "Daily temperature (C)", x = "") +
  scale_fill_manual(values = met.brewer("Hiroshige",8)) +
  theme(legend.position = "none",
        axis.line = element_line(color = "black", size = 1.2),
        axis.ticks = element_line(color = "black", size = 1.2),
        axis.ticks.length = unit(0.25, "cm"))
p5  

# Plot wind during fall 

p6 <- ggplot(era5_lf, aes(x = year, y = wind_sp.m.s., fill = year))+
  geom_boxplot()+
  coord_flip() +
  theme_cowplot() +
  labs(title = "", y = expression("Wind speed (m " * s^-1 * ")"), x = "") +
  scale_fill_manual(values = met.brewer("Hiroshige",8)) +
  theme(legend.position = "none",
        axis.line = element_line(color = "black", size = 1.2),
        axis.ticks = element_line(color = "black", size = 1.2),
        axis.ticks.length = unit(0.25, "cm"),
        axis.text.x = element_text(size =30), # Increase axis tick mark text size
        axis.text.y = element_text(size = 30), # Increase axis tick mark text size
        axis.title.x = element_text(size = 30), # Increase axis labels size
        axis.title.y = element_text(size = 30))
p6  

# ggsave("Data/Figures/presentations/wind_speed.jpg", p6, dpi = 300, width = 7, height = 5, units = "in")


#Grid for export
# 
# p7 <- plot_grid(p1, p2, p3, p4, p5, p6, ncol = 2,labels = "auto")
# p7 

# ggsave("Data/Figures/6_panel_external_v1.jpg", dpi = 300, width = 12, height = 8, units = "in")

lay <- rbind(c(1,2,3),
             c(1,4,5))

p9 <- gridExtra::grid.arrange(p0, p6,p1,p8,p2, layout_matrix = lay, widths = c(0.38,0.31,0.31))



ggsave("Data/Figures/comments/5_panel_external_fonts_increased.jpg",p9, dpi = 300, width = 20, height = 12, units = "in")


shapiro.test(era5_lf$wind_sp.m.s.)
kruskal.test(wind_sp.m.s. ~ year, data = era5_lf)

