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
  labs(title = "", y = "", x = "")+
  theme(legend.position = "none")
  # # labs(title = "", y = expression(paste("Daily average surface water ", Delta, "CO"[2]~(mu*mol*L^-1))), x = "")+
  # theme(axis.text.x = element_text(size = 16), # Increase axis tick mark text size
  #       axis.text.y = element_text(size = 16), # Increase axis tick mark text size
  #       axis.title.x = element_text(size = 18), # Increase axis labels size
  #       axis.title.y = element_text(size = 18))
# annotate("text", x = 257, y = Inf, label = "A", hjust = 1.5, vjust = 1.0, fontface = "bold", size = 6)



# slope of O2 over time grouped by year

p2_1 <- ggplot(lf, aes(x = doy, y = o2, color = year))+
  geom_smooth(method = lm, se = FALSE)+
  geom_point(aes(color = year), alpha = 0.2)+
  theme_classic()+
  scale_color_manual(values = met.brewer("Hiroshige", 8))+
  scale_x_continuous(breaks = labels_df$doy, labels = labels_df$label)+
  labs( title = "", y = "", x = "")+
  theme(legend.position = "none")
  # labs(y = expression(paste("Daily average surface water ", Delta, "O"[2]~(mu*mol*L^-1))), x = "", title = "")+
  # theme(axis.text.x = element_text(size = 16), # Increase axis tick mark text size
  #       axis.text.y = element_text(size = 16), # Increase axis tick mark text size
  #       axis.title.x = element_text(size = 18), # Increase axis labels size
  #       axis.title.y = element_text(size = 18),
  #       legend.title = element_text(size = 18), # Increase legend title size
  #       legend.text = element_text(size = 16))
# annotate("text", x = 257, y = Inf, label = "B", hjust = 1.5, vjust = 1.0, fontface = "bold", size = 6)