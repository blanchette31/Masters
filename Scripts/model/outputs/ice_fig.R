## ice formation model ##

#prepare workspace # 

rm(list = ls())

library(tidyverse)
library(ggpubr)

#load data

df <- read.csv("Data//model//results//ice_model.csv", header = TRUE)
dur <- read.csv("Data//model//results//Ice_duration.csv", header = TRUE)


df$ice_off <-  as.Date(df$ice_off)
df$ice_on <- as.Date(df$ice_on)

df$doyoff = as.numeric(strftime(df$ice_off, "%j"))
df$doyoff

df$doyon = as.numeric(strftime(df$ice_on, "%j"))
df$year = as.factor(df$year)

#Ordonner le facteur année selon la date de perte de feuille complete
df$year <- factor(df$year, levels(df$year)[order(df$year, decreasing = T)])


p1 <- ggplot(df, aes(y = year)) + 
  geom_linerange(aes(xmin = doyon, xmax = doyoff),linetype=1,color="#3182bd") +
  geom_point(aes(x=doyon),size=3,color="#9ecae1")+
  geom_point(aes(x=doyoff),size=3,color="#9ecae1")+
  theme_classic()+ 
  labs(x = "Ice on period", y = "") +
  scale_x_continuous()+
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12))

dur$Winter <- as.factor(dur$Winter)

dur$Winter <- factor(dur$Winter, levels(dur$Winter)[order(dur$Winter, decreasing = T)])

mean(dur$Duration)


# Create a plot
p1 <- ggplot(dur, aes(x = Winter, y = Duration)) +
  geom_bar(stat = "identity", fill = "#9ecae1") +
  labs(
    y = "Duration (days)", x = ""
  ) + scale_y_continuous(limits = c(0,200),
                         breaks = seq(0, 200, by = 50))+
  theme_classic()+
  coord_flip()+
  geom_hline(yintercept = mean(dur$Duration), linetype = "dashed", color = "blue")



p2 <- ggplot(dur, aes(y = Winter))+
  geom_linerange(aes(xmin = start, xmax = end),linetype=1,color="#9ecae1") +
  geom_point(aes(x=start),size=3,color="blue")+
  geom_point(aes(x=end),size=3,color="blue")+
  labs(x = "")+
  scale_x_continuous(limits = c(0,213),breaks = c(1, 31, 62, 93, 121, 152, 182, 213), 
                     labels = c("Nov", "Dec", "Jan", "Feb", "March", "April", "May", "June"))+
  theme_classic()+
  theme(panel.grid.major.x = element_line(color = "grey70", linetype = "dashed", size = 0.5), axis.text.x = element_text(angle = 30, hjust = 1) )+
  labs(y= "", x = "")+
  geom_vline(xintercept = 62, color = "grey30", linetype = "dashed")


p3 <- ggarrange(p2, p1, ncol = 2, nrow =1)

ggsave("Data//Figures//model_ice.jpg", p3, dpi = 600)

