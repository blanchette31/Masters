


# Packages 
rm(list = ls())
library(tidyverse)
library(gridExtra)
library(ggpubr)


# Load dataframe
df = read.csv("Data//Processed//phenocam//pheno_clean.csv")

#set columns as dates 
df$Couleurs = as.Date(df$Couleurs)
df$Perte = as.Date(df$Perte)

df$doycol = as.numeric(strftime(df$Couleurs, "%j"))
df$doycol

df$doyper = as.numeric(strftime(df$Perte, "%j"))
df$year = as.factor(df$year)

#Enlever 2012
df <- df %>%
  filter(!is.na(Couleurs))

#Enlever 2011 
df = subset(df, year != "2011")
df = subset(df, year != "2012")
df = subset(df, year != "2013")
df = subset(df, year != "2014")
df = subset(df, year != "2020")


#Ordre dans lequel les années apparaissent sur le graphique
levels(df$year)
#Enlever le niveau 2012
df$year <- droplevels(df$year)

#Ordonner le facteur année selon la date de perte de feuille complete
df$year <- factor(df$year, levels(df$year)[order(df$year, decreasing = T)])
  
p1 <- ggplot(df, aes(y= year)) + 
  geom_linerange(aes(xmin = doycol, xmax = doyper),linetype=1,color="#009688") +
  geom_point(aes(x=doycol),size=3,color="#FFC107")+
  geom_point(aes(x=doyper),size=3,color="#FFC107")+
  theme_classic()+ 
  labs(x = "Period of leaf fall", y = "") +
  scale_x_continuous(limits = c(240,340), breaks = c(244, 274, 305, 335), 
                     labels = c("Sep","Oct","Nov","Dec"))+
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12))


# Jours pour perte totale
df$temps_perte = (df$doyper - df$doycol)
df$temps_perte = as.numeric(df$temps_perte)

p2 <- ggplot(df, aes(x=year, y = temps_perte)) +
  #geom_linerange(aes(ymin=0, ymax=temps_perte), linetype=1, color= "#009688")+
  #geom_point(aes(y=temps_perte), size = 3, color = "#FFC107")+ 
  geom_crossbar(aes(ymin = 0, ymax= temps_perte), color = "#FFC107", fill = "#FFC107", width = 0.65)+
  theme_classic() + coord_flip()+
  labs(y= "Duration of leaf fall (days)", x  = "")+
  scale_y_continuous(limits =c(0,70), breaks = c(0,10,20,30,40,50,60,70))+
  scale_x_discrete()+
  theme(plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12))
 
p3 = ggarrange(p1, p2, ncol = 2, nrow =1)
p3
ggsave("Data//Figures//leaf_fall_period.png", plot = p3, bg = "transparent", dpi = 600, height = 5, width = 7)

#Distribution de la date de début de couleur et perte de feuille
df_long <- df[,c("year","doycol","doyper")] %>% 
  pivot_longer(cols = 2:3, names_to = "type", values_to = "doy")

ggplot(df_long, aes(x = doy, color = type, fill = type)) +
  geom_density(alpha=0.5) +
  theme_bw()

#Date de perte de feuille selon le temps
df$year <- as.numeric(as.character(df$year))
ggplot(df, aes(x = year, y = doycol)) +
  geom_point() + 
  geom_smooth(se = F) +
  theme_bw()
  


