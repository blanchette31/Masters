
# Packages 
library(ggplot2)
library(gridExtra)
library(dplyr)


# Load dataframe
df = read.csv("Donnees_phenocam\\Dates_phenocam.csv")

#set columns as dates 
df$Couleurs = as.Date(df$Couleurs)
df$Perte = as.Date(df$Perte)

df$doycol = as.numeric(strftime(df$Couleurs, "%j"))
df$doycol

df$doyper = as.numeric(strftime(df$Perte, "%j"))
df$year = as.factor(df$year)


p1 =df %>%
  filter(!is.na(Couleurs)) %>% 
  mutate(name=factor(year, levels=year)) %>%
ggplot(aes(x= year)) + geom_linerange(aes(ymin = doycol, ymax = doyper),linetype=2,color="blue")+
 geom_point(aes(y=doycol),size=3,color="red")+
  geom_point(aes(y=doyper),size=3,color="red")+
  theme_bw()+ coord_flip() +labs(y= "Jour de l'année", x = "Année") +
  scale_y_continuous(limits = c(240,340), breaks = c(244, 274, 305, 335), labels = c("Septembre","Octobre","Novembre","Decembre"))# +
  #scale_x_discrete(breaks = c(2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021), labels = c("2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021"))

# Jours pour perte totale
df$temps_perte = (df$doyper - df$doycol)
df$temps_perte = as.numeric(df$temps_perte)

p2 = df %>% 
  filter(!is.na(Couleurs)) %>%
ggplot(aes(x=year)) + geom_linerange(aes(ymin=0, ymax=temps_perte), linetype=1, color= "blue")+
  geom_point(aes(y=temps_perte), size = 3, color = "red")+ theme_bw() + coord_flip()+
  labs(y= "Durée de la période de perte des feuilles (jours)", x = "Année")+scale_y_continuous(limits =c(0,70), breaks = c(0,10,20,30,40,50,60,70))+
  scale_x_discrete()

grid.arrange(p1,p2, nrow = 1)

