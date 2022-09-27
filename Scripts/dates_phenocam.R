
# Packages 
library(ggplot2)
library(gridExtra)
library(dplyr)


# Load dataframe
df = read.csv("Data\\Raw\\phenocam\\Dates_phenocam_V2.csv")

#set columns as dates 
df$Couleurs = as.Date(df$Couleurs)
df$Perte = as.Date(df$Perte)

df$doycol = as.numeric(strftime(df$Couleurs, "%j"))
df$doycol

df$doyper = as.numeric(strftime(df$Perte, "%j"))
df$year = df %>%
  rename(year =ï..year )
df$year = as.factor(df$year)

write.csv(df, "Data//Processed//phenocam//dates_phenocam.csv", row.names = T)

df_pheno = read.csv("Data//Processed//phenocam//Dates_phenocam.csv")


#Enlever 2012
df_pheno <- df_pheno %>%
  filter(!is.na(Couleurs))

#Ordre dans lequel les années apparaissent sur le graphique
levels(df_pheno$year)
#Enlever le niveau 2012
df_pheno$year <- droplevels(df_pheno$year)

#Ordonner le facteur année selon la date de perte de feuille complete
df_pheno$year <- factor(df_pheno$year, levels(df_pheno$year)[order(df_pheno$doyper, decreasing = T)])

p1 <- ggplot(df_pheno, aes(y= year)) + 
  geom_linerange(aes(xmin = doycol, xmax = doyper),linetype=2,color="blue") +
  geom_point(aes(x=doycol),size=3,color="red")+
  geom_point(aes(x=doyper),size=3,color="red")+
  theme_bw()+ 
  labs(x = "", y = "") +
  scale_x_continuous(limits = c(240,340), breaks = c(244, 274, 305, 335), 
                     labels = c("Septembre","Octobre","Novembre","Decembre"))# +
#scale_x_discrete(breaks = c(2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021), labels = c("2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021"))

# Jours pour perte totale
df_pheno$temps_perte = (df$doyper - df$doycol)
df_pheno$temps_perte = as.numeric(df$temps_perte)

p2 <- ggplot(df, aes(x=year)) + geom_linerange(aes(ymin=0, ymax=temps_perte), linetype=1, color= "blue")+
  geom_point(aes(y=temps_perte), size = 3, color = "red")+ theme_bw() + coord_flip()+
  labs(y= "Durée de la période de perte des feuilles (jours)")+scale_y_continuous(limits =c(0,70), breaks = c(0,10,20,30,40,50,60,70))+
  scale_x_discrete()

grid.arrange(p1,p2, nrow = 1)

#Distribution de la date de début de couleur et perte de feuille
df_long <- df[,c("year","doycol","doyper")] %>% 
  pivot_longer(cols = 2:3, names_to = "type", values_to = "doy")

ggplot(df_long, aes(x = doy, color = type, fill = type)) +
  geom_density(alpha=0.5) +
  theme_bw()

#Date de perte de feuille selon le temps
df$year <- as.numeric(as.character(df$year))
ggplot(df, aes(x = year, y = doyper)) +
  geom_point() + 
  geom_smooth(se = F) +
  theme_bw()

####fin#####



p1 = df_pheno %>%
 dplyr:: filter(!is.na(Couleurs)) %>% 
 #dplyr::mutate(name=factor(year, levels=year)) %>%
ggplot(aes(x= year)) + geom_linerange(aes(ymin = doycol, ymax = doyper),linetype=2,color="blue")+
 geom_point(aes(y=doycol),size=3,color="red")+
  geom_point(aes(y=doyper),size=3,color="red")+
  theme_bw()+ coord_flip() +labs(y= "Jour de l'année", x = "Année") +
  scale_y_continuous(limits = c(240,340), breaks = c(244, 274, 305, 335), labels = c("Septembre","Octobre","Novembre","Decembre"))# +
  scale_x_discrete(breaks = c(2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021), labels = c("2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020", "2021"))

# Jours pour perte totale
df_pheno$temps_perte = (df_pheno$doyper - df$doycol)
df_pheno$temps_perte = as.numeric(df_pheno$temps_perte)

p2 = df_pheno %>% 
  filter(!is.na(Couleurs)) %>%
ggplot(aes(x= year)) + geom_linerange(aes(ymin=0, ymax=temps_perte), linetype=1, color= "blue")+
  geom_point(aes(y=temps_perte), size = 3, color = "red")+ theme_bw() + coord_flip()+
  labs(y= "Durée de la période de perte des feuilles (jours)", x = "Année")+scale_y_continuous(limits =c(0,70), breaks = c(0,10,20,30,40,50,60,70))+
  scale_x_discrete()

grid.arrange(p1,p2, nrow = 1)


