##TEMPERATURE, OXYGEN PROFILE AND LIGHT ATTENUATION
##BETWEEN OCTOBER 2011 AND 2012

#Nettoyer l'espace de travail
rm(list = ls())

#Choisir le répertoire de travail
setwd("/Users/Morgan/Documents/SMasse")

#Ouvrir les librairies
library("ggplot2")
library("lubridate")
library("MBA")
library("reshape2")
library("colorRamps")
library("gridExtra")

#Ouvrir les données
profil <- read.table("Data_brut_Steph_Cyn/Profil_ODV.txt", header = T )
profil$Date <- as.Date(profil$Date)
profil[profil$Date == "2011-10-07",'Date'] <- "2011-10-17" #il y a une erreur de date
profil$Date <- decimal_date(profil$Date)
masse <- read.csv("Data_Morgan/nutriments_tauxAO_SMasse.csv")
masse$date <- decimal_date(as.Date(masse$date))
colnames(masse)[c(2,3)] <- c('Date','Depth')
masse <- merge(masse,profil[,c('Date','Depth','Incident_Light')], all.x = T)

#Enlever la ligne "Glace" et mettre en format numérique
profil<- profil[!profil$pH == 'Glace',]
profil$Temperature <- as.numeric(as.character(profil$Temperature))
profil$DO_mg <- as.numeric(as.character(profil$DO_mg))

#Créer un vecteur pour l'axe des x, pour déterminer les ticks marks
x <- data.frame(date=as.Date(c('2011-10-01','2011-11-01','2011-12-01','2012-01-01','2012-02-01','2012-03-01','2012-04-01',
       '2012-05-01','2012-06-01','2012-07-01','2012-08-01','2012-09-01','2012-10-01','2012-11-01')))
x$x <- decimal_date(x$date)

#Interpolations de la température, oxygène et lumière
mba.temp <- mba.surf(profil[,c('Date','Depth','Temperature')], 100,100)
dimnames(mba.temp$xyz.est$z) <- list(mba.temp$xyz.est$x, mba.temp$xyz.est$y)
df1 <- melt(mba.temp$xyz.est$z, varnames = c('Date','Depth'), value.name = 'Temperature') 

mba.o2 <- mba.surf(profil[,c('Date','Depth','DO_mg')], 100,100)
dimnames(mba.o2$xyz.est$z) <- list(mba.o2$xyz.est$x, mba.o2$xyz.est$y)
df2 <- melt(mba.o2$xyz.est$z, varnames = c('Date','Depth'), value.name = 'DO_mg') 

mba.light <- mba.surf(profil[,c('Date','Depth','Incident_Light')], 100,100)
dimnames(mba.light$xyz.est$z) <- list(mba.light$xyz.est$x, mba.light$xyz.est$y)
df3 <- melt(mba.light$xyz.est$z, varnames = c('Date','Depth'), value.name = 'Incident_Light')

#Graphiques
p1 = ggplot(df1, aes(x = Date, y = Depth)) +
  geom_raster(aes(fill = Temperature), interpolate = F, hjust = 0.5, vjust = 0.5) +
  geom_contour(aes(z = Temperature)) +
  geom_point(data = profil, aes(Date, Depth), colour = 'white') +
  geom_point(data = masse, aes(Date,Depth),colour = 'black') +
  scale_y_reverse() +
  scale_fill_gradientn(colours = matlab.like2(10)) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous("", breaks = c(2011.74794520548, 2011.83287671233, 2011.91506849315, 2012,
                                    2012.08469945355, 2012.16393442623, 2012.24863387978,
                                    2012.3306010929, 2012.41530054645, 2012.49726775956, 2012.58196721311,
                                    2012.66666666667, 2012.74863387978, 2012.83333333333),
                     labels = c("Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov"),
                     limits = c(2011.74794520548,2012.83333333333)) +
  labs(y = 'depth (m)', fill = 'temperature (°C)') +
  annotate(geom="text", x=2011.74794520548, y=0.5, label="A",
           color="black", size = 6)
  
p2 = ggplot(df2, aes(x = Date, y = Depth)) +
  geom_raster(aes(fill = DO_mg), interpolate = F, hjust = 0.5, vjust = 0.5) +
  geom_contour(aes(z = DO_mg)) +
  geom_point(data = profil, aes(Date, Depth), colour = 'white') +
  geom_point(data = masse, aes(Date,Depth),colour = 'black') +
  scale_y_reverse() +
  scale_fill_gradientn(colours = matlab.like2(10)) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous("", breaks = c(2011.74794520548, 2011.83287671233, 2011.91506849315, 2012,
                                    2012.08469945355, 2012.16393442623, 2012.24863387978,
                                    2012.3306010929, 2012.41530054645, 2012.49726775956, 2012.58196721311,
                                    2012.66666666667, 2012.74863387978, 2012.83333333333),
                     labels = c("Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov"),
                     limits = c(2011.74794520548,2012.83333333333)) +
  labs(y = 'depth (m)',fill = 'oxygen (mg/L)    ')  +
  annotate(geom="text", x=2011.74794520548, y=0.5, label="B",
           color="black", size = 6)

pdf("Figures_MB/profiles_temp_do.pdf", width = 9, height = 5)
grid.arrange(p1,p2, nrow = 2)
dev.off()

#Graphique lumière, non exporté
ggplot(df3, aes(x = Date, y = Depth)) +
  geom_raster(aes(fill = Incident_Light), interpolate = F, hjust = 0.5, vjust = 0.5) +
  geom_contour(aes(z = Incident_Light)) +
  geom_point(data = profil, aes(Date, Depth), colour = 'white') +
  geom_point(data = masse, aes(Date,Depth),colour = 'black') +
  scale_y_reverse() +
  scale_fill_gradientn(colours = matlab.like2(10)) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous("", breaks = c(2011.74794520548, 2011.83287671233, 2011.91506849315, 2012,
                                    2012.08469945355, 2012.16393442623, 2012.24863387978,
                                    2012.3306010929, 2012.41530054645, 2012.49726775956, 2012.58196721311,
                                    2012.66666666667, 2012.74863387978, 2012.83333333333),
                     labels = c("Oct","Nov","Dec","Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov"),
                     limits = c(2011.74794520548,2012.83333333333)) +
  labs(y = 'Depth (m)')  
