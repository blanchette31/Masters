#Metrics calculation for analyzing O2 and CO2 depature timeseries
#by Dominic Vachon, domvachon@gmail.com
#2019-10-08
#O2-CO2 metrics framework is presented in Vachon et al. (in revision), Limnology and Oceanography Letters

#
#Load library to plot ellipse and to calcuate type II regressions
library(ellipse)
library(lmodel2)

# Load O2 and CO2 data
#here an example from Övre Björntjärn (here named Lake1) in Sweden
lake1 <- read.table('lake1.txt',header=TRUE,sep="\t")
data <- data.frame(CO2dep=lake1$CO2dep,O2dep=lake1$O2dep)

#removing NAs in data
row.keep1 <- is.na(data$CO2dep)
x <- data$CO2dep[!row.keep1]
y <- data$O2dep[!row.keep1]
row.keep2 <- is.na(y)
x <- x[!row.keep2]
y <- y[!row.keep2]

#calculating means
mu <- c(mean(x,na.rm=TRUE),mean(y,na.rm=TRUE))

#ellipe axis length calculation
corMat = cor(cbind(x,y))
covMat <- var(cbind(x,y))
evals <- eigen(covMat)$values
ell.len <- 2*sqrt(5.991*evals)

#Type II regression for slope calculation
reg <- lmodel2(y~x,nperm=99)
inter <- reg$regression.results[2,2]
slope <- reg$regression.results[2,3]
corr <- reg$r
reg

#gathering all metrics in a data frame
metrics <- data.frame(meanCO2dep=mu[1],meanO2dep=mu[2],offset=mu[1]+mu[2],EQ=1/abs(slope),width=ell.len[2],stretch=ell.len[1])
  
#
#plot cloud of O2 and CO2 departures
#

#defining points of 95% confidence ellipe
ell.point <- ellipse(cor(x, y),scale=c(sd(x),sd(y)),centre=mu,level=0.95,npoints = 100)

#plot cloud
plot(y~x,type='p',xlab="CO2 dep", ylab="O2 dep", main="O2 and CO2 departure cloud")
lines(ell.point,col='blue')
points(x=mu[1],y=mu[2],col='red',pch=19,labels="centroid")
text(x=mu[1],y=mu[2], labels = "centroid", pos = 4,col="red")
abline(a = 0, b = -1,lty=5)
legend("topright",c("95% ellipse","1:-1 line"),col=c("blue","black"),lty=c(1,5))

#final metrics
metrics





