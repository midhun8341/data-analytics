attach(BODYFAT)
BODYFAT <- read.csv("BODYFAT.csv")
names(BODYFAT)
dim(BODYFAT)
str(BODYFAT)
summary(BODYFAT)
par(mfrow=c(10,3))
pairs(~PctBodyFat1+PctBodyFat2+Density+Age+Weight+Height+Adioposity+Hip+Thigh+Knee+Ankle+Biceps+Forearm+Wrist)

bodyfat_new<-BODYFAT[,3:19]
dim(bodyfat_new)
str(bodyfat_new)
library(e1071) #for skewness
library(PerformanceAnalytics) #for coreelogram(chart.correlation)
library(Hmisc) #for missing value treatment
library(corrplot) #for corellogram
library(party) #selecting best variables
library(Boruta) #deciding if a variable is imp 
library(caret) # for boxcox transformation
library(car)
library(DMwR)
library(DAAG)
library(olsrr)
library(relaimpo)
set.seed(150)
sample <- sample.int(n= nrow(bodyfat_new),size=floor(.70*nrow(bodyfat_new)),replace=F)
bodyfat_train<-bodyfat_new[sample,] #70% train data
bodyfat_test<-bodyfat_new[-sample,] #30%test data
dim(bodyfat_train)
dim(bodyfat_test)
for(k in names(bodyfat_train))
{
  #file name
   png(file=paste(k,"_dens_scatter_box",".png",sep=""),width=900,height = 550)
  
  #selected column
  x <- as.numeric (bodyfat_train[, k])
  
  #skewness and kurtosis values for pasting  in graph
  skewness <- round(skewness(x),2)
  kurtosis <- round(kurtosis(x),2)
  
  dens <-density(x, na.rm = T) #density func
   
  par(mfrow=c(1,3)) #setup plot area in 3 columns
  
  
  #density plot
  plot(dens,type="l",col="red",ylab = "frequnecy",xlab=k,main = paste(k,":density plot"),sub = paste("Skewness:",skewness,"kurtosis:",kurtosis))
  
  #overlapping normal curve
  curve(dnorm(x,mean = mean(x,na.rm = TRUE),sd=sd(x,na.rm = TRUE)),col="darkblue",lwd=2, add = TRUE,yaxt="n")
  lines(dens)
  
  polygon(dens, col="red")
  
  
  #scatterplot
  plot(x,bodyfat_train$PctBodyFat2,col="blue",ylab="PctBodyFat2", xlab = k,main = paste(k,":scatterplot"),pch=20)
  
  #boxplot
  boxplot(x,main=paste(k,":BoxPlot"),sub=paste("outliers:",paste(boxplot.stats(x)$out,collapse = " ")))
  dev.off()
  }

attach(bodyfat_train)
fit <- lm(PctBodyFat2~ .-Weight-Abdomen-FatFreeWt-Adioposity-Thigh-Hip-Chest,data = bodyfat_train)
summary(fit)

ols_vif_tol(fit)

fit <- lm(PctBodyFat2~ . - Density,data = bodyfat_train)
summary(fit)

ols_vif_tol(fit)
names(f)