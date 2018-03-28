LINREG <- read.csv("SimpleLinear.csv")
attach(LINREG)
names(LINREG)
dim(LINREG)
str(LINREG)
summary(LINREG)
pairs(~Y+X) 
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
fit <- lm(Y ~ . , data = LINREG)
summary (fit)
ols_vif_tol(fit)
