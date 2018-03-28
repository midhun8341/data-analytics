#Q3
Fitness <- read.csv("Fitness.csv")
#A.CORRELATION
cor(Fitness$Performance, Fitness$RunTime) #The correlationship of -.82 shows 
#that both the predictors are inversely proportional.

#B.ANALYSING_ROBUSTNESS
#STEP_1_SCATTER_PLOT
plot(mfrow = c(1,2))
scatter.smooth(x = Fitness$Oxygen_Consumption, y = Fitness$Performance, main = "SCATTER_OXY-PERF")
scatter.smooth(x = Fitness$Oxygen_Consumption, y = Fitness$RunTime, main = "SCATTER_OXY-RUNT")

#STEP_2_BOXPLOT
boxplot(Fitness$Performance, main = "BOXPLOT_PERFORMANCE", sub = paste("performance_outliers", boxplot.stats(Fitness$Performance)$out) )
boxplot(Fitness$RunTime, main = "BOXPLOT_RUNTIME", sub = paste("performance_RUNTIME", boxplot.stats(Fitness$RunTime)$out) )

#3.LINEAR_EQUATION
output <- lm(Oxygen_Consumption ~ Performance + RunTime, data = Fitness)
summary(output)
print(output)
#Linear regression equation -> Y = 52.78 + (0.063 * X1) + (-2.62 * X2) where, Y = Oxygen_consumption, X1 = Performance and X2 = Runtime.

#4.ASSUMPTION_VALIDATION
#A.We have already checked for the leaniarity B/W the predictor and the response variable using a scatterplot.
#B.Normality
library(e1071)
par(mfrow = c(1,2))
plot(density(Fitness$Performance), main = "performance density")
plot(density(Fitness$RunTime), main = "Runtime denisty")
#It seems to be clearly evidental that the above graphs show a close to normal trend.
#C.Multi colinearity
library(VIF)
library(olsrr) #Used to get the VIF.
ols_vif_tol(output)
abline(Fitness)
#D.Homoscadsticity
