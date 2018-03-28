#SCATTER_PLOT
cars[1:6,] #here cars, is a default data set that is fed into the R console.
scatter.smooth(x = cars$speed, y = cars$dist, main = "Dist ~ Speed") 
#A scatter.smooth function gives us the scatter plat between the x and y 
#variables. It helps us understand the relationshipt between the both.

#BOX_PLOT
par(mfrow = c(1,2)) #This helps us display the below graphs in the for of 
#matrices arranged in rows. while "fcol" can be used to arrange the same
# in columns.
boxplot(cars$speed, main = "speed", sub = paste("Outliers", boxplot.stats(cars$speed)$out))
boxplot(cars$dist, main = "Distance", sub =paste("Outliers", boxplot.stats(cars$dist)$out))
#The above executed functions can be used to create a boxploot to identify
#Outliers.

#DENSITYPLOT
library(e1071)
par(mfrow = c(1,2))
plot(density(cars$speed), main = "DENSITY_PLOT_SPEED", ylab = "Freequency", sub = paste("Skewness :", round(e1071::skewness(cars$speed), 2)))
polygon(density(cars$speed), col="red")
plot(density(cars$dist), main = "DENSITY_PLOT_DISTCANCE", ylab = "Freequency", sub = paste("Skewness :", round(e1071::skewness(cars$dist), 2)))
polygon(density(cars$speed), col="red")

#CORRELATION
cor(cars$speed, cars$dist) #correlation can be calculated by using the "cor" 
#function followed by 2 other variables.

LM <- lm(dist  ~ speed, data = cars) #Just like this one can perform linear regression.
summary(LM) # This displays the result.
print(LM) #This only shows us the required output, that is the B0 and B1 values in the case.

#BEST_FIT_MODEL
AIC(LM)
BIC(LM)

#PREDICTING_THE_MODEL

#STEP_1 (creating the training and test data set.)
set.seed(100) #Reproduces results of random sampeling.
contra <- sample(1:nrow(cars), 0.8*nrow(cars)) #Parameters to set the training data set.
#where .8<- refers to the size of the data set.
Train_cars <- cars[contra, ]
Test_cars <- cars[-contra ]

#STEP_2_DEVELOPMENT_AND_PREDECTION
linear_model <- lm(dist ~ speed, data = Train_cars) #developing a model.
distpred <- predict(linear_model, Test_cars) #Testing the developed model.
summary(linear_model)
AIC(linear_model)

#STEP_3_CHECK_FOR_ACCURRACY
actuals_preds <- data.frame(cbind(actuals=Test_cars$dist, predicteds=distpred))
accuracy <- cor(actuals_preds)
head(actuals_preds)

