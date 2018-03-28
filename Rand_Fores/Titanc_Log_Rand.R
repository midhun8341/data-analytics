Titanic_train <- read.csv("train.csv")

#Changing labels into coded variables.
Titanic_train$Embarked <- factor(Titanic_train$Embarked, levels = c("C","S","Q"), labels = c(1,2,3))
Titanic_train$Embarked
Titanic_train$Sex <- factor(Titanic_train$Sex, levels = c("male","female"), labels = c(1,2))
Titanic_train$Sex
Titanic_train$Fare <- floor(Titanic_train$Fare)
Titanic_train$Age <- floor(Titanic_train$Age)
Titanic_train$Fare
Titanic_train$SibSp

#missing values and NA imputation.
library(zoo)
Titanic_train$Age <- na.aggregate(Titanic_train$Age,  FUN = median)
Titanic_train$Age
Titanic_train$Embarked <- na.aggregate(as.integer(Titanic_train$Embarked),  FUN = median)
Titanic_train$Fare <- na.aggregate(Titanic_train$Fare,  FUN = median)
Titanic_train$Survived
names(Titanic_train)

#Check for outliers and imputation.
#BOXPLOT
par(mfrow = c(1,2))
boxplot(Titanic_train$Age, main = "AGE", sub = "outliers")
boxplot(Titanic_train$Fare, main = "FARE", sub ="outliers" )
#We deffinitele have some outliers to deal with, which we will be working on in the following code.

#Treating the outliers
x <- Titanic_train$Age
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
boxplot.stats(x)$out
summary(x)

y <- Titanic_train$Fare
qnt.y <- quantile(y, probs=c(.25, .75), na.rm = T)
caps.y <- quantile(y, probs=c(.05, .85), na.rm = T)
H.y <- 1.5 * IQR(y, na.rm = T)
y[y < (qnt.y[1] - H.y)] <- caps.y[1]
y[y > (qnt.y[2] + H.y)] <- caps.y[2]
boxplot.stats(y)$out
summary(y)

New_train <- data.frame(x,y,Titanic_train)

#Check for Skewness and kurtosis
par(mfrow = c(1,2))
library(e1071)
plot(density(New_train$x), main = "DENSITY_PLOT_AGE", ylab = "Freequency", sub = paste("Skewness :", round(e1071::skewness(New_train$x), 2)))
plot(density(New_train$y), main = "DENSITY_PLOT_FARE", ylab = "Freequency", sub = paste("Skewness :", round(e1071::skewness(New_train$y), 2)))
#It is evidental from the above plot that Fare is positively skewed. We'll have to transform it.
Titanic_train$Fare <- log(Titanic_train$Fare + 1)
Titanic_train$Fare <- na.aggregate(Titanic_train$Fare,  FUN = median)
Titanic_train$Fare

#Creating the tese data set.
titanic_test <- read.csv("test.csv")
titanic_test$Embarked <- factor(titanic_test$Embarked, levels = c("C","S","Q"), labels = c(1,2,3))
titanic_test$Embarked
titanic_test$Sex <- factor(titanic_test$Sex, levels = c("male","female"), labels = c(1,2))
titanic_test$Sex
titanic_test$Fare <- floor(titanic_test$Fare)
titanic_test$Age <- floor(titanic_test$Age)
titanic_test$Fare

#missing values and NA imputation.
library(zoo)
titanic_test$Age <- na.aggregate(titanic_test$Age,  FUN = median)
titanic_test$Age
titanic_test$Embarked <- na.aggregate(as.integer(titanic_test$Embarked),  FUN = median)
titanic_test$Fare <- na.aggregate(titanic_test$Fare,  FUN = median)
names(titanic_test)

#Check for outliers and imputation.
#BOXPLOT
par(mfrow = c(1,2))
boxplot(titanic_test$Age, main = "AGE", sub = "outliers")
boxplot(titanic_test$Fare, main = "FARE", sub ="outliers" )
#Just like the train data, the test data also seems to be affected with outliers.

#Treating the outliers
x <- titanic_test$Age
qnt.x <- quantile(x, probs=c(.25, .75), na.rm = T)
caps.x <- quantile(x, probs=c(.05, .95), na.rm = T)
H.x <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt.x[1] - H.x)] <- caps.x[1]
x[x > (qnt.x[2] + H.x)] <- caps.x[2]
boxplot.stats(x)$out
summary(x)

y <- titanic_test$Fare
qnt.y <- quantile(y, probs=c(.25, .75), na.rm = T)
caps.y <- quantile(y, probs=c(.05, .85), na.rm = T)
H.y <- 1.5 * IQR(y, na.rm = T)
y[y < (qnt.y[1] - H.y)] <- caps.y[1]
y[y > (qnt.y[2] + H.y)] <- caps.y[2]
boxplot.stats(y)$out
summary(y)

new_test <- data.frame(x, y, titanic_test)

#Check for Skewness and kurtosis
par(mfrow = c(1,2))
library(e1071)
plot(density(new_test$x), main = "DENSITY_PLOT_AGE", ylab = "Freequency", sub = paste("Skewness :", round(e1071::skewness(new_test$x), 2)))
plot(density(new_test$y), main = "DENSITY_PLOT_FARE", ylab = "Freequency", sub = paste("Skewness :", round(e1071::skewness(new_test$y), 2)))
#Its similar to the train data set. Hence, we'll have to transform it.

titanic_test$Fare <- log(titanic_test$Fare + 1)
titanic_test$Fare <- na.aggregate(titanic_test$Fare,  FUN = median)
titanic_test$Fare

#Splitting the train data
set.seed(15)
Split <- sample(1 : nrow(New_train), .7 * nrow(New_train))
new_train.1 <- New_train[Split, ]
new_test.1 <- New_train[-Split, ]

#Random_Forest
#DEveloping the model
names(New_train)
library(randomForest)
model <- randomForest(Survived ~ +x +y +Pclass +Sex +Age +SibSp +Fare, mtry = 3, new_train.1, importance = TRUE)
model
pred <- predict(model, new_test.1)
pred
pred.1 <- ifelse(pred <= .5, 0, 1)
table(pred.1, new_test.1$Survived)
(155+71)/(155+30+12+71)
#.84
Metrics::accuracy(pred.1, new_test.1$Survived)
pred <- predict(model, new_test)
summary(pred)
pred.1 <- ifelse(pred <= .5, "Dead", "Survived")
table(pred.1)

#----------------------------------------------------------------------------------------------#

#Logistic_Regression

#Developing the model.
model.lr <- glm(Survived ~ +x +y +Pclass +Sex +Age +SibSp +Fare, new_train.1, family = "binomial")
model.lr
anova(model.lr, test = "Chisq")

pred.lr <- predict(model, new_test.1, type = "response" )
pred.lr

pred.1 <- ifelse(pred.lr <= .5, 0, 1)
table(pred.1, new_test.1$Survived)
(151+70)/(151+33+14+70)
#.82
#Preddecting the Test data set
pred.t <- predict(model, new_test, type = "response")
pred.t
pred.1 <- ifelse(pred.t <= .5, "Dead", "Survived")
pred.1
table(pred.1)

#----------------------------------------------------------------------------------------------#
