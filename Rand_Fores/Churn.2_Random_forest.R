#1.Calling the data set.
churn.data <- read.csv("CHURN.txt", header = TRUE)
churn.1 <- churn.data[,-4]
churn <- churn.1[,-1]
str(churn)
#2. Check missing values or N/A.
colSums(is.na(churn)) #Check for missing values.
#There are no missing values. So we can proceede further with the existing data.

#3.Changing the required variables into coded variables.
names(churn)
churn$Int.l.Plan <- factor(churn$Int.l.Plan, levels = c("yes", "no"), labels = c(0,1))
churn$VMail.Plan<- factor(churn$VMail.Plan, levels = c("yes", "no"), labels = c(0,1))
churn$Churn. <- factor(churn$Churn., levels = c("False.", "True."), labels = c(0,1))

#4Check for Outliers.
#BOXPLOT
boxplot.stats(churn$Day.Mins)$out
#332.9 337.4 326.5 350.8 335.5  30.9  34.0 334.3 346.8  12.5  25.9   0.0   0.0  19.5 329.8 7.9 328.1  27.0  17.6 326.3 345.3   2.6   7.8  18.9  29.9
boxplot.stats(churn$Day.Calls)$out
#158 163  36  40 158 165  30  42   0  45   0  45 160 156  35  42 158 157  45  44  44  44  40
boxplot.stats(churn$Day.Charge)$out
# 56.59 57.36 55.51 59.64 57.04  5.25  5.78 56.83 58.96  2.13  4.40  0.00  0.00  3.32 56.07 1.34 55.78  4.59  2.99 55.47 58.70  0.44  1.33  3.21  5.08
boxplot.stats(churn$Eve.Mins)$out
# 348.5 351.6  31.2 350.5  42.2 347.3  58.9  43.9  52.9  42.5  60.8  58.6  56.0  48.1  60.0  350.9  49.2 339.9 361.8 354.2 363.7   0.0 341.3
boxplot.stats(churn$Eve.Calls)$out
#164  46 168  42  37  12 157 155  45  36 156  46  44 155  46  43   0 155 159 170
boxplot.stats(churn$Eve.Charge)$out
#29.62 29.89  2.65 29.79  3.59 29.52  5.01  3.73  4.50  3.61  5.17  4.98  4.76  4.09  5.10 29.83  4.18 28.89 30.75 30.11 30.91  0.00 29.0
boxplot.stats(churn$Night.Mins)$out
#57.5 354.9 349.2 345.8  45.0 342.8 364.3  63.3  54.5  50.1  43.7 349.7 352.5  23.2  63.6 381.9 377.5 367.7  56.6  54.0  64.2 344.3 395.0 350.2  50.1  53.3 352.2 364.9  61.4  47.4
boxplot.stats(churn$Intl.Mins)$out
#20.0  0.0 17.6  2.7 18.9  0.0 18.0  2.0  0.0 18.2  0.0  0.0  1.3  0.0  0.0  0.0  2.2 18.0  0.0 17.9  0.0 18.4  2.0 17.8  2.9  3.1 17.6  2.6  0.0  0.0 18.2  0.0 18.0  1.1  0.0 18.3 0.0  0.0  2.1  2.9  2.1  2.4  2.5  0.0  0.0 17.8
boxplot.stats(churn$Intl.Calls)$out
#19 15 11 12 13 11 12 11 13 12 11 11 18 11 12 13 12 12 11 15 13 15 11 11 14 13 11 13 13 12 11

#Treating th outliers.

x <- churn$Night.Charge
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
boxplot.stats(x)$out
summary(x)

y <- churn$Day.Charge
qnt.y <- quantile(y, probs=c(.25, .75), na.rm = T)
caps.y <- quantile(y, probs=c(.05, .85), na.rm = T)
H.y <- 1.5 * IQR(y, na.rm = T)
y[y < (qnt.y[1] - H.y)] <- caps.y[1]
boxplot.stats(y)$out
summary(y)
str(churn)
#5.Check for skewness
plot(density(churn$Account.Length), main = "acc_length", ylab = "Freequency", sub = paste("Skewness :", round(e1071::skewness(churn$Account.Length), 2)))
library(e1071)
par(mfrow = c(2,3))
plot(density(churn$Day.Calls), main = "day_calls", ylab = "Freequency", sub = paste("Skewness :", round(e1071::skewness(churn$Day.Calls), 2)))
plot(density(churn$Day.Mins), main = "day_mins", ylab = "Freequency", sub = paste("Skewness :", round(e1071::skewness(churn$Day.Mins), 2)))
plot(density(churn$Day.Charge), main = "day_chrge", ylab = "Freequency", sub = paste("Skewness :", round(e1071::skewness(churn$Day.Charge), 2)))

plot(density(churn$Eve.Mins), main = "eve_mins", ylab = "Freequency", sub = paste("Skewness :", round(e1071::skewness(churn$Eve.Mins), 2)))
plot(density(churn$Eve.Charge), main = "eve_charge", ylab = "Freequency", sub = paste("Skewness :", round(e1071::skewness(churn$Eve.Charge), 2)))
plot(density(churn$Eve.Calls), main = "eve_calls", ylab = "Freequency", sub = paste("Skewness :", round(e1071::skewness(churn$Eve.Calls), 2)))

par(mfrow = c(2,3))
plot(density(churn$Night.Calls), main = "Night_calls", ylab = "Freequency", sub = paste("Skewness :", round(e1071::skewness(churn$Night.Calls), 2)))
plot(density(churn$Night.Charge), main = "Night_mins", ylab = "Freequency", sub = paste("Skewness :", round(e1071::skewness(churn$Night.Charge), 2)))
plot(density(churn$Night.Mins), main = "Night_chrge", ylab = "Freequency", sub = paste("Skewness :", round(e1071::skewness(churn$Night.Mins), 2)))

plot(density(churn$Intl.Calls), main = "Intl_calls", ylab = "Freequency", sub = paste("Skewness :", round(e1071::skewness(churn$Intl.Calls), 2)))
plot(density(churn$Intl.Mins), main = "Intl_mins", ylab = "Freequency", sub = paste("Skewness :", round(e1071::skewness(churn$Intl.Mins), 2)))
plot(density(churn$Intl.Charge), main = "Intl_chrge", ylab = "Freequency", sub = paste("Skewness :", round(e1071::skewness(churn$Intl.Charge), 2)))

#It can be concluded that Intl_cals is positively skewed, so it can be overcome by log transformation. But since its negligeble, it can be ignored at this point.

#6.Splitting the data set.
set.seed(50)
Split <- sample(1:nrow(churn), .7 * nrow(churn))
Train_data <- churn[Split, ]
Test_data <- churn[-Split, ]

#Developing the model
names(churn)
library(randomForest)
model <- randomForest(as.factor(Churn.) ~ . , Train_data, mtry = 17, importance = TRUE, type = "binomial")
model
summary(model)
pred <- predict(model, Test_data, type = "response")
pred
library(Metrics)
accuracy(pred, Test_data$Churn.)
#.95

model.1 <- randomForest(as.factor(Churn.) ~ CustServ.Calls + Intl.Calls + VMail.Plan + Int.l.Plan, Train_data, mtry = 4, importance = TRUE, family = "binomial")
model.1
summary(model.1)
pred.1 <- predict(model.1, Test_data, type = "response")
pred.1
library(Metrics)
accuracy(pred.1, Test_data$Churn.)
#.89

