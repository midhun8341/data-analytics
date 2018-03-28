Attrition <- read.csv("attrition.csv")
names(Attrition)
dim(Attrition)
str(Attrition)
Attrition$Gender <- as.factor(Attrition$Gender)
Attrition$Churn <- as.factor(Attrition$Churn)
Attrition$Income <- as.factor(Attrition$Income)
Attrition$Visits <- as.factor(Attrition$Visits)

#Missing values and imputation.
colSums(is.na(Attrition))
#There are no missing values.

#BOXPLOT
par(mfrow = c(1,2))
boxplot.stats(Attrition$Calls)$out
boxplot.stats(Attrition$Visits)$out
boxplot.stats(Attrition$Education)$out

x <- Attrition$Calls
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
boxplot.stats(x)$out
summary(x)

#Skewness Check

library(e1071)

par(mfrow = c(1,2))

plot(density(Attrition$Calls), main = "DENSITY_PLOT_Calls", ylab = "Freequency", sub = paste("Skewness :", round(e1071::skewness(Attrition$Calls), 2)))
plot(density(Attrition$Education), main = "DENSITY_PLOT_EDU", ylab = "Freequency", sub = paste("Skewness :", round(e1071::skewness(Attrition$Education), 2)))
   
#Splitting the data
set.seed(30)
Split <- sample(1 : nrow(Attrition), .7 * nrow(Attrition))
train_data <- Attrition[Split, ]     
test_data <- Attrition[-Split, ]

#Developing the model
names(Attrition)
library(randomForest)
model <- randomForest(Churn ~ . , train_data, mtry = 8 , importance = TRUE)
model
pred <- predict(model, test_data)
pred
table(pred, test_data$Churn)
library(Metrics)
Metrics::accuracy(pred, test_data$Churn)
