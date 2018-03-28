data <- read.csv("attrition.csv")
summary(data)
str(data)

data$Gender <- factor(data$Gender, labels = c("male", "Female"), levels= c(1,0))
data$Churn <- factor(data$Churn, labels = c("churn", "No_churn"), levels= c(1,0))

colSums(is.na(data)) #No missing values

boxplot.stats(data$Age)$out
boxplot.stats(data$Income)$out
boxplot.stats(data$Calls)$out
boxplot.stats(data$Visits)$out

library(e1071)
par(mfrow = c(1,2))
plot(density(data$Age), main = "Age", ylab = "Freequency", sub = paste("Skewness :", round(skewness(data$Age),2)))
plot(density(data$Education), main = "Age", ylab = "Freequency", sub = paste("Skewness :", round(skewness(data$Education),2)))

set.seed = 34
split <- sample(1 : nrow(data), .7 * nrow(data))
train.3 <- data[split, ]
test <- data[-split, ]

#Developing the model.
names(data)
library(randomForest)
model <- randomForest(Churn ~  + Gender + FamilySize + Education + Calls + Visits , train.3, importance = TRUE, mtry = 4)
model
pred <- predict(model, test)
pred

table(pred, test$Churn)
Metrics::accuracy(pred, test$Churn)

importance(model)


#Shows redundant variables
library(caret)
library(mlbench)
cormat <- cor(data[,3:8])
summary(cormat)
high_cor <- findCorrelation(cormat, cutoff = 0.5)
print(high_cor)


#Ranks based on importance
set.seed = 5
control <- trainControl(method = "repeatedcv", repeats = 3, number = 10)
model.1 <- train(Churn ~., data, method = "lvq", preProcess = "scale", trcontrol = control)
imp <- varImp(model.1, scale = FALSE)
print(imp)
plot(imp)

#Tells us which variables are important.
install.packages("Boruta")
library(Boruta)
boruta <- Boruta(Churn~., data, doTrace=2)
print(boruta)
