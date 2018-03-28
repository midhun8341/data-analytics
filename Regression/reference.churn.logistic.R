churn <- read.csv("attrition.csv")
str(churn)
summary(churn)
is.na(churn)
colSums(is.na(churn))
dim(churn)

#Cleanin the data.
churn$Gender <- factor(churn$Gender, labels = c("Females", "Males"))
churn$Income <- factor(churn$Income, labels = c("Low", "High"))
churn$Churn <- factor(churn$Churn, labels = c("No", "Yes"))
churn$Visits <- as.integer(churn$Visits)
sort(unique(churn$Age))
#Looking for insights.
library(ggplot2)
ggplot(data = churn, aes(x = churn$Education)) + geom_histogram(binwidth = .5, , fill = "lightgreen", col = "red")
ggplot(data = churn, aes(x = churn$Age)) + geom_histogram(binwidth = .5, , fill = "lightblue", col = "red")
ggplot(data = churn, aes(x = churn$FamilySize)) + geom_histogram(binwidth = .5, , fill = "lightgreen", col = "red")

ggplot(data = churn, aes(y = churn$Calls, x = churn$Churn, fill = churn$Churn)) + geom_boxplot()
ggplot(data = churn, aes(y = churn$Education, x = churn$Gender, fill = churn$Gender)) + geom_boxplot()
ggplot(data = churn, aes(y = churn$Education, x = churn$Churn, fill = churn$Churn)) + geom_boxplot()

ggplot(data = churn, aes(x = churn$Age, churn$Calls, col = factor(Visits))) + geom_point() + geom_smooth(method = lm, se = F)

#Checking for outliers.
boxplot.stats(churn$Education)$out
boxplot.stats(churn$Age)$out
boxplot.stats(churn$FamilySize)$out
boxplot.stats(churn$Calls)$out
boxplot.stats(churn$Visits)$out

#Check for density.
library(e1071)
ggplot(data = churn, aes(x = churn$Calls)) + geom_density(fill = "lightblue")
skewness(churn$Calls)
ggplot(data = churn, aes( x = churn$Age)) + geom_density(fill = "lightyellow")
skewness(churn$Age)

#Splitting the data.
split <- sample(1:nrow(churn), .7 * nrow(churn))
train_data <- churn[split, ]
test_data <- churn[-split, ]

#Developing the model.
model <- glm(Churn ~ . , train_data, family = "binomial")
summary(model)

pred <- predict(model, test_data, type = "response")
pred

pred.1 <- ifelse(pred <= .5,0,1)
pred.1 <- factor(pred.1, labels = c("No", "Yes"))

table(pred.1, test_data$Churn)
Metrics::accuracy(pred.1, test_data$Churn)
caret::confusionMatrix(pred.1, test_data$Churn)

#Random Forests.
library(randomForest)
model.2 <- randomForest(Churn ~ ., train_data, mtry =7,  family = "binomial")
model.2
pred.3 <- predict(model.2, test_data)
pred.3
Metrics::accuracy(pred.3, test_data$Churn)
library(dplyr)
insight <- churn %>% filter(Churn == "Yes")
insight
ggplot(data = insight, aes(x = insight$Education)) + geom_histogram(bins = 17)
