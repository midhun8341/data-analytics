library(randomForest)
health <- read.csv("health.csv")
names(health)
str(health)
#Splitting the data set
set.seed(50)
split <- sample(1 : nrow(health), .7 * nrow(health))
train_data <- health[split, ]
test_data <- health[-split, ]

#developing the model

model <- randomForest(Suffered.Heart.storke ~ . , train_data, mtry = 2, importance = TRUE, type = "binomial")
model

pred <- predict(model, test_data)
test_data
str(pred)
pred.1 <- ifelse(pred <= .5, 0, 1)
table(pred.1, test_data$Suffered.Heart.storke)
(649+288)/(649+8+15+288)
#PPR = .97
library(caret)
confusionMatrix(pred.1, test_data$Suffered.Heart.storke)
