library(randomForest)
library(MASS) #Retrieves the Boston dataset.
library(tree) # Used to execute the desicision trees.
attach(Boston)
split <- sample(1 : nrow(Boston), .7 * nrow((Boston)))
Boston.train <- Boston[split, ]
Boston.test <- Boston[-split, ]

model <- randomForest(medv ~ ., Boston.train, mtry = 13, importance = TRUE)
model
pred.model <- predict(model, Boston.test)
pred.model
plot(pred.model, Boston.test$medv)
library(Metrics)
Metrics::rmse(pred.model, Boston.test$medv)
sqrt(11.76)
plot(model)

model.1 <- randomForest(medv ~ ., Boston.train, mtry = 13,  importance = TRUE)
model.1
pred.model.1 <- predict(model.1, Boston.test)
Metrics::rmse(pred.model.1, Boston.test$medv)
sqrt(10.409)

#Now we shall attempt with a certain number of trees.

model.2 <- randomForest(medv ~ ., Boston.train, mtry = 13, ntree = 35,  importance = TRUE)
model.2
pred.2 <- predict(model.2, Boston.test)
Metrics::rmse(pred.2, Boston.test$medv)
sqrt(12.92)

cor(pred.2, Boston.test$medv)
#0.9298763054
