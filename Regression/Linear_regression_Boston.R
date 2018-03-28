library(MASS) #Retrieves the Boston dataset.
attach(Boston)
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

Boston_new <- as.data.frame(lapply(Boston, normalize))
split <- sample(1 : nrow(Boston_new), .7 * nrow((Boston_new)))
Boston.train <- Boston_new[split, ]
Boston.test <- Boston_new[-split, ]

names(Boston)
model <- lm(medv ~ . - indus -age, Boston.train)
summary(model)
pred <- predict(model, Boston.test)
pred

cor(pred, Boston.test$medv)
#0.8603800539
library(Metrics)
Metrics::rmse(pred, Boston.test$medv)
#4.49
rmse(pred, Boston.test$medv)
#0.1143037246
