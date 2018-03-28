library(ISLR)
attach(Caravan)
set.seed
train.1 <- sample(1 :nrow(Caravan), .7*nrow(Caravan))
train.caravan <- Caravan[train.1, ]
test.caravan <- Caravan[-train.1, ]

model <- glm(Purchase ~ . , data = train.caravan, family = binomial)
summary(model)

predict.model <- predict(model, test.caravan, type = "response" )
summary(predict.model)
table(test.caravan$Purchase, predict.mod)
predict.mod <- ifelse(predict.model <=.5, "no", "yes")
