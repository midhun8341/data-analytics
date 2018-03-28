mainland <- read.csv("Mainland raw.csv")
names(mainland)
scatter.smooth(mainland$Rice.Bowl.Price, mainland$Pepsi)
#Not so significant
scatter.smooth(mainland$Rice.Bowl.Price, mainland$Rice.Bowls)
#The same
scatter.smooth(mainland$Rice.Bowl.Price, mainland$Beer)
#no change
#HOW to check if multiple variables contribute to the y variable?
#splittig the Data set
split <- sample(1: nrow(mainland), .8 * nrow(mainland))
train_data <- mainland[split, ]
test_data <- mainland[-split, ]

#devoleping the model
model <- lm(as.numeric(Rice.Bowl.Price) ~ . -Pepsi -Beer , data = train_data) #since the response variable is in the form of a factor.
summary(model)

pred_model <- predict(model, test_data)
summary(pred_model)
pred_model

library(Metrics)
Metrics::rmse(pred_model,as.numeric(test_data$Rice.Bowl.Price))
#The accuracy is pretty spot on.

# A good r-square value should be about .7-.9.
#the accuracy of the prediction in case of classification can be done unsing the accuracy formula
#(TP+TN)/(all)
str(mainland$Rice.Bowl.Price)
summary(as.numeric(mainland$Rice.Bowl.Price))