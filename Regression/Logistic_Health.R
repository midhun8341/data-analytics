health <- read.csv("health.csv")
names(health)
str(health)

#Splitting the data set
set.seed(50)
split <- sample(1 : nrow(health), .7 * nrow(health))
train_data <- health[split, ]
test_data <- health[-split, ]

#developing the model
model <- glm(Suffered.Heart.storke ~ . , train_data, family = "binomial")
summary(model)

pred <- predict(model, test_data, type = 'response')
pred
summary(pred)
pred.1 <- ifelse(pred <= .5, 0, 1)
table(pred.1, test_data$Suffered.Heart.storke)
length(pred.1)
(612+126)/(612+19+31+126)
#TPR = .93
