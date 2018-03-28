honor <- read.csv("Honor raw.csv")
names(honor)

#splitting the data set.
split <- sample(1: nrow(honor), .7 * nrow(honor))
train_data <- honor[split, ]
test_data <- honor[-split, ]

#developing the model
model1 <- glm(hon ~ female, train_data, family = "binomial")
summary(model)

model2 <- glm(hon ~ math + femalexmath, train_data, family = "binomial")
summary(model2)
library(pscl)
pR2(model2)

library(AICcmodavg)
AIC(model1, model2)
