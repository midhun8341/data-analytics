library(randomForest)
library(ISLR)
attach(Carseats)
Carseats <- Carseats
High=ifelse (Sales <=8," No"," Yes ")
Carseats =data.frame(Carseats ,High)

#splitting the data
set.seed(5)
split <- sample(1:nrow(Carseats), .8 * nrow(Carseats))
train_data <- Carseats[split, ]
test_data <- Carseats[-split, ]

#developing the data
model <- randomForest(High ~ . - Sales, train_data, mtry = 10, importance = TRUE)
summary(model) #If not able to execute the above code, the try executing it "as.factor"
plot(model)
model
pred <- predict(model, test_data, type = "class")
table (pred, test_data$High)

(45+19)/(45+12+4+19)
#Here, we have a higher significance (Tue Positive Rate) as when compared to 
#traditional decision tree.

str(High)
