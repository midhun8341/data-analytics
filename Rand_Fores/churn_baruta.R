churn <- read.csv("CHURN.txt")
churn <- churn[,-1]
str(churn)

feature <- Boruta(churn$Churn. ~ .,churn, doTrace =2)
feature

colSums(is.na(churn))
churn$Int.l.Plan <- factor(churn$Int.l.Plan, levels = c("yes", "no"), labels = c(0,1))
churn$VMail.Plan<- factor(churn$VMail.Plan, levels = c("yes", "no"), labels = c(0,1))
churn$Churn. <- factor(churn$Churn., levels = c("False.", "True."), labels = c(0,1))

set.seed(50)
Split <- sample(1:nrow(churn), .7 * nrow(churn))
Train_data <- churn[Split, ]
Test_data <- churn[-Split, ]

names(churn)
model <- randomForest(Churn. ~ .- Account.Length - Area.Code - Day.Calls - Eve.Calls - Night.Calls - State, Train_data, mtry = 10, importance = TRUE)
model
pred <- predict(model, Test_data)
Metrics::accuracy(pred, Test_data$Churn.)
#95.7 % accuracy
