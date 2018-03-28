churn <- read.csv("CHURN.txt")
  colSums(is.na(churn))
str(churn)
dim(churn)
summary(churn)

library(Boruta)
featur <- Boruta(churn$Churn. ~., churn, doTrace = 2)
featur
plot(featur, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(featur$ImpHistory),function(i)
  featur$ImpHistory[is.finite(featur$ImpHistory[,i]),i])
names(lz) <- colnames(featur$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(featur$ImpHistory), cex.axis = 0.7)

churn <- churn[,,]
churn <- churn[,-(1:3)]
churn <- churn[,-(5)]
churn <- churn[,-7]
churn <- churn[,-9]

str(churn)

#Splitting the data
set.seed(50)
Split <- sample(1:nrow(churn), .7 * nrow(churn))
Train_data <- churn[Split, ]
Test_data <- churn[-Split, ]
Train_data <- Train_data[, -1]
Test_data <- Test_data[, -1]
#USing Decision Trees
library(randomForest)
model.tree <- randomForest(Churn. ~., Train_data, mtry = 9, importance = TRUE)
model.tree
pred <- predict(model.tree,Test_data)
pred
library(Metrics)
Metrics::accuracy(Test_data$Churn., pred)

library(tree)
model.froest <- tree(Churn. ~., Train_data)
plot(model.froest)
summary(model.froest)
pred.1 <- predict(model.froest, Test_data, type = "class")

table(pred.1, Test_data$Churn.)
length(pred.1)
text(model.froest, pretty = 0)

cv.model <- cv.tree(model.froest)
plot(cv.model$size, cv.model$dev, type = "b")
plot(cv.model$k, cv.model$dev, type = "b")
cv.model
 #No need of pruning.

model.3 <- glm(Churn. ~., Train_data, family = "binomial")
pred.3 <- predict(model.3, Test_data, type = "response")
pred.3 <- ifelse(pred.3 <= .5, "False.", "True.")
accuracy(Test_data$Churn., pred.3)

chisq.test(churn)
ceiling(runif(19, 4, 97))
getwd()
VIF::vif(churn$Day.Mins)
VIF::vif()
library(ROCR)
roc(Test_data$Churn., pred)
