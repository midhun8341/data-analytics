train <- read.csv("train_u6lujuX_CVtuZ9i.csv")
test <- read.csv("test_Y3wMUE5_7gLdaTN.csv")
head(train)

#Check for missing values.
colSums(is.na(train))
library(zoo)
train$Dependents <- na.aggregate(train$Dependents, mode)
str(train)
train$LoanAmount <- na.aggregate(train$LoanAmount, median)
train$Loan_Amount_Term <- na.aggregate(train$Loan_Amount_Term, median)
train$Dependents <- na.aggregate(train$Dependents, mode)
train$Credit_History <- na.aggregate(train$Credit_History, mode)
train$Gender <- na.aggregate(as.integer(train$Gender), mode)
train$Married <- na.aggregate(as.integer(train$Married), mode)
train$Self_Employed <- na.aggregate(as.integer(train$Self_Employed), mode)
train$Education <- na.aggregate(as.integer(train$Education), mode)
train$CoapplicantIncome <- na.aggregate(train$CoapplicantIncome, median)
train$Property_Area <- na.aggregate(as.integer(train$Property_Area), mode)

#Check for outliers.
boxplot.stats(train$ApplicantIncome)$out
median(train$ApplicantIncome)
boxplot.stats(train$LoanAmount)$out
median(train$LoanAmount)

#Processing the test data.

test$Dependents <- na.aggregate(test$Dependents, mode)
str(test)
test$LoanAmount <- na.aggregate(test$LoanAmount, median)
test$Loan_Amount_Term <- na.aggregate(test$Loan_Amount_Term, median)
test$Dependents <- na.aggregate(test$Dependents, mode)
test$Credit_History <- na.aggregate(test$Credit_History, mode)
test$Gender <- na.aggregate(as.integer(test$Gender), mode)
test$Married <- na.aggregate(as.integer(test$Married), mode)
test$Self_Employed <- na.aggregate(as.integer(test$Self_Employed), mode)
test$Education <- na.aggregate(as.integer(test$Education), mode)
test$CoapplicantIncome <- na.aggregate(test$CoapplicantIncome, median)
test$Property_Area <- na.aggregate(as.integer(test$Property_Area), mode)

#Training the model.
library(randomForest)
model <- randomForest(train$Loan_Status ~ . - train$Loan_ID, train, mtry = 4, importance = T, family = "binomial")
