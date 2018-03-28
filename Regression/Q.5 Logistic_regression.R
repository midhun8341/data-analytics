#Q5) Logistic Regression
Logistic_reg <- read.csv("SAFETY.csv")
New_data <- data.frame(Logistic_reg$Unsafe, Logistic_reg$Weight)
str(New_data)
dim(New_data)
New_data
#STEP_1 Creating test and train data set.
set.seed(50)
split <- sample(1:nrow(New_data), .7 * nrow(New_data))
Train_data_L <- New_data[split, ]
Test_data_L <- New_data[-split, ]

#STEP_2 Training the model
model <- glm(Logistic_reg.Unsafe ~ Logistic_reg.Weight, data = Train_data_L, family = binomial)
summary(model)
#a) From the above summary, we fail to reject the null hypothesis that the regression co-efficients are 0, because the currosponding P-value is sigificant.

#STEP_3 Testing the model
predict <- predict(model, type = "response")
predict
#b) Have a problem interpreting the result to geth the equation.