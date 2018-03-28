loc <- "http://archive.ics.uci.edu/ml/machine-learning-databases/"

ds <- "pima-indians-diabetes/pima-indians-diabetes.data"
url <- paste(loc, ds, sep="")
diabetes <- read.table(url, sep=",", header=FALSE)
names(diabetes) <- c("npregant", "plasma", "bp", "triceps",
                     "insulin", "bmi", "pedigree", "age", "class")
diabetes$class <- factor(diabetes$class, levels=c(0,1),
                         labels=c("normal", "diabetic"))
df <- diabetes
set.seed(1234)
train <- sample(nrow(df), 0.7 *nrow(df))
df.train <- df[train,]
df.validate <- df[-train,]
table(df.train$class)
table(df.validate$class)

names(diabetes)

#Random forest
library(randomForest)
model <- randomForest(class ~ . , df.train, mtry = 8, importance = TRUE, type = "binomial")
model
pred <- predict(model, na.omit(df.validate))
pred
table(na.omit(df.validate$class), pred, dnn=c("Actual", "Predicted"))
(129+45)/(129+45+25+32)
0.75