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
#developing the model

model <- glm(class ~ . , df.train, family = "binomial")
model
pred <- predict(model, na.omit(df.validate), type = "response")
pred
pred.1 <- ifelse(pred <= .5, "diabetic", "normal")
table(na.omit(df.validate$class), pred.1, dnn=c("Actual", "Predicted"))
(138+42)/(138+16+35+42)
.77