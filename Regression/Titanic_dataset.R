TITANIC <- read.csv("Titanictrain.csv", header = TRUE)
names(TITANIC)
attach(TITANIC)
a1 <- TITANIC [ ,2]
b1 <- TITANIC [ ,6]
df <- data.frame(a1,b1)
dim(df)
names(df)
library(imputeTS)
df$b1  <- na.mean(df$b1, option = "mean")
df
attach(df)
a1 = factor(a1,levels=c(0,1),labels=c("Dead", "Alive"));
a1;
model1 = glm(a1~b1, data = df, family = binomial)
library(lmtest)
lrtest(model1)
