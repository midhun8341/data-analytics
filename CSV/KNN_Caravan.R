# Caravan data set includes 85 predictors that measure
# demographic characteristics for 5,822 individuals.
# The response variable is Purchase, which indicates whether or 
# not a given individual purchases a caravan insurance policy. 
# In this data set, only 6% of people purchased caravan insurance.

library (ISLR) # install packages if required
library(class)
dim(Caravan)

attach (Caravan )
summary (Purchase )
str(Caravan)

# standardize the data so that all standardize  variables are given 
# a mean of zero and a standard deviation of one.
# Then all variables will be on a comparable scale.
# exclude column 86, because that is the qualitative Purchase variable

standardized.X=scale(Caravan [,-86])

test =1:1000
train.X=standardized.X[-test ,]
test.X=standardized.X[test ,]
train.Y=Purchase [-test]
test.Y=Purchase [test]
set.seed (1)
knn.pred=knn(train.X,test.X,train.Y,k=1)
mean(test.Y!= knn.pred)
table(knn.pred ,test.Y)
9/75

knn.pred=knn(train.X,test.X,train.Y,k=3)
table(knn.pred ,test.Y)
5/26

knn.pred=knn(train.X,test.X,train.Y,k=5)
table(knn.pred ,test.Y)
4/15
