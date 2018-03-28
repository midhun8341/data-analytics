sales.advert <- read.csv("Sales raw.csv")
cor(sales.advert$sales, sales.advert$adverts) #it's got a correlation of about 57%.
scatter.smooth(sales.advert$adverts, sales.advert$sales,)
cov(sales.advert$adverts, sales.advert$sales)

#splitting the data set
set.seed(35)
split <- sample(1: nrow(sales.advert), .7 * nrow(sales.advert))
train_data <- sales.advert[split, ]
test_data <- sales.advert[-split, ]

#developing the model.
model <- lm(sales ~ scaled_advert.train , train_data)
summary(model)
#prediction
pred <- predict(model, test_data)
summary(pred)
length(pred)
length(test_data$sales)

mean(pred)
mean(test_data$sales)
test_data$sales_table <- ifelse(test_data$sales <= 192, "no", "yes")
pred_table <- ifelse(pred <= 183, "no", "yes")

table(pred_table, test_data$sales_table)
(26+18)/(26+11+5+18)
#PPR <- 73%
summary(sales.advert)

X <- (sales.advert$adverts - mean(sales.advert$adverts))^2/sd(sales.advert$adverts)
summary(X)
scaled_advert <- sqrt(X)
summary(scaled_advert)
train_data$adverts
sales.advert.new <- data.frame(sales.advert, scaled_advert)
set.seed(35)
split.1 <- sample(1: nrow(sales.advert.new), .7 * nrow(sales.advert.new))
train_data.1 <- sales.advert.new[split, ]
test_data.1 <- sales.advert.new[-split, ]

#devolepling the new model.
model.1 <- lm(sales ~ scaled_advert, train_data.1)
summary(model)
summary(scaled_advert)
scatter.smooth(scaled_advert, sales.advert$sales)
scaled_sales <- (sales.advert$sales - mean(sales.advert$sales))^2/sd(sales.advert$sales)
sales.advert.new.1 <- data.frame(sales.advert.new, scaled_sales)
scatter.smooth(scaled_sales, scaled_advert)
log_sales <- log(sales.advert$sales)
log_advert <- log(sales.advert$adverts)
scatter.smooth(scaled_advert , log_sales)
cor(log_sales,scaled_advert)

# all the transformation give us bad correlation.