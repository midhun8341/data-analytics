data <- read.csv("Snap_Deal", sep = ",")

data$Discount = gsub('[[:punct:]]', '', data$Discount)
data$Actual.Prices = gsub('[[:punct:]]', '', data$Actual.Prices)
data$Discount.Prices = gsub('[[:punct:]]', '', data$Discount.Prices)
data$Names = gsub('[[:punct:]]', '', data$Names)

data$Names = gsub('[0-9]', '', data$Names)

data$Discount = gsub("[a-zA-Z]", '', data$Discount)

data$Actual.Prices = gsub("[a-zA-Z]", '', data$Actual.Prices)

data$Discount.Prices = gsub("[a-zA-Z]", '', data$Discount.Prices)

attach(data)
Actual_Discount <- as.integer(Actual.Prices) - as.integer(Discount.Prices)
head(Actual_Discount)
Act_disc <- as.data.frame(Actual_Discount)
X <- cbind(data, Act_disc)


write.csv(X, file = "Snap_deall")
