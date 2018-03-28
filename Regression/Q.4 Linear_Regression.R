#Q4
Fitness_1 <- read.csv("FITNESS.csv")
#a) Checking with runtime.

#STEP_1 check for linearity.
scatter.smooth(Fitness_1$Oxygen_Consumption, Fitness_1$RunTime, main = Scatter_plot)
#It shows a strong negative linear relationship with the outcome variable.

#STEP_2 Correlation
cor(Fitness_1$Oxygen_Consumption, Fitness_1$RunTime)
#-0.86 

#STEP_3 devoleping the model
fit1 <- lm(Oxygen_Consumption ~ RunTime, data = Fitness_1 )
summary(fit1)
#Multiple_R_squared_1 = 0.7434

#b) Checking with Runtine, Run_pluse, Maximum_pluse.

#STEP_1 check for linearity.
par(mfrow = c(1,3))
scatter.smooth(Fitness_1$Oxygen_Consumption, Fitness_1$Run_Pulse, main = Scatter_plot_Runpulse)
scatter.smooth(Fitness_1$Oxygen_Consumption, Fitness_1$RunTime, main = Scatter_plot_Runtime)
scatter.smooth(Fitness_1$Oxygen_Consumption, Fitness_1$Maximum_Pulse, main = Scatter_plot_maximumpulse)
#It is very clearly evidental that unlike run_time, neither run_pluse nor max_pluse share a linear relationship with Oxygen_consumption.

#STEP_2 Correlation
cor(Fitness_1$Oxygen_Consumption, Fitness_1$RunTime)
cor(Fitness_1$Oxygen_Consumption, Fitness_1$Run_Pulse)
cor(Fitness_1$Oxygen_Consumption, Fitness_1$Maximum_Pulse)
#Both run_pulse and max_pulse have a poor correlationship with the response variable.

#STEP_3 devoleping the model
fit2 <- lm(Oxygen_Consumption ~ RunTime + Run_Pulse + Maximum_Pulse , data = Fitness_1 )
summary(fit2)
#Adjusted R-squared_2 =  0.789 

#c) Checking with Runtine, Age, Weight, Run_pluse, Maximum_pluse

#STEP_1 check for linearity.
par(mfrow = c(1,5))
scatter.smooth(Fitness_1$Oxygen_Consumption, Fitness_1$RunTime, main = Scatter_plot_Runtime)
scatter.smooth(Fitness_1$Oxygen_Consumption, Fitness_1$Weight, main = Scatter_plot_Weight)
scatter.smooth(Fitness_1$Oxygen_Consumption, Fitness_1$Run_Pulse, main = Scatter_plot_Runpulse)
scatter.smooth(Fitness_1$Oxygen_Consumption, Fitness_1$Age, main = Scatter_plot_Age)
scatter.smooth(Fitness_1$Oxygen_Consumption, Fitness_1$Maximum_Pulse, main = Scatter_plot_maximumpulse)
#Like in the case before, only Run_time seems to be linearly related with the outcome variable.

#STEP_2 Correlation
cor(Fitness_1$Oxygen_Consumption, Fitness_1$RunTime)
cor(Fitness_1$Oxygen_Consumption, Fitness_1$Run_Pulse)
cor(Fitness_1$Oxygen_Consumption, Fitness_1$Maximum_Pulse)
cor(Fitness_1$Oxygen_Consumption, Fitness_1$Weight)
cor(Fitness_1$Oxygen_Consumption, Fitness_1$Age)
#The same goes with correlation as well.

#STEP_3 devoleping the model
fit3 <- lm(Oxygen_Consumption ~ RunTime + Run_Pulse + Maximum_Pulse + Age + Weight, data = Fitness_1 )
summary(fit3)
#In this case, the variavle weight has a higher p-value, increasing the chance of accepting its null hypothesis.
#Therefore we shall reject it.

#The best fit model here appears to be the 1'st one where we considered just the run_time as the predictor variable.

