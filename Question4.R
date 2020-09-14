library(readr)
Salary_Data <- read.csv("C:\\Users\\91755\\Desktop\\Assignment\\14 - Simple Linear\\Salary_Data.csv")
attach(Salary_Data)
View(Salary_Data)
summary(Salary_Data)
cor(Salary_Data) #cor=0.9782416

library(psych)

#Linear Regression
plot(Salary_Data)
reg <- lm(Salary_Data$Salary~Salary_Data$YearsExperience)
summary(reg) #R^2=0.957
confint(reg, level = 0.95)
pred <- predict(reg, interval = "predict")
sqrt(mean(reg$residuals)^2) #RMSE=2.615537e-13
cor(pred, Salary_Data$Salary) #Accuracy=0.9782416

#Logarithmic Transformation
plot(log(YearsExperience), Salary)
reg1 <- lm(Salary_Data$Salary~log(YearsExperience))
summary(reg1) #R^2=0.8539
confint(reg1, level = 0.95)
pred1 <- predict(reg1, interval = "predict")
sqrt(mean(reg1$residuals)^2) #RMSE=1.622051e-12
cor(pred1, Salary_Data$Salary) #Accuracy=0.9240611

#Exponential Transformation
plot(log(Salary), YearsExperience)
reg2 <- lm(log(Salary)~YearsExperience)
summary(reg2) #R^2=0.932
confint(reg2, level = 0.95)
log_pred2 <- predict(reg2, interval = "predict")
pred2 <- exp(log_pred2)
sqrt(mean(reg2$residuals)^2) #RMSE=5.26335e-18
cor(pred2, Salary_Data$Salary) #Accuracy=0.9660470

#Quadratic Transformation raised to the power 2
plot(YearsExperience^2, Salary)
reg3 <- lm(Salary_Data$Salary~Salary_Data$YearsExperience + I(YearsExperience^2))
summary(reg3) #R^2=0.957
confint(reg3, level = 0.95)
pred3 <- predict(reg3, interval = "predict")
sqrt(mean(reg3$residuals)^2) #RMSE=3.202623e-13
cor(pred3, Salary_Data$Salary) #Accuracy=0.9782511

#Quadratic Transformation raised to the powe 3
plot(YearsExperience^3, Salary)
reg4 <- lm(Salary_Data$Salary~Salary_Data$YearsExperience + I(YearsExperience^2) + I(YearsExperience^3))
summary(reg4) #R^2=0.9636
confint(reg4, level = 0.95)
pred4 <- predict(reg4, interval = "predict")
sqrt(mean(reg4$residuals)^2) #RMSE=7.202757e-14
cor(pred4, Salary_Data$Salary) #Accuracy=0.9816298

#quadratic transformation raised to the power 3 posses the high R^2 value so it is the best model
plot(reg4)
