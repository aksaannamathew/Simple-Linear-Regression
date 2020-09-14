library(readr)
emp_data <- read.csv("C:\\Users\\91755\\Desktop\\Assignment\\14 - Simple Linear\\emp_data.csv")
View(emp_data)
attach(emp_data)
summary(emp_data)

library(psych)
cor(emp_data) #cor=-0.9117216

#Linear Regression
plot(emp_data)        
reg <- lm(emp_data$Churn_out_rate~emp_data$Salary_hike)
summary(reg) #R^2= 0.8312
confint(reg, level = 0.95)
pred <- predict(reg, interval = "predict")
sqrt(mean(reg$residuals)^2) #RMSE=4.449566e-17
cor(pred, emp_data$Churn_out_rate) #Accuracy=0.9117216

#Logarithmic Transformation
plot(log(Salary_hike),Churn_out_rate)
reg1 <- lm(Churn_out_rate~log(Salary_hike))
summary(reg1) #R^2=0.8486
confint(reg1, level = 0.95)
pred1 <- predict(reg1, interval = "predict")
sqrt(mean(reg1$residuals)^2) #RMSE=4.885849e-16
cor(pred1, emp_data$Churn_out_rate) #Accuracy=0.9212077

#Exponential Transformation
plot(log(Churn_out_rate),Salary_hike)
reg2 <- lm(log(Churn_out_rate)~Salary_hike)
summary(reg2) #R^2=0.8735
confint(reg2, level = 0.95)
log_pred2 <- predict(reg2, interval = "predict")
pred2 <- exp(log_pred2)
sqrt(mean(reg2$residuals)^2) #RMSE=2.083023e-18
cor(pred2, emp_data$Churn_out_rate) #Accuracy=0.9334219

#Quadratic Transformation raised to the power 2
plot(Salary_hike^2,Churn_out_rate)
reg3 <- lm(emp_data$Churn_out_rate~ emp_data$Salary_hike + I(Salary_hike^2))
summary(reg3) #R^2=0.9737
confint(reg3, level = 0.95)
pred3 <- predict(reg3, interval = "predict")
sqrt(mean(reg3$residuals)^2) #RMSE=1.221571e-16
cor(pred3, emp_data$Churn_out_rate) #Accuracy=0.9867642

#Quadratic Transformation raised to the power 3
plot(Salary_hike^3, Churn_out_rate)
reg4 <- lm(emp_data$Churn_out_rate~ emp_data$Salary_hike + I(Salary_hike^2) + I(Salary_hike^3))
summary(reg4) #R^2=0.9893
confint(reg4, level = 0.95)
pred4 <- predict(reg4, interval = "predict")
sqrt(mean(reg4$residuals)^2) #RMSE=8.882597e-17
cor(pred4, emp_data$Churn_out_rate) #Accuracy=0.9946502

##quadratic transformation raised to the power 3 posses thge highest r^2 value so it is the best model
plot(re(reg4))
