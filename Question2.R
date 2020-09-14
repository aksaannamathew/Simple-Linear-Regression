library(readr)
delivery <- read.csv("C:\\Users\\91755\\Desktop\\Assignment\\14 - Simple Linear\\delivery_time.csv")
View(delivery)
colnames(delivery) <- c("DT", "ST")
View(delivery)
attach(delivery)
summary(delivery)
cor(delivery) #cor=0.8259973

install.packages("psych")
library(psych)

#Linear Regression
plot(delivery)
reg <- lm(delivery$DT~delivery$ST)
summary(reg) #R^2= 0.6823
confint(reg, level = 0.95)
pred <- predict(reg, interval = "predict")
sqrt(mean(reg$residuals)^2) #RMSE=4.758099e-17
cor(pred, delivery$DT) #Accuracy=0.8259973

#Logaritmic Transformation
plot(log(ST),DT)
reg1 <- lm(DT~log(ST))
summary(reg1) #R^2=0.6954
confint(reg1, level = 0.95)
pred1 <- predict(reg1, newdata = delivery)
sqrt(mean(reg1$residuals)^2) #RMSE=1.863589e-16
cor(pred1, delivery$DT) #Accuracy=0.8339325

#Exponential Transformation
plot(log(DT),ST)
reg2 <- lm(log(DT)~ST)
summary(reg2) #R^2=0.7109
confint(reg2, level = 0.95)
log_pred2 <- predict(reg2, interval = "predict")
pred2 <- exp(log_pred2)
sqrt(mean(reg2$residuals)^2)#RMSE=2.312965e-18
cor(pred2, delivery$DT) #Accuracy=0.8085780

#Quadratic equation raised to the power 2
plot(ST^2,DT)
reg3 <- lm(delivery$DT~delivery$ST+ I(ST^2))
summary(reg3) #R^2=0.6934
confint(reg3, level = 0.95)
pred3 <- predict(reg3, interval = "predict")
sqrt(mean(reg3$residuals)^2) #RMSE=4.22116e-17
cor(pred3, delivery$DT) #Accuracy=0.8327302

#Quadratic equation raised to the power 2
plot(ST^3,DT)
reg4 <- lm(delivery$DT~delivery$ST + I(ST^2) + I(ST^3))
summary(reg4) #R^2=0.7034
confint(reg4, level = 0.95)
pred4 <- predict(reg4, interval = "predict")
sqrt(mean(reg4$residuals)^2) #RMSE=1.321694e-18
cor(pred4, delivery$DT) #Accuracy= 0.8387088

#exponential transformation posses the highest r^2 value so it is the best model
plot(reg2)
