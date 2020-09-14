install.packages("readr")
library(readr)
calories <- read.csv("C:\\Users\\91755\\Desktop\\14 - Simple Linear\\calories_consumed.csv")
View(calories)
summary(calories)
colnames(calories) <- c("wg", "cc")
View(calories)
attach(calories)

install.packages("psych")
library(psych)

cor(calories) #cor= 0.94(strong correlation)


#Linear regression
plot(calories)
reg <- lm(calories$wg~calories$cc)
summary(reg) #R^2= 0.896
confint(reg, level = 0.95)
pred <- predict(reg, newdata = calories)
sqrt(mean(reg$residuals)^2)#RMSE= 4.822036e-15
cor(pred, calories$wg) #Accuracy = 0.946991


#Logarithmetic Transformation
plot(log(cc), wg)
reg1 <- lm(wg~log(cc))
summary(reg1) #R^2= 0.8077
confint(reg1, level = 0.95)
pred1 <- predict(reg1, newdata = calories)
sqrt(mean(reg1$residuals)^2) #RMSE= 3.552714e-15
cor(pred1, calories$wg) #Accuracy= 0.8987253

#Exponential Transformation
plot(log(wg), cc)
reg2 <- lm(log(wg)~cc)
summary(reg2) #R^2= 0.8776
confint(reg2, level = 0.95)
log_pred2 <- predict(reg2, newdata = calories)
pred2 <- exp(log_pred2)
sqrt(mean(reg2$residuals)^2) #RMSE= 2.478176e-19
cor(pred2, calories$wg) #Accuracy= 0.9437992

#Quadratic transformation raised to the power 2
plot(cc^2, wg)
reg3 <- lm(calories$wg~calories$cc +I(cc^2))
summary((reg3)) #R^2= 0.9521
confint(reg3, level = 0.95)
pred3 <- predict(reg3, newdata = calories)
sqrt(mean(reg3$residuals)^2) #RMSE= 4.949414e-15
cor(pred3, calories$wg) #Accuracy= 0.9757338

#Quadratic transformation raised to the power 3
plot(cc^3, wg)
reg4 <- lm(calories$wg~calories$cc + I(cc^2) + I(cc^3))
summary(reg4) #R^2= 0.9811
confint(reg4, level = 0.95)
pred4 <- predict(reg4, newdata = calories)
sqrt(mean(reg4$residuals)^2) #RMSE= 5.712197e-16
cor(pred4, calories$wg) #Accuracy= 0.9905292

#Quadratic transformation raised to the power 3 is the best model
plot(reg4)
