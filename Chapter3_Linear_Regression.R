library(MASS)
library(ISLR)
#Simple Linear Regression
names(Boston)
dim(Boston)
head(Boston)
summary(Boston)
str(Boston)
#to have linear regressions
lm.fit <- lm(medv ~ lstat, data = Boston)
#to get the coefficients
lm.fit
#to get the necessary regression information
summary(lm.fit)
#to find out all pieces of information
names(lm.fit)
#to get the coefficients
coef(lm.fit)
#to get the 95% confidence interval for the coefficients
confint(lm.fit)
#to get the prediction values and the confidence intervals for all the towns
predict(lm.fit, data.frame(lstat = c(5, 10, 15)), 
        interval = "confidence")
#to get the prediction values and the prediction intervals for a paticular city
predict(lm.fit, data.frame(lstat = c(5, 10, 15)), 
        interval = "prediction")
#to draw the points and lines
plot(Boston$lstat, Boston$medv)
abline(lm.fit)
abline(lm.fit, lwd = 3)
abline(lm.fit, lwd = 3, col = "red")

lm.fit <- lm(medv ~ lstat + age, data = Boston)
summary(lm.fit)
lm.fit <- lm(medv ~ ., data = Boston)
summary(lm.fit)
