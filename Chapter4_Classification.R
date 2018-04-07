######################################################################
#Logistic Model######################################################
######################################################################
library(ISLR)
names(Default)
dim(Default)
head(Default)
summary(Default)
str(Default)
library(ggplot2)
ggplot(Default, aes(x = balance, y = income, colour = default)) + geom_point()
par(mfrow = c(1, 2))
boxplot(income ~ default, data = Default, xlab = "Default", ylab = "Income", col = c("blue","orange"))
boxplot(balance ~ default, data = Default, xlab = "Default", ylab = "Balance", col = c("blue", "orange"))
#to build the logistic model between default and balance
glm.fit1 <-glm(default ~ balance, family = binomial(link = "logit"), data = Default)
summary(glm.fit1)
par(mfrow = c(1, 1))
plot(Default$default ~ Default$balance)
lines(fitted(glm.fit1) ~ Default$balance)
#to make predictions with the balance of $1000 and $2000
predict.glm(glm.fit1, data.frame(balance = 1000),type = "response")
predict.glm(glm.fit1, data.frame(balance = 2000),type = "response")
#to build the logistic model between default and student
glm.fit2 <- glm(default ~ student, family = binomial(link = "logit"), data = Default)
summary(glm.fit2)
#to make predictions for students and non-students.
predict.glm(glm.fit2, data.frame(student = "Yes"),type = "response")
predict.glm(glm.fit2, data.frame(student = "No"),type = "response")
glm.fit3 <- glm(default ~ balance + income + student, data = Default, family = binomial(link = "logit")) 
summary(glm.fit3)
#to select the model with hybrid method.
step(glm.fit3, direction = "both")
#to model with balance and student
glm.fit4 <- glm(default ~ balance + student, data = Default, family = binomial(link = "logit")) 
summary(glm.fit4)
par(mfrow = c(1, 1))
boxplot(balance~student, data = Default, 
        xlab = "Student Status", ylab = "Credit Card Balance",
        col = c("blue", "orange"))
# to divide [0, 2700] into N parts and calculate the default rate for students and non-students repectively. 
N = 10
student_default_rate <- rep(NA, N)
non_student_default_rate <- rep(NA, N)
interval_len <- 2700/N 
for(i in 1:N){
  student_default <- Default[which(Default$balance <= interval_len * i-1 & Default$balance >= (i - 1) * interval_len 
                                   & Default$student == "Yes"), "default"]
  non_student_default <- Default[which(Default$balance <= interval_len * i-1 & Default$balance >= (i - 1) * interval_len
                                   & Default$student == "No"), "default"]
  student_default_rate[i] <- sum(student_default == "Yes") / length(student_default)
  non_student_default_rate[i] <-sum(non_student_default == "Yes") / 
                                length(non_student_default)
}
par(mfrow = c(1,1))
balance <- rep(NA, times = N)
for(i in 1:N){
  balance[i] <- interval_len/2 + interval_len * (i - 1)
}

plot(student_default_rate ~ balance, 
     xlab = "Credit Card Balance", ylab = "Default Rate",
     type = "l", col = "orange", lwd = 3)
lines(non_student_default_rate  ~ balance, col = "blue", lwd = 3)
student_average_default_rate <- 
  length(Default[Default$student == "Yes" & Default$default == "Yes", 1])/
  length(Default[Default$student == "Yes", 1])
non_student_average_default_rate <-  length(Default[Default$student == "No" & Default$default == "Yes", 1])/
  length(Default[Default$student == "No", 1])
abline(h = student_average_default_rate, lwd = 2, lty = 2, col = "orange")
abline(h = non_student_average_default_rate, lwd = 2, lty = 2 , col = "blue")
legend("topleft", inset = .05, title = "default Comparison",c("Student","Non-Student"), 
       lty = c(1,1), col = c("orange","blue"))
predict.glm(glm.fit3, data.frame(student = "Yes", balance = 1500, income = 40000),type = "response")
predict.glm(glm.fit3, data.frame(student = "No", balance = 1500, income = 40000),type = "response")
#####################################################################
#linear discriminant analysis########################################
#####################################################################
library(MASS)
default_forecast <- lda(default ~ balance + student, data = Default)
predict_group <- predict(default_forecast)$class
original_group <- Default$default
table(original_group, predict_group)
prop.table(table(original_group, predict_group))
#to calculate the classification error rate 
1 - sum(diag(prop.table(table(original_group, predict_group))))
names(predict(default_forecast))
#to calculate the error rates with threshold at 0.2 
data1 <- as.numeric(predict(default_forecast)$posterior[, "Yes"] > 0.2)
predict_group2 <- factor(data1, labels = c("No", "Yes"))
table(original_group, predict_group2)
prop.table(table(original_group, predict_group2))
#to calulate the classification error rate 
1 - sum(diag(prop.table(table(original_group, predict_group2))))
#to draw the picture between threshold and different error rates
threshold <- seq(from = 0, to = 0.99, by = 0.01)
classification_error_rate <- rep(NA, times = length(threshold))
classification_error_rate_of_default <- rep(NA, times = length(threshold))
classification_error_rate_of_non_default <- rep(NA, times = length(threshold))
for(i in 1:length(threshold)){
  predict_group_new <- as.numeric(predict(default_forecast)$posterior[, "Yes"] > threshold[i])
  #predict_group_new <- factor(predict_new, labels = c("No", "Yes"))
  original_group2 <- as.numeric(original_group) - 1
  table_new <- table(original_group2, predict_group_new)
  #to calulate three classification error rates
  if(sum(predict_group_new) == 0){
        classification_error_rate[i] =  sum(original_group2)/length(original_group2)
        classification_error_rate_of_default[i] = 1
        classification_error_rate_of_non_default[i] = 0
  }else if(sum(predict_group_new) == length(original_group2)){
        classification_error_rate[i] =  1 - sum(original_group2) / length(original_group2)
        classification_error_rate_of_default[i] = 0
        classification_error_rate_of_non_default[i] = 1
  }else{
        classification_error_rate[i] <- (table_new[1,2] + table_new[2,1]) / 
                                       dim(Default)[1] 
      classification_error_rate_of_default[i] <- table_new[2,1] / 
                                                 sum(table_new[2,])  
      classification_error_rate_of_non_default[i] <- table_new[1,2] / 
                                                     sum(table_new[1,])
  }
}
plot(classification_error_rate ~ threshold, 
     xlab = "Threshold", ylab = "Error Rate",
     type = "l", col = "orange", lwd = 3, ylim = c(-0.1, 1.1))
lines(classification_error_rate_of_default ~ threshold, col = "blue", 
      lty = 2, lwd = 3)
lines(classification_error_rate_of_non_default~ threshold, 
      col = "red", lty = 3,lwd = 3)
legend("topleft", inset = .05, title = "Comparison",
       c("All","Default","NoDefault"), lty = c(1,2,3), 
       col = c("orange","blue","red"), lwd = 3)
#ROC Curve
plot(y = 1 - classification_error_rate_of_default, 
     x = classification_error_rate_of_non_default, 
     type = "l", lwd = 3, lty = 1,
     xlab = "False Positive Rate", ylab = "True Positive Rate",
     main = "ROC Curve", xlim = c(0, 1), ylim = c(0, 1))
################################################################################
#comparison of LDA, QDA, Logistic, KNN##########################################
#scenario1######################################################################
################################################################################
par(mfrow = c(2, 2))
library(MASS)
library(class)
N <- 100
sigma <- matrix(c(1, 0, 0, 1), ncol = 2, nrow = 2, byrow = TRUE)
mu0 <- c(0, 0)
mu1 <- c(3, 3)
error_rate <- data.frame(error_rate = NA, 
                         method = c(rep("lda", N), rep("qda", N),
                                    rep("logistic", N), rep("knn", N)))
for(k in 1:N){
      group0 <- mvrnorm(n = 1020, mu = mu0, Sigma = sigma)
      group1 <- mvrnorm(n = 1020, mu = mu1, Sigma = sigma)
      group <- c(rep(0, times = 1020), rep(1, times = 1020))
      simulated_data <- data.frame(x = c(group1[, 1], group2[, 1]), 
                                   y = c(group1[, 2], group2[, 2]),
                                   group = group)
      training_data <- simulated_data[c(1:20, 1021:1040),]
      testing_data <- simulated_data[-c(1:20, 1021:1040), -3]
      ##########################################################################
      #lda model################################################################
      ##########################################################################
      lda_fit <- lda(group ~ ., data = training_data)
      lda_prediction <-predict(lda_fit, newdata = testing_data)
      error_rate[k, 1] <- 
      mean(lda_prediction$class != simulated_data[-c(1:20, 1021:1040), 3])
      ##########################################################################
      #qda model################################################################
      ##########################################################################
      qda_fit <- qda(group ~ ., data = training_data)
      qda_prediction <-predict(qda_fit, newdata = testing_data)
      error_rate[k + N, 1] <- 
      mean(qda_prediction$class != simulated_data[-c(1:20, 1021:1040), 3])
      ##########################################################################
      #logistic model###########################################################
      ##########################################################################
      glm_fit <- glm(group ~ ., data = training_data, 
                     family = binomial(link = "logit"))
      glm_probs <- predict(glm_fit, newdata = testing_data, type = "response")
      error_rate[k + 2 * N, 1] <- 
      mean(as.numeric(glm_probs > 0.5) != simulated_data[-c(1:20, 1021:1040), 3])
      ##########################################################################
      #knn######################################################################
      ##########################################################################
      knn_prediction <- knn(train = training_data[, 1:2], 
                test = testing_data, 
                cl = training_data[, 3], 
                k=1)
      error_rate[k + 3 * N, 1] <-
      mean(knn_prediction != simulated_data[-c(1:20, 1021:1040), 3])
}
 boxplot(error_rate ~ method, data = error_rate, 
         main = "scenario1", col = c("orange","blue","red","purple"))
################################################################################
#comparison of LDA, QDA, Logistic, KNN##########################################
#scenario2######################################################################
################################################################################
library(MASS)
library(class)
N <- 100
sigma <- matrix(c(1, -0.5, -0.5, 1), ncol = 2, nrow = 2, byrow = TRUE)
mu0 <- c(0, 0)
mu1 <- c(3, 3)
error_rate <- data.frame(error_rate = NA, 
                         method = c(rep("lda", N), rep("qda", N),
                                    rep("logistic", N), rep("knn", N)))
for(k in 1:N){
      group0 <- mvrnorm(n = 1020, mu = mu0, Sigma = sigma)
      group1 <- mvrnorm(n = 1020, mu = mu1, Sigma = sigma)
      group <- c(rep(0, times = 1020), rep(1, times = 1020))
      simulated_data <- data.frame(x = c(group1[, 1], group2[, 1]), 
                                   y = c(group1[, 2], group2[, 2]),
                                   group = group)
      training_data <- simulated_data[c(1:20, 1021:1040),]
      testing_data <- simulated_data[-c(1:20, 1021:1040), -3]
      ##########################################################################
      #lda model################################################################
      ##########################################################################
      lda_fit <- lda(group ~ ., data = training_data)
      lda_prediction <-predict(lda_fit, newdata = testing_data)
      error_rate[k, 1] <- 
      mean(lda_prediction$class != simulated_data[-c(1:20, 1021:1040), 3])
      ##########################################################################
      #qda model################################################################
      ##########################################################################
      qda_fit <- qda(group ~ ., data = training_data)
      qda_prediction <-predict(qda_fit, newdata = testing_data)
      error_rate[k + N, 1] <- 
      mean(qda_prediction$class != simulated_data[-c(1:20, 1021:1040), 3])
      ##########################################################################
      #logistic model###########################################################
      ##########################################################################
      glm_fit <- glm(group ~ ., data = training_data, 
                     family = binomial(link = "logit"))
      glm_probs <- predict(glm_fit, newdata = testing_data, type = "response")
      error_rate[k + 2 * N, 1] <- 
      mean(as.numeric(glm_probs > 0.5) != simulated_data[-c(1:20, 1021:1040), 3])
      ##########################################################################
      #knn######################################################################
      ##########################################################################
      knn_prediction <- knn(train = training_data[, 1:2], 
                test = testing_data, 
                cl = training_data[, 3], 
                k=1)
      error_rate[k + 3 * N, 1] <-
      mean(knn_prediction != simulated_data[-c(1:20, 1021:1040), 3])
}
 boxplot(error_rate ~ method, data = error_rate, 
         main = "scenario2", col = c("orange","blue","red","purple"))
################################################################################
#comparison of LDA, QDA, Logistic, KNN##########################################
#scenario3######################################################################
################################################################################
library(MASS)
library(class)
library(mvtnorm)
N <- 100
sigma <- diag(2)
mu0 <- c(0, 0)
mu1 <- c(1, 1)
error_rate <- data.frame(error_rate = NA, 
                         method = c(rep("lda", N), rep("qda", N),
                                    rep("logistic", N), rep("knn", N)))
for(k in 1:N){
      group0 <- rmvt(n = 1020, sigma = sigma, df = 10)
      group1 <- rmvt(n = 1020, sigma = sigma, df = 10, delta = rep(1, nrow(sigma)))
      group <- c(rep(0, times = 1020), rep(1, times = 1020))
      simulated_data <- data.frame(x = c(group1[, 1], group2[, 1]), 
                                   y = c(group1[, 2], group2[, 2]),
                                   group = group)
      training_data <- simulated_data[c(1:20, 1021:1040),]
      testing_data <- simulated_data[-c(1:20, 1021:1040), -3]
      ##########################################################################
      #lda model################################################################
      ##########################################################################
      lda_fit <- lda(group ~ ., data = training_data)
      lda_prediction <-predict(lda_fit, newdata = testing_data)
      error_rate[k, 1] <- 
      mean(lda_prediction$class != simulated_data[-c(1:20, 1021:1040), 3])
      ##########################################################################
      #qda model################################################################
      ##########################################################################
      qda_fit <- qda(group ~ ., data = training_data)
      qda_prediction <-predict(qda_fit, newdata = testing_data)
      error_rate[k + N, 1] <- 
      mean(qda_prediction$class != simulated_data[-c(1:20, 1021:1040), 3])
      ##########################################################################
      #logistic model###########################################################
      ##########################################################################
      glm_fit <- glm(group ~ ., data = training_data, 
                     family = binomial(link = "logit"))
      glm_probs <- predict(glm_fit, newdata = testing_data, type = "response")
      error_rate[k + 2 * N, 1] <- 
      mean(as.numeric(glm_probs > 0.5) != simulated_data[-c(1:20, 1021:1040), 3])
      ##########################################################################
      #knn######################################################################
      ##########################################################################
      knn_prediction <- knn(train = training_data[, 1:2], 
                test = testing_data, 
                cl = training_data[, 3], 
                k=1)
      error_rate[k + 3 * N, 1] <-
      mean(knn_prediction != simulated_data[-c(1:20, 1021:1040), 3])
}
 boxplot(error_rate ~ method, data = error_rate, 
         main = "scenario3", col = c("orange","blue","red","purple"))
 ################################################################################
#comparison of LDA, QDA, Logistic, KNN##########################################
#scenario4######################################################################
################################################################################
library(MASS)
library(class)
N <- 100
sigma1 <- matrix(c(1, 0.5, 0.5, 1), ncol = 2, nrow = 2, byrow = TRUE)
sigma2 <- matrix(c(1, -0.5, -0.5, 1), ncol = 2, nrow = 2, byrow = TRUE)
mu0 <- c(0, 0)
mu1 <- c(1, 1)
error_rate <- data.frame(error_rate = NA, 
                         method = c(rep("lda", N), rep("qda", N),
                                    rep("logistic", N), rep("knn", N)))
for(k in 1:N){
      group0 <- mvrnorm(n = 1020, mu = mu0, Sigma = sigma1)
      group1 <- mvrnorm(n = 1020, mu = mu1, Sigma = sigma2)
      group <- c(rep(0, times = 1020), rep(1, times = 1020))
      simulated_data <- data.frame(x = c(group1[, 1], group2[, 1]), 
                                   y = c(group1[, 2], group2[, 2]),
                                   group = group)
      training_data <- simulated_data[c(1:20, 1021:1040),]
      testing_data <- simulated_data[-c(1:20, 1021:1040), -3]
      ##########################################################################
      #lda model################################################################
      ##########################################################################
      lda_fit <- lda(group ~ ., data = training_data)
      lda_prediction <-predict(lda_fit, newdata = testing_data)
      error_rate[k, 1] <- 
      mean(lda_prediction$class != simulated_data[-c(1:20, 1021:1040), 3])
      ##########################################################################
      #qda model################################################################
      ##########################################################################
      qda_fit <- qda(group ~ ., data = training_data)
      qda_prediction <-predict(qda_fit, newdata = testing_data)
      error_rate[k + N, 1] <- 
      mean(qda_prediction$class != simulated_data[-c(1:20, 1021:1040), 3])
      ##########################################################################
      #logistic model###########################################################
      ##########################################################################
      glm_fit <- glm(group ~ ., data = training_data, 
                     family = binomial(link = "logit"))
      glm_probs <- predict(glm_fit, newdata = testing_data, type = "response")
      error_rate[k + 2 * N, 1] <- 
      mean(as.numeric(glm_probs > 0.5) != simulated_data[-c(1:20, 1021:1040), 3])
      ##########################################################################
      #knn######################################################################
      ##########################################################################
      knn_prediction <- knn(train = training_data[, 1:2], 
                test = testing_data, 
                cl = training_data[, 3], 
                k=1)
      error_rate[k + 3 * N, 1] <-
      mean(knn_prediction != simulated_data[-c(1:20, 1021:1040), 3])
}
 boxplot(error_rate ~ method, data = error_rate, 
         main = "scenario4", col = c("orange","blue","red","purple"))
 par(mfrow = c(1,1))
###################################################################
#Lab: Logistic, LDA, QDA, KNN######################################
###################################################################
#The Stock Market Data#
#######################
library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)
#to calculate the correlation matrix
cor(Smarket[, -9])
#There is an ascending trend in the Volume.
plot(Smarket$Volume)
#####################
#Logistic Regression#
#####################
#We try to predict Direction with lag1--lag5 and volume
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
               data = Smarket, family = binomial)
summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef
summary(glm.fit)$coef[, 4]
glm.probs <- predict(glm.fit, type = "response")
glm.probs[1:10]
#to see the value for dummy variable
contrasts(Smarket$Direction)
glm.pred <- rep("Down", 1250)
glm.pred[glm.probs > .5] = "Up"
table(glm.pred, Direction = Smarket$Direction)
#the training error rate
1 - mean(glm.pred == Smarket$Direction)
train <- (Smarket$Year < 2005)
Smarket.2005 <- Smarket[!train, ]
dim(Smarket.2005)
Direction.2005 <- Smarket$Direction[!train]
glm.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
               data = Smarket, family = binomial(link = "logit"), 
               subset = train)
glm.probs <- predict(glm.fit, Smarket.2005, type = "response")
glm.pred <- rep("Down", 252)
glm.pred[glm.probs > .5] <- "Up"
table(glm.pred, Direction.2005)
#the testing error
1 - mean(glm.pred == Direction.2005)
#to have the logistic regression model with Lag1 and Lag2
glm.fit <- glm(Direction ~ Lag1 + Lag2, data = Smarket, 
               family = binomial(link = "logit"), subset = train)
glm.probs <- predict.glm(glm.fit, Smarket.2005, type = "response")
glm.pred <- rep("Down", 252)
glm.pred[glm.probs > .5] <- "Up"
table(glm.pred, Direction.2005)
1 - mean(glm.pred == Direction.2005)
predict.glm(glm.fit, 
            newdata = data.frame(Lag1 = c(1.2, 1.5), Lag2 = c(1.1, -0.8)),
            type = "response")
################################
#Linear Discriminant Analysis###
################################
library(MASS)
lda.fit <- lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
lda.fit
#the coefficients of linear discriminants means that 
#the dividing line is -0.642*Lag1 -0.514*Lag2
plot(lda.fit)
#to plot each one by calculating  -0.642*Lag1 -0.514*Lag2
lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class <-lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class == Direction.2005)
sum(lda.pred$posterior[, 2] > .5)
sum(lda.pred$posterior[, 2] < .5)
##########################################
#Quadratic Discriminant Analysis##########
##########################################
qda.fit <- qda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
qda.fit
qda.class = predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)
mean(qda.class == Direction.2005)
#####################################
##K-Nearest Neighbors###############
####################################
library(class)
train.X <- cbind(Smarket$Lag1, Smarket$Lag2)[train, ]
test.X <- cbind(Smarket$Lag1, Smarket$Lag2)[!train, ]
train.Direction <- Smarket$Direction[train]
set.seed(1)
#Given 1 neighbour
knn.pred <- knn(train = train.X, 
                test = test.X, 
                cl = train.Direction, 
                k=1)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)
#Given three neighbours
knn.pred2 <- knn(train = train.X, 
                test = test.X, 
                cl = train.Direction, 
                k=3)
table(knn.pred2, Direction.2005)
mean(knn.pred2 == Direction.2005)
####################################################################
#Caravan Insurance Data############################################
###################################################################
dim(Caravan)
summary(Caravan$Purchase)
standardized.Caravan <- scale(Caravan[, -86])
test <- 1:1000
train.Caravan <- standardized.Caravan[-test, ]
test.Caravan <- standardized.Caravan[test, ]
train.Purchase <- Caravan$Purchase[-test]
test.Purchase <- Caravan$Purchase[test]
set.seed(1)
knn.pred <- knn(train = train.Caravan,
                test = test.Caravan,
                cl = train.Purchase,
                k = 1)
mean(test.Purchase != knn.pred)
table(test.Purchase, knn.pred)
9/77
knn.pred <- knn(train = train.Caravan,
                test = test.Caravan,
                cl = train.Purchase,
                k = 3)
mean(test.Purchase != knn.pred)
table(test.Purchase, knn.pred)
5/26
knn.pred <- knn(train = train.Caravan,
                test = test.Caravan,
                cl = train.Purchase,
                k = 5)
mean(test.Purchase != knn.pred)
table(test.Purchase, knn.pred)
4/15
glm.fit <- glm(Purchase ~ ., data = Caravan, 
               family = binomial(link = "logit"), subset = -test)
glm.probs <- predict.glm(glm.fit, Caravan[test, ], 
                         type = "response")
glm.pred <- rep("No", 1000)
glm.pred[glm.probs > .25] <- "Yes"
table(glm.pred, test.Purchase)
11 / (22 + 11)
