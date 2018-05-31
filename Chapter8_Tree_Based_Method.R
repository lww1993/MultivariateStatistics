################################################################################
######regression trees##########################################################
################################################################################
library(tree)
library(ISLR)
#Major League Baseball Data from the 1986 and 1987 seasons######################
names(Hitters)
dim(Hitters)
#to delete the missing data#####################################################
sum(is.na(Hitters))
#to compute the number of missing data##########################################
Hitters2 <- na.omit(Hitters)
dim(Hitters2)
################################################################################
#to plot the figure8.1##########################################################
################################################################################
Hitters2$Log_salary <- log(Hitters2$Salary)
Hitters3 <- Hitters2[, c("Log_salary", "Hits", "Years")]
tree_hitters <- tree(Log_salary ~ ., Hitters3, minsize = 100)
summary(tree_hitters)
plot(tree_hitters)
text(tree_hitters, pretty = 0)
################################################################################
#to plot figure8.4##############################################################
################################################################################
Hitters4 <- Hitters2[, c("Log_salary", "Hits", "Years", "RBI", "Walks", "Runs", "PutOuts")]
tree_hitters2 <- tree(Log_salary ~ ., Hitters4)
summary(tree_hitters2)
plot(tree_hitters2)
text(tree_hitters2, pretty = 0)
################################################################################
#to compute the regression errors###############################################
################################################################################
#to compute the training error##################################################
tree_hitters3 <- tree(Log_salary ~ . - Salary, Hitters2)
training_error <- 0
for(k in 1:9){
  prediction <- predict(prune.tree(tree_hitters3, best = k + 1, method = "deviance"))
  training_error[k] <- mean((prediction - Hitters2[, "Log_salary"])^2)  
}
training_error
#to compute the test error######################################################
test_error <- 0
set.seed(1000)
training_set <- sample(x = dim(Hitters2)[1], size = floor(dim(Hitters2)[1] / 2))
tree_hitter4 <- tree(Log_salary ~ . - Salary, subset = training_set, Hitters2)
for(k in 1:9){
  prediction <- predict(prune.tree(tree_hitter4, best = k + 1, method = "deviance"),
                        newdata = Hitters2[-training_set, ])
  test_error[k] <- mean((prediction - Hitters2[-training_set, "Log_salary"]) ^ 2)  
}
test_error
#to compute the cross validation error with K fold##############################
cv_error <- 0
set.seed(1000)
K <- 10
groups <- sample(x = K, size = dim(Hitters2)[1], replace = TRUE)
for(k in 1:9){
  error <- 0
  for(j in 1:K){
    training_set <- which(groups != j)
    tree_hitters5 <- tree(Log_salary ~ . - Salary, subset = training_set, Hitters2) 
    prediction <- 
      predict(prune.tree(tree_hitters5, best = k + 1, method = "deviance"),
              newdata = Hitters2[-training_set, ])
    error <- error + mean((prediction - Hitters2[-training_set, "Log_salary"]) ^ 2)   
  }
  cv_error[k] <- error/K       
}
which.min(cv_error)
tree_hitters6 <- prune.tree(tree(Log_salary ~ . - Salary, Hitters2), 
                            best = which.min(cv_error) + 1, method = "deviance")
summary(tree_hitters6)
plot(tree_hitters6)
text(tree_hitters6, pretty = 0)
#to compare the errors
error_type <- c(rep("Training", 9), rep("Test", 9), 
                rep("Cross Validation", 9))
tree_size <- rep(2:10, 3)
errors <- c(training_error, test_error, cv_error)
Error <- data.frame(error_type, tree_size, errors)
library(ggplot2)
ggplot(Error, aes(x = tree_size, y = errors, colour = error_type)) + geom_line()
################################################################################
#Classification tree############################################################
################################################################################
Heart <- read.csv(file = 
                    "L:\\courses\\Introduction_to_Statistical_Learning_with_R\\MLchapter8\\1_data\\Heart.csv", 
                  sep = ",", header = TRUE, encoding = "UTF-8")
names(Heart)
dim(Heart)
###to deal with missing data####################################################
sum(is.na(Heart))
#to compute the number of missing data
Heart2<-na.omit(Heart)
dim(Heart2)
tree_heart2 <- tree(AHD ~ . - X, Heart2)
summary(tree_heart2)
plot(tree_heart2)
text(tree_heart2, pretty = 0)
################################################################################
#to compute the classification errors###########################################
################################################################################
#to compute the training error##################################################
training_error <- 0
for(k in 1:16){
  prediction <- predict(prune.tree(tree_heart2, best = k + 1, method = "misclass"))
  prediction1 <- (as.numeric(prediction[ ,1] < prediction[ ,2]) + 1)
  training_error[k] <- mean(prediction1 != as.numeric(Heart2[, "AHD"]))  
}
training_error
#to compute the test error######################################################
test_error <- 0
set.seed(1000)
training_set <- sample(x = dim(Heart2)[1], size = floor(dim(Heart2)[1] / 2))
tree_heart3 <- tree(AHD ~ . - X, subset = training_set, Heart2)
for(k in 1:16){
  prediction <- predict(prune.tree(tree_heart3, best = k + 1, method = "misclass"),
                        newdata = Heart2[-training_set, ])
  prediction1 <- (as.numeric(prediction[ ,1] < prediction[ ,2]) + 1)
  test_error[k] <- mean(prediction1 != as.numeric(Heart2[-training_set, "AHD"]))  
}
test_error
#to compute the cross validation error with K fold##############################
cv_error <- 0
set.seed(1000)
K <- 10
groups <- sample(x = K, size = dim(Heart2)[1], replace = TRUE)
for(k in 1:16){
  error <- 0
  for(j in 1:K){
    training_set <- which(groups != j)
    tree_heart4 <- tree(AHD ~ . - X, subset = training_set, Heart2) 
    prediction <- 
      predict(prune.tree(tree_heart4, best = k + 1, method = "misclass"),
              newdata = Heart2[-training_set, ])
    prediction1 <- (as.numeric(prediction[ ,1] < prediction[ ,2]) + 1)
    error <- error + 
      sum(prediction1 != as.numeric(Heart2[-training_set, "AHD"]))     
  }
  cv_error[k] <- error/dim(Heart2)[1]       
}
which.min(cv_error)
tree_heart5 <- prune.tree(tree(AHD ~ . - X, Heart2), 
                          best = which.min(cv_error) + 1, method = "misclass")
summary(tree_heart5)
plot(tree_heart5)
text(tree_heart5, pretty = 0)
#to compare the errors
error_type <- c(rep("Training", 16), rep("Test", 16), 
                rep("Cross Validation", 16))
tree_size <- rep(2:17, 3)
errors <- c(training_error, test_error, cv_error)
Error <- data.frame(error_type, tree_size, errors)
library(ggplot2)
ggplot(Error, aes(x = tree_size, y = errors, colour = error_type)) + geom_line()
################################################################################
#Decision Trees#################################################################
################################################################################
library(tree)
library(ISLR)
data(Carseats)
Carseats$High <- as.factor(Carseats$Sales > 8)
Carseats$High <- factor(Carseats$High, labels = c("No", "Yes"))
head(Carseats)
dim(Carseats)
summary(Carseats)
str(Carseats)
#to build classification trees##################################################
tree_carseats <- tree(High ~ . - Sales, Carseats)
summary(tree_carseats)
plot(tree_carseats)
text(tree_carseats, pretty = 0)
tree_carseats
#to compute the test error######################################################
set.seed(2)
train <- sample(1:nrow(Carseats), 200)
Carseats_test <- Carseats[-train, ]
High_test <- Carseats$High[-train]
#to build the model with the training set#######################################
tree_carseats2 <- tree(High ~ . - Sales, Carseats, subset = train)
#to make predictions on the test set
tree_pred <- predict(tree_carseats2, Carseats_test, type = "class")
table(tree_pred, High_test)
prop.table(table(tree_pred, High_test))
sum(diag(prop.table(table(tree_pred, High_test))))
#to get the best tree with the ten-fold cross validation######################## 
set.seed(3)
cv_carseats <- cv.tree(tree_carseats2, FUN = prune.misclass)
cv_carseats
par(mfrow =c(1 ,2))
plot(cv_carseats$size, cv_carseats$dev, type = "b")
plot(cv_carseats$k, cv_carseats$dev, type = "b")
par(mfrow = c(1, 1))
#the best size is 9 and the best lambda is 1.75
prune_carseats <- prune.misclass(tree_carseats2, best = 9)
plot(prune_carseats)
text(prune_carseats, pretty = 0)
tree_pred2 <- predict(prune_carseats, Carseats_test, type = "class")
sum(diag(prop.table(table(tree_pred2, High_test))))
#to build the tree with size = 15, and to predict
prune_carseats <- prune.misclass(tree_carseats2, best = 15)
# the tree with 17 nodes, because trees with 15 is not available.
plot(prune_carseats)
text(prune_carseats, pretty = 0)
tree_pred3 <- predict(prune_carseats, Carseats_test, type = "class")
sum(diag(prop.table(table(tree_pred3, High_test))))
################################################################################
#regression trees###############################################################
################################################################################
library(MASS)
set.seed(1)
#to build regression trees
train <- sample(1:nrow(Boston), size = floor(nrow(Boston)/2))
tree_boston <- tree(medv ~ ., data = Boston, subset = train)
summary(tree_boston)
plot(tree_boston)
text(tree_boston, pretty = 0)
#ten-fold cross validation######################################################
cv_boston <- cv.tree(tree_boston)
plot(cv_boston$size, cv_boston$dev, type = "b")
which.min(cv_boston$dev)
#size = 8 is the best tree
#to build a tree with size at 5
prune_boston <- prune.tree(tree_boston, best = 5)
plot(prune_boston)
text(prune_boston, pretty = 0)
#to compute the test errors with size = 8 and size = 5
#size = 8
yhat <- predict(tree_boston, newdata = Boston[-train, ])
boston_test <- Boston[-train, "medv"]
plot(yhat, boston_test)
abline(a = 0, b = 1)
mean((yhat - boston_test) ^ 2)
#size = 5
yhat2 <- predict(prune_boston, newdata = Boston[-train, ])
boston_test2 <- Boston[-train, "medv"]
plot(yhat2, boston_test2)
abline(a = 0, b = 1)
mean((yhat2 - boston_test2) ^ 2)
################################################################################
#bagging on trees###############################################################
################################################################################
library(randomForest)
set.seed(1)
bag_boston <- randomForest(medv ~ ., data = Boston, subset = train, 
                           mtry = ncol(Boston) - 1, importance = TRUE)
bag_boston
yhat_bag <- predict(bag_boston, newdata = Boston[-train, ])
plot(yhat_bag, boston_test)
abline(0, 1)
mean((yhat_bag - boston_test) ^ 2)
barplot(sort((bag_boston)$importance[, 1]))
#the importance of different variables
################################################################################
#random forest##################################################################
################################################################################
set.seed(1)
rf_boston <- randomForest(medv ~ ., data = Boston, 
                          subset = train, mtry = 5, importance = TRUE)
yhat_rf <- predict(rf_boston, newdata = Boston[-train, ])
mean((yhat_rf - boston_test) ^ 2)
barplot(sort((rf_boston)$importance[, 1]))
#the importance of different variables
################################################################################
#boosting on trees##############################################################
################################################################################
library(gbm)
set.seed(1)
boost_boston <- gbm(medv ~ ., data = Boston[train, ], 
                    distribution = "gaussian", n.trees = 5000,
                    interaction.depth = 4, shrinkage = 0.001)
#ntrees is the number of trees
#the default of shrinkage is 0.001
#the interaction depth controls the depth of each tree
summary(boost_boston)
#to produce a relative influence plot and 
#to output the relative influence statistics
par(mfrow = c(1, 2))
#to plot the relationship between median values and other variables
plot(boost_boston, i = "rm")
plot(boost_boston, i = "lstat")
par(mfrow = c(1, 1))
yhat_boost <- predict(boost_boston, newdata = Boston[-train, ], n.trees = 5000)
mean((yhat_boost - boston_test) ^ 2)
#to try different parameters
boost_boston2 <- gbm(medv ~ ., data = Boston[train, ], 
                     distribution = "gaussian", n.trees = 5000, 
                     interaction.depth = 4, shrinkage = 0.2,
                     verbose = FALSE)
yhat_boost2 <- predict(boost_boston2, newdata = Boston[-train, ], n.trees = 5000)
mean((yhat_boost2 - boston_test) ^ 2)

