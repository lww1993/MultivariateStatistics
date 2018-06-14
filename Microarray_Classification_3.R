###################################################################################
#Microarray¡¡Classification########################################################
###################################################################################
setwd("L:\\courses\\DataMiningCases\\Classifying_Microarray_Samples")
load(file = "data\\ALL.RData")
#the dimension of ALL data is 128*12627, 
#the last two columns are about the biological information
dim(ALL_data)
names(ALL_data)[1:10]
table(ALL_data$BT)
table(ALL_data$mol_biol)
table(ALL_data$BT, ALL_data$mol_biol)
# To get B-cell ALL cases and samples with a subset of the mutations
index1 <- which(ALL_data$BT %in% (labels(table(ALL_data$BT))[[1]])[1:5] &
                ALL_data$mol_biol %in% (labels(table(ALL_data$mol_biol))[[1]])[1:4])
ALLb_data <- ALL_data[index1, ]
dim(ALLb_data)
#the dimension of B-cell data is 94*12627
#################################################################################
#exploring the dataset###########################################################
#################################################################################
expression_data <- as.matrix(ALLb_data[, 1:12625])
summary(as.vector(expression_data))
library(ggplot2)
#library(RSQLite)
exprVs <- data.frame(exprVal = as.vector(expression_data),
                     mutation = factor(rep(ALLb_data$mol_biol, 
                                       times = dim(ALLb_data)[2] - 2)))
ds <- data.frame(Stat = c("1stQ", "Median", "3rdQ"),
                 Value = quantile(exprVs$exprVal, probs = c(0.25, 0.5, 0.75)),
                 Color = c("red", "green", "red"))
ggplot(exprVs, aes(x = exprVal)) + 
       geom_histogram(binwidth = 1/2, fill = "lightgrey") +
       geom_vline(data = ds, aes(xintercept = Value, color = Color)) +
       geom_text(data = ds, aes(x = Value-0.3, y = 0, label = Stat, colour = Color),
       angle = 90, hjust = "left") +
       xlab("Expression Levels") + 
       guides(colour = "none", fill = "none")
sapply(labels(table(ALLb_data$mol_biol))[[1]],
       function(x) 
       summary(as.vector(expression_data[which(ALLb_data$mol_biol == x), ])))
ggplot(exprVs, aes(x = exprVal, colour = mutation)) + geom_density()
#Things are rather similar across these subsets of samples,
#and, what's more, they are similar to the global distribution of expression levels
#####As an exercise you may try to produce a graph that 
#####shows several plots similar to that shown in Figure 7.1, 
#####one for each mutation, using ggplot facets. 
################################################################################
#Feature Selection##############################################################
################################################################################
#to calculate the inter-quartile range(IQR) of each column of the matrix 
library(ggplot2)
#the medians and and IQR of each column
dg <- data.frame(col_median = apply(expression_data, 2, median), 
                 col_IQR = apply(expression_data, 2, IQR))
#to plot between medians and the IQR
ggplot(dg, aes(x = col_median, y = col_IQR)) + geom_point() +
       xlab("Median expression level") + ylab("IQR expression level") +
       ggtitle("Main Characteristics of Genes Expression Levels")
#Namely, we will remove any genes with a variability 
#that is smaller than 1/5 of the global IQR. 
# We also remove the genes which names contain "AFFX"
index2 <- union(grep("^AFFX", colnames(ALLb_data)),
                which(apply(expression_data, 2, IQR) < 
                IQR(as.vector(expression_data)) / 5))
ALLb_data2 <- ALLb_data[, -index2]
expression_data2 <- expression_data[, -index2]
dim(expression_data2)
dim(ALLb_data2)
#we are left with only 3,736 genes from the initial 12,625
################################################################################
#ANOVA Filters##################################################################
################################################################################
#to keep the genes with anova(p value < 0.01) 
p_value <- NA
for(k in 1:(dim(ALLb_data2)[2]-2)){
      p_value[k] <- 
      ((summary(aov(ALLb_data2[, k] ~ factor(ALLb_data2$mol_bio))))[[1]])$Pr[1]
}
index3 <- which(p_value >= 0.01)
ALLb_data3 <- ALLb_data2[, -index3]
dim(ALLb_data3)
expression_data3 <- expression_data2[, -index3]
dg <- data.frame(col_median = apply(expression_data3, 2, median), 
                 col_IQR = apply(expression_data3, 2, IQR))
ggplot(dg, aes(x = col_median, y = col_IQR)) + geom_point() +
xlab("Median expression level") + ylab("IQR expression level") +
ggtitle("Distribution Properties of the Selected Genes")
################################################################################
#Filtering Using Random Forests#################################################
################################################################################
library(randomForest)
set.seed(1234)
colnames(ALLb_data3) <- c(paste("X", colnames(ALLb_data3)[1:783], sep = ""), 
                         colnames(ALLb_data3)[784:785])
colnames(expression_data3) <- paste("X", colnames(expression_data3)[1:783], sep = "")
#the col names have to begin with letters, not the digits
ALLb_data3$mol_biol <- factor(ALLb_data3$mol_biol)
#the classification variables have to be factors 
model_RF <- randomForest(mol_biol ~ . - BT, ALLb_data3, importance = TRUE)
imp <- importance(model_RF)
rf_genes <- rownames(imp)[order(imp[,"MeanDecreaseAccuracy"],
decreasing = TRUE)[1:30]]
sapply(rf_genes, 
       function(gene) tapply(ALLb_data3[, gene], ALLb_data3$mol_biol, median))

library(tidyr)
library(dplyr)
d <- gather(ALLb_data3[, c(rf_genes, "mol_biol")], 
            Gene, ExprValue, 1:length(rf_genes))
dat <- group_by(d, mol_biol, Gene) %>%
summarise(med=median(ExprValue), iqr=IQR(ExprValue))
ggplot(dat, aes(x = med,y = iqr,color = mol_biol)) +
geom_point(size=6) + facet_wrap( ~ Gene) +
labs(x="MEDIAN expression level",y="IQR expression level",color="Mutation")
################################################################################
#Filtering Using Ensembling#####################################################
################################################################################
clust1 <- hclust(dist(t(expression_data3)), method = "complete")
table1 <- table(cutree(clust1, k = 30))
clust30 <- cutree(clust1, k = 30)
#to get the variable sets#######################################################
J <- 500
#the number of the varible sets
ensemble_genes <- matrix(NA, nrow = J, ncol = 30)
for(j in 1:J){
      for(k in 1:30){
            index <- sample(1:table1[k], size = 1)
            ensemble_genes[j, k] <- 
            names(clust30[clust30 == k])[index]    
      }
}
################################################################################
#To Model#######################################################################
################################################################################
K <- 10

#random forests#################################################################
#ntree=500,750,1000#############################################################
#mtry = 5, 15###################################################################
library(randomForest)
################################################################################
#data after ANOVA filter
index <- sample(1:K, size = dim(ALLb_data3)[1], replace = TRUE)
error1 <- 0
for(k in 1:K){
      test <- (1:dim(ALLb_data3)[1])[index == k]
      train <- (1:dim(ALLb_data3)[1])[index != k]
      rf_model <- randomForest(mol_biol ~ . - BT, data = ALLb_data3, subset = train, 
                               ntree = 1000, mtry = 15)
      prediction1 <- predict(rf_model, newdata = ALLb_data3[test, ])
      error1 <- error1 + sum(prediction1 != ALLb_data3$mol_biol[test])
}
error1 / dim(ALLb_data3)[1]
################################################################################
#data after random forest filter
ALLb_data3_rf <- ALLb_data3[, c(rf_genes, "BT", "mol_biol")]
library(randomForest)
index <- sample(1:K, size = dim(ALLb_data3_rf)[1], replace = TRUE)
error2 <- 0
for(k in 1:K){
      test <- (1:dim(ALLb_data3_rf)[1])[index == k]
      train <- (1:dim(ALLb_data3_rf)[1])[index != k]
      rf_model <- randomForest(mol_biol ~ . - BT, data = ALLb_data3_rf, subset = train, 
                               ntree = 1000, mtry = 15)
      prediction1 <- predict(rf_model, newdata = ALLb_data3_rf[test, ])
      error2 <- error2 + sum(prediction1 != ALLb_data3_rf$mol_biol[test])
}
error2 / dim(ALLb_data3_rf)[1]
###############################################################################
#data after ensemble filter
library(randomForest)
index <- sample(1:K, size = dim(ALLb_data3)[1], replace = TRUE)
error3 <- 0
for(k in 1:K){
      test <- (1:dim(ALLb_data3)[1])[index == k]
      train <- (1:dim(ALLb_data3)[1])[index != k]
      prediction1 <- matrix(NA, nrow = J, ncol = length(test))
      for(j in 1:J){
            ALLb_data3_ensemble  <- 
            ALLb_data3[, c(ensemble_genes[j, ], "BT", "mol_biol")]
            rf_model <- 
            randomForest(mol_biol ~ . - BT, 
            data = ALLb_data3_ensemble, 
            subset = train, ntree = 1000, mtry = 15)
            prediction1[j, ] <- 
            predict(rf_model, newdata = ALLb_data3_ensemble[test, ])
      }
      max1 <- function(x){
                          return(as.numeric(labels(which.max(table(x)))))
              }
      prediction2 <- apply(prediction1, 2, max1)
      error3 <- error3 + sum(prediction2 != as.numeric(ALLb_data3$mol_biol[test]))
}
error3 / dim(ALLb_data3)[1]
#SVM############################################################################
#cost = 1,100###################################################################
#gamma = 0.01, 0.001, 0.0001####################################################
library(e1071)
#data after ANOVA filter
index <- sample(1:K, size = dim(ALLb_data3)[1], replace = TRUE)
error1 <- 0
for(k in 1:K){
      test <- (1:dim(ALLb_data3)[1])[index == k]
      train <- (1:dim(ALLb_data3)[1])[index != k]
      svm_model <- svm(mol_biol ~ . - BT, data = ALLb_data3, subset = train, 
                               cost = 1, gamma = 0.01)
      prediction1 <- predict(svm_model, newdata = ALLb_data3[test, ])
      error1 <- error1 + sum(prediction1 != ALLb_data3$mol_biol[test])
}
error1 / dim(ALLb_data3)[1]
#################################################################################
#data after random forest filter
ALLb_data3_rf <- ALLb_data3[, c(rf_genes, "BT", "mol_biol")]
library(e1071)
index <- sample(1:K, size = dim(ALLb_data3_rf)[1], replace = TRUE)
error2 <- 0
for(k in 1:K){
      test <- (1:dim(ALLb_data3_rf)[1])[index == k]
      train <- (1:dim(ALLb_data3_rf)[1])[index != k]
      svm_model <- svm(mol_biol ~ . - BT, data = ALLb_data3_rf, subset = train, 
                      cost = 1, gamma = 0.01)
      prediction1 <- predict(svm_model, newdata = ALLb_data3_rf[test, ])
      error2 <- error2 + sum(prediction1 != ALLb_data3_rf$mol_biol[test])
}
error2 / dim(ALLb_data3_rf)[1]
################################################################################
#data after ensemble filter
library(e1071)
index <- sample(1:K, size = dim(ALLb_data3)[1], replace = TRUE)
error3 <- 0
for(k in 1:K){
      test <- (1:dim(ALLb_data3)[1])[index == k]
      train <- (1:dim(ALLb_data3)[1])[index != k]
      prediction1 <- matrix(NA, nrow = J, ncol = length(test))
      for(j in 1:J){
            ALLb_data3_ensemble  <- 
            ALLb_data3[, c(ensemble_genes[j, ], "BT", "mol_biol")]
            svm_model <- 
            svm(mol_biol ~ . - BT, 
            data = ALLb_data3_ensemble, 
            subset = train, cost = 1, gamma = 0.01)
            prediction1[j, ] <- 
            predict(svm_model, newdata = ALLb_data3_ensemble[test, ])
      }
      max1 <- function(x){
                          return(as.numeric(labels(which.max(table(x)))))
              }
      prediction2 <- apply(prediction1, 2, max1)
      error3 <- error3 + sum(prediction2 != as.numeric(ALLb_data3$mol_biol[test]))
}
error3 / dim(ALLb_data3)[1]
#KNN############################################################################
#k = 3,5,7######################################################################
#standard = TRUE, FALSE#########################################################
library(class)
################################################################################
#data after ANOVA filter
index <- sample(1:K, size = dim(ALLb_data3)[1], replace = TRUE)
error1 <- 0
for(k in 1:K){
      test <- (1:dim(ALLb_data3)[1])[index == k]
      train <- (1:dim(ALLb_data3)[1])[index != k]
      test_set <- ALLb_data3[test, -c(dim(ALLb_data3)[2] - 1, dim(ALLb_data3)[2]) ]
      train_set <- ALLb_data3[train,  -c(dim(ALLb_data3)[2] - 1, dim(ALLb_data3)[2])] 
      prediction1 <- knn(train = train_set, test = test_set, 
                         cl = ALLb_data3[train, dim(ALLb_data3)[2]], k = 3)
      error1 <- error1 + sum(prediction1 != ALLb_data3$mol_biol[test])
}
error1 / dim(ALLb_data3)[1]
################################################################################
#data after random forest filter
ALLb_data3_rf <- ALLb_data3[, c(rf_genes, "BT", "mol_biol")]
library(class)
index <- sample(1:K, size = dim(ALLb_data3_rf)[1], replace = TRUE)
error2 <- 0
for(k in 1:K){
      test <- (1:dim(ALLb_data3_rf)[1])[index == k]
      train <- (1:dim(ALLb_data3_rf)[1])[index != k]
      test_set <- ALLb_data3_rf[test, 
                                -c(dim(ALLb_data3_rf)[2] - 1, dim(ALLb_data3_rf)[2]) ]
      train_set <- 
      ALLb_data3_rf[train,  -c(dim(ALLb_data3_rf)[2] - 1, dim(ALLb_data3_rf)[2])] 
      prediction1 <- knn(train = train_set, test = test_set, 
                         cl = ALLb_data3_rf[train, dim(ALLb_data3_rf)[2]], k = 3)
      error2 <- error2 + sum(prediction1 != ALLb_data3_rf$mol_biol[test])
}
error2 / dim(ALLb_data3_rf)[1]
###################################################################################
#data after ensemble filter
library(class)
index <- sample(1:K, size = dim(ALLb_data3)[1], replace = TRUE)
error3 <- 0
for(k in 1:K){
      test <- (1:dim(ALLb_data3)[1])[index == k]
      train <- (1:dim(ALLb_data3)[1])[index != k]
      prediction1 <- matrix(NA, nrow = J, ncol = length(test))
      for(j in 1:J){
            ALLb_data3_ensemble  <- 
            ALLb_data3[, c(ensemble_genes[j, ], "BT", "mol_biol")]
            test_set <- ALLb_data3_ensemble[test, 
            -c(dim(ALLb_data3_ensemble)[2] - 1, dim(ALLb_data3_ensemble)[2]) ]
            train_set <- ALLb_data3_rf[train,  
            -c(dim(ALLb_data3_ensemble)[2] - 1, dim(ALLb_data3_ensemble)[2])] 
            prediction1[j, ] <- knn(train = train_set, test = test_set, 
            cl = ALLb_data3_ensemble[train, dim(ALLb_data3_ensemble)[2]],
                          k = 5)
      }
      max1 <- function(x){
                          return(as.numeric(labels(which.max(table(x)))))
              }
      prediction2 <- apply(prediction1, 2, max1)
      error3 <- error3 + sum(prediction2 != as.numeric(ALLb_data3$mol_biol[test]))
}
error3 / dim(ALLb_data3)[1]