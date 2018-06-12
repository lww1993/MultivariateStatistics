###################################################################################
#Microarray¡¡Classification########################################################
###################################################################################
setwd("L:\\courses\\DataMiningCases\\Classifying_Microarray_Samples")
load(file = "data\\ALL.RData")
#the dimension of ALL data is 128*12627, 
#the last two columns are about the biological information
dim(ALL_data)
names(ALL_data)
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
#the col names have to begin with letters, not the digits
ALLb_data3$mol_biol <- factor(ALLb_data3$mol_biol)
#the classification variables have to be factors 
model_RF <- randomForest(mol_biol ~ . - BT, ALLb_data3, importance = TRUE)
imp <- importance(model_RF)
rf_genes <- rownames(imp)[order(imp[,"MeanDecreaseAccuracy"],
decreasing = TRUE)[1:30]]
#dt <- data.frame(t(es), Mut = ALLb$mol.bio)
#dt$Mut <- droplevels(dt$Mut)
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

