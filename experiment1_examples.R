library(MASS)
library(ISLR)
library(leaps)
#设置R语言对中文的支持
Sys.setlocale(,"CHS")
#设置工作目录
setwd("L:\\courses\\Introduction_to_Statistical_Learning_with_R\\MLchapter3")
advertising <- read.csv(file = "1_data\\Advertising.csv", header = TRUE, sep = ",")
advertising <- advertising[, 2:5]
#数据基本信息
dim(advertising)
head(advertising)
summary(advertising)
str(advertising)      
#做线性拟合
sales_fit <- lm(Sales ~ ., data = advertising)
summary(sales_fit)
#穷举法
library(leaps)
sales_fit_full <- regsubsets(Sales ~ ., data = advertising, method = "exhaustive")
result1 <- summary(sales_fit_full)
data.frame(result1$outmat,result1$rss,result1$rsq,result1$adjr2,result1$cp,result1$bic)
#向前选择变量法
sales_fit_forward <- regsubsets(Sales ~ ., data = advertising, method = "forward")
result2 <- summary(sales_fit_forward)
data.frame(result2$outmat,result2$rss,result2$rsq,result2$adjr2,result2$cp,result2$bic)
#向后选择变量法
sales_fit_backward <- regsubsets(Sales ~ ., data = advertising, method = "backward")
result3 <- summary(sales_fit_backward)
data.frame(result3$outmat,result3$rss,result3$rsq,result3$adjr2,result3$cp,result3$bic)
#做线性拟合
sales_fit <- lm(Sales ~ TV + Radio, data = advertising)
summary(sales_fit)












