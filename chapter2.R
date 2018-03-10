#设置R语言对中文的支持
Sys.setlocale(, "CHS")

#设置工作目录
setwd("L:/MVAcourseware2018/main/Chapter2_DataStructure_and_Graphics")

#读文件
library(xlsx)
case2<-read.xlsx(file="1_data/sample1.xlsx",sheetName="Case",header=TRUE,encoding="UTF-8")

#查看case2的形式
dim(case2)        #数据维度；
head(case2)       #数据的前六行；
tail(case2)       #数据的后六行；
summary(case2)    #数据的最大值、最小值、中位数等；
str(case2)        #数据的类型等；

#查看调查对象所在地区的分布
table(case2$地区)    
barplot(table(case2$地区)) #看一下地区的分布

#查看性别、教育程度、观点之间的关系
ftable(case2$教育程度,case2$性别,case2$观点)
barplot(ftable(case2$教育程度,case2$性别,case2$观点),beside=TRUE,col=3:4)

#查看月收入与性别的关系
hist(case2$月收入)                      #看一下收入的直方图
boxplot(case2$月收入~case2$性别)        #根据性别，分别做相应的箱线图
t.test(case2$月收入~case2$性别)         #使用T检验,检验月收入与性别是否相关
#此时用的是Welch's Test,假设是数据服从正态分布，但是方差不相等。比传统的Student's t检验应用更广。

#作图观察性别与所持观点之间，是否有关
table(case2$性别,case2$观点)
barplot(table(case2$性别,case2$观点), beside=TRUE, col=3:4,legend.text=TRUE,
args.legend = list(x = "topleft"))



  






 