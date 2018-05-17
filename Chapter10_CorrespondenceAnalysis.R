Sys.setlocale(, "CHS")
setwd("L:\\courses\\MVAcourseware2018\\main\\Chapter10_Correspondence_Analysis")
################################################################################
#例10.1,研究收入与满意度之间的关系##############################################
################################################################################
#读数据
library(xlsx)
income_satisfaction <- read.xlsx(file = "chapter10.xlsx", sheetName = "Example1",
                                 header = TRUE, encoding = "UTF-8")
income_satisfaction2 <- income_satisfaction[, -1]
rownames(income_satisfaction2) <- income_satisfaction[, 1]
income_satisfaction <- income_satisfaction2
dim(income_satisfaction)
#卡方检验
chisq.test(income_satisfaction)
#p value小于2e-16从而 说明这两者是密切相关的。
#对应分析
library(MASS)
income_satisfaction_ca <- corresp(x = income_satisfaction, nf = 2)
income_satisfaction_ca
biplot(income_satisfaction_ca)
abline(v = 0, h = 0, lty = 3)
##v(vertical)垂直直线上的x值
##h(horizontal)水平直线上的y值
##lty线的类型
#1.满意度与收入有关，从图中可以看出是成方向变化的；
#2.幸福指数排名上，可以看出，基本上排名比较靠前的收入比较高。
#(联合国幸福指数排名2014,2015等)
################################################################################
#例10.2,研究不同省份与其经济构成的关系##########################################
#读数据#########################################################################
Sys.setlocale(, "CHS")
setwd("L:\\courses\\MVAcourseware2018\\main\\Chapter10_Correspondence_Analysis")
library(xlsx)
province_economy <- read.xlsx(file = "chapter10.xlsx", sheetName = "Example2",
                              header = TRUE, encoding = "UTF-8")
province_economy2 <- province_economy[, -1]
rownames(province_economy2) <- province_economy[, 1]
province_economy <- province_economy2
dim(province_economy)
head(province_economy)
#卡方检验
chisq.test(province_economy)
#对应分析
library(MASS)
province_economy2 <- corresp(x = province_economy, nf = 2)
province_economy2
biplot(province_economy2)
abline(v = 0, h = 0, lty = 3)
##v(vertical)垂直直线上的x值
##h(horizontal)水平直线上的y值
##lty线的类型

#从上图可以看出：
#1.港澳台经济：广东、福建；
#2.联营经济与外资：北京、上海、天津、海南；
#3.集体经济：江苏、浙江；
#4.股份经济与集体经济：山东、安徽；
#5.国有经济：余下省份。
################################################################################
#第十章案例#####################################################################
################################################################################
Sys.setlocale(, "CHS")
setwd("L:\\courses\\MVAcourseware2018\\main\\Chapter10_Correspondence_Analysis")
#读数据
library(xlsx)
internet_visit <- read.xlsx(file = "chapter10.xlsx", sheetName = "Case",
                          header = TRUE, encoding = "UTF-8")
internet_visit2 <- internet_visit[, -1]
rownames(internet_visit2) <- internet_visit[, 1]
internet_visit <- internet_visit2
dim(internet_visit)                 
head(internet_visit)
#卡方检验
chisq.test(internet_visit)
#对应分析
library(MASS)
internet_visit_ca <- corresp(x = internet_visit, nf = 2)
internet_visit_ca
biplot(internet_visit_ca)
abline(v = 0, h = 0, lty = 3)
##v(vertical)垂直直线上的x值
##h(horizontal)水平直线上的y值
##lty线的类型
#访问量：网易、搜狐、新浪、Tom、21cn;
#速度：中国新闻网
#浏览页面数：千龙新闻网
#流量：凤凰网、电脑之家等。
###############################################################################################
#这个问题研究1973年到1978年之间，美国授予的哲学博士学位数目####################################
###############################################################################################
Sys.setlocale(,"CHS")
setwd("L:\\courses\\MVAcourseware2018\\main\\Chapter10_Correspondence_Analysis")
library(xlsx)
doctor_numbers <- read.xlsx(file = "chapter10.xlsx",sheetName = "exercise3",
                            header = TRUE, encoding = "UTF-8")
doctor_numbers2 <- doctor_numbers[, -1]
rownames(doctor_numbers2) <- doctor_numbers[, 1]
doctor_numbers <- doctor_numbers2
dim(doctor_numbers)
#卡方检验
chisq.test(doctor_numbers)
#对应分析
library(MASS)
doctor_numbers_ca<-corresp(x = doctor_numbers, nf = 2)
doctor_numbers_ca
biplot(doctor_numbers_ca)
abline(v = 0, h = 0, lty = 3)
##v(vertical)垂直直线上的x值
##h(horizontal)水平直线上的y值
##lty线的类型                