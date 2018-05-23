#x1:体重；x2:腰围；x3:脉搏
#y1:引体向上次数；y2:仰卧起坐次数；y3:跳跃次数。
Sys.setlocale(,"CHS")
setwd("L:\\courses\\MVAcourseware2018\\main\\chapter11_Canonical_Correlation_Analysis")
library(xlsx)
body.exercise <- read.xlsx(file = "chapter11.xlsx", sheetName = "example1",
                 header = TRUE, encoding = "UTF-8")
dim(body.exercise)
head(body.exercise)
#做典型相关分析
cancor(x = body.exercise[, 1:3], y = body.exercise[, 4:6], 
       xcenter = FALSE, ycenter = FALSE)
#x1:原煤消费量；x2:油品消费量；x3:电力消费量；x4:进口能源量
#y1:农业生产总值；y2:工业生产总值；y3:建筑业生产总值；y4:第三产业生产总值，y5:全省户籍人口；
#y6:人均可支配收入
library(xlsx)
expenditure.economy <- read.xlsx(file = "chapter11.xlsx", sheetName = "example2",
                                 header = TRUE, encoding = "UTF-8")
dim(expenditure.economy)
head(expenditure.economy)
#做典型相关分析
cancor(x = expenditure.economy[, 1:4], y = expenditure.economy[, 5:10],
       xcenter = FALSE, ycenter = FALSE)
#x1:工资性收入；x2:家庭经营收入；x3:财产性收入；x4:转移性收入
#y1:生活消费；y2:家庭经营支出；y3:购置生产性固定资产的支出；y4:财产性支出；y5:转移性支出。
library(xlsx)
income.expenditure<-read.xlsx(file="chapter11.xlsx",sheetName="case",header=TRUE,encoding="UTF-8")
dim(income.expenditure)
head(income.expenditure)
#做典型相关分析
cancor(x=income.expenditure[,1:4],y=income.expenditure[,5:9],xcenter=FALSE,ycenter=FALSE)

library(CCP)
rho <- cancor(x=income.expenditure[,1:4],y=income.expenditure[,5:9],xcenter=FALSE,ycenter=FALSE)$cor

## Define number of observations, number of dependent variables, number of independent variables.
N = dim(income.expenditure)[1]       
p = dim(income.expenditure[,1:4])[2]   
q = dim(income.expenditure[,5:9])[2]

## Calculate p-values using the F-approximations of different test statistics:
p.asym(rho, N, p, q, tstat = "Wilks")
p.asym(rho, N, p, q, tstat = "Hotelling")
p.asym(rho, N, p, q, tstat = "Pillai")
p.asym(rho, N, p, q, tstat = "Roy")