################################################################################
#例9.1：水泥行业的上市公司共14家，根据其经营业绩进行因子模型分析。##############
#x1:主营业务利润率                   x2:销售毛利率##############################
#x3:速动比率                         x4:资产负债率##############################
#x5:主营业务收入增长率               x6:营业利润增长率##########################
#其中x1,x2代表获利能力，x3,x4代表偿债能力，x5,x6代表发展能力。##################
################################################################################
Sys.setlocale(, "CHS")
#设置工作目录
setwd("L:\\courses\\MVAcourseware2018\\main\\Chapter9_Exploratory_Factor_Analysis\\version2017")
#读数据，设置行名。
library(xlsx)
library(psych)
management <- read.xlsx(file = "chapter9.xlsx",
                        sheetName = "Example9.1",
                        header = TRUE, encoding = "UTF-8")
management2 <- management[, -1]
rownames(management2) <- management[, 1]
dim(management2)
head(management2)
fa.parallel(cor(management2), n.obs = 14, 
            fa = "both", n.iter = 100,
            main = "Scree plots with parallel analysis")
################################################################################
#因子未旋转,极大似然法,首先确定选取的因子个数###################################
################################################################################
factanal(x = management2, factors = 1, rotation = "none")
factanal(x = management2, factors = 2, rotation = "none")
factanal(x = management2, factors = 3, rotation = "none")
#SSloadings指的是每个因子的系数平方和；
#Uniquenesses指的是特殊因子的方差；
#loadings指的是系数；
#我们选取因子个数为3
################################################################################
#以下我们比较方差最大的正交旋转与未旋转时的系数#################################
################################################################################
factanal(x = management2, factors = 3, rotation = "none")$loadings
#                   Factor1 Factor2 Factor3
#主营业务利润率      0.950  -0.307         
#销售毛利率          0.948  -0.310         
#速动比率           -0.340  -0.782   0.517 
#资产负债率          0.363   0.561  -0.531 
#主营业务收入增长率  0.454   0.693   0.556 
#营业利润增长率      0.383   0.163   0.527 
factanal(x = management2, factors = 3, rotation = "varimax")$loadings
#                   Factor1 Factor2 Factor3
#主营业务利润率      0.983           0.155 
#销售毛利率          0.985           0.142 
#速动比率                   -0.990  -0.124 
#资产负债率          0.127   0.844         
#主营业务收入增长率          0.293   0.953 
#营业利润增长率      0.210           0.631 
management.fa <- factanal(x = management2, factors = 3, rotation = "varimax")
################################################################################
#计算因子得分###################################################################
################################################################################
#加权最小二乘法计算因子得分
scores1 <- factanal(x = management2, factors = 3, rotation = "varimax", 
                    scores = "Bartlett")$scores 
#回归法计算因子得分
scores2 <- factanal(x = management2, factors = 3, rotation = "varimax",
                    scores = "regression")$scores
#作图
plot(scores1[, 1], scores1[, 2])
text(scores1[, 1], scores1[, 2], labels = rownames(management2), 
     cex = 0.8, pos = 2, col = "red")
lines(x = rep(x = 0, times = 31), y = seq(from = -3,to = 3,by = 0.2), lty = 2)
lines(x = seq(from = -8, to = 4, by = 0.2), y = rep(0, times = 61), lty = 2)
#前两个因子信息重叠图
biplot(scores1, management.fa$loadings)
################################################################################
################################################################################
################################################################################
library(psych)
fa(r = cor(management2), nfactors = 3, 
   n.obj = dim(management2)[1], rotate = "varimax", 
   covar = FALSE, fm = "pa")
#                   PA1   PA2   PA3   h2      u2 com
#主营业务利润率     0.98  0.07  0.16 0.99  0.0068 1.1
#销售毛利率         0.99  0.08  0.14 1.01 -0.0053 1.1
#速动比率           0.00 -1.03 -0.12 1.08 -0.0820 1.0
#资产负债率         0.13  0.81  0.03 0.67  0.3328 1.1
#主营业务收入增长率 0.05  0.29  0.84 0.79  0.2116 1.2
#营业利润增长率     0.20 -0.10  0.72 0.56  0.4401 1.2
fa(r = cor(management2), nfactors = 3, 
   n.obj = dim(management2)[1], rotate = "none", 
   covar = FALSE, fm = "pa")
#                     PA1   PA2   PA3   h2      u2 com
#主营业务利润率      0.82  0.51 -0.23 0.99  0.0068 1.9
#销售毛利率          0.83  0.51 -0.25 1.01 -0.0053 1.9
#速动比率           -0.60  0.84  0.16 1.08 -0.0820 1.9
#资产负债率          0.53 -0.58 -0.23 0.67  0.3328 2.3
#主营业务收入增长率  0.56 -0.21  0.66 0.79  0.2116 2.2
#营业利润增长率      0.41  0.19  0.60 0.56  0.4401 2.0
##注意，此时出现了所谓的Heywood case:至少一个特殊因子的方差为0.
#####################################################################################################
#####################################################################################################
#####################################################################################################
#主因子解法作因子分析,加权最小二乘法计算因子得分
fa1 <- fa(r = cor(management2), nfactors = 3, 
          n.obj = dim(management2)[1], rotate = "varimax", 
          covar = FALSE, fm = "pa", scores = "Bartlett")
scores1_pa <- factor.scores(x = management2, f = fa1, method = "Bartlett")
biplot(scores1_pa$scores, fa1$loadings)
#主因子解法作因子分析,回归法计算因子得分
fa2 <- fa(r = cor(management2), nfactors = 3, 
          n.obj = dim(management2)[1], rotate = "varimax", 
          covar = FALSE, fm = "pa", scores = "Thurstone")
scores2_pa <- factor.scores(x = management2,f = fa2, method = "Thurstone")
biplot(scores2_pa$scores, fa2$loadings)
################################################################################
#factor analysis################################################################
################################################################################
#to get the correlation matrix
library(psych)
covariances <- ability.cov$cov
correlations <- cov2cor(covariances)
#to decide the number of the factors
fa.parallel(correlations, n.obs = 112, 
            fa = "both", n.iter = 100,
            main = "Scree plots with parallel analysis")
#we select 2 factors
fa <- fa(correlations, nfactors = 2, rotate = "none", fm = "pa")
fa
#to do a factor rotation
fa.varimax <- fa(correlations, nfactors = 2, rotate = "varimax", fm = "pa")
fa.varimax
#to plot each variable
factor.plot(fa.varimax, labels = rownames(fa.varimax$loadings))
fa.varimax$weights