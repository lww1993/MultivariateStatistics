################################################################################
#��9.1��ˮ����ҵ�����й�˾��14�ң������侭Ӫҵ����������ģ�ͷ�����##############
#x1:��Ӫҵ��������                   x2:����ë����##############################
#x3:�ٶ�����                         x4:�ʲ���ծ��##############################
#x5:��Ӫҵ������������               x6:Ӫҵ����������##########################
#����x1,x2��������������x3,x4������ծ������x5,x6������չ������##################
################################################################################
Sys.setlocale(, "CHS")
#���ù���Ŀ¼
setwd("L:\\courses\\MVAcourseware2018\\main\\Chapter9_Exploratory_Factor_Analysis\\version2017")
#�����ݣ�����������
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
#����δ��ת,������Ȼ��,����ȷ��ѡȡ�����Ӹ���###################################
################################################################################
factanal(x = management2, factors = 1, rotation = "none")
factanal(x = management2, factors = 2, rotation = "none")
factanal(x = management2, factors = 3, rotation = "none")
#SSloadingsָ����ÿ�����ӵ�ϵ��ƽ���ͣ�
#Uniquenessesָ�����������ӵķ��
#loadingsָ����ϵ����
#����ѡȡ���Ӹ���Ϊ3
################################################################################
#�������ǱȽϷ�������������ת��δ��תʱ��ϵ��#################################
################################################################################
factanal(x = management2, factors = 3, rotation = "none")$loadings
#                   Factor1 Factor2 Factor3
#��Ӫҵ��������      0.950  -0.307         
#����ë����          0.948  -0.310         
#�ٶ�����           -0.340  -0.782   0.517 
#�ʲ���ծ��          0.363   0.561  -0.531 
#��Ӫҵ������������  0.454   0.693   0.556 
#Ӫҵ����������      0.383   0.163   0.527 
factanal(x = management2, factors = 3, rotation = "varimax")$loadings
#                   Factor1 Factor2 Factor3
#��Ӫҵ��������      0.983           0.155 
#����ë����          0.985           0.142 
#�ٶ�����                   -0.990  -0.124 
#�ʲ���ծ��          0.127   0.844         
#��Ӫҵ������������          0.293   0.953 
#Ӫҵ����������      0.210           0.631 
management.fa <- factanal(x = management2, factors = 3, rotation = "varimax")
################################################################################
#�������ӵ÷�###################################################################
################################################################################
#��Ȩ��С���˷��������ӵ÷�
scores1 <- factanal(x = management2, factors = 3, rotation = "varimax", 
                    scores = "Bartlett")$scores 
#�ع鷨�������ӵ÷�
scores2 <- factanal(x = management2, factors = 3, rotation = "varimax",
                    scores = "regression")$scores
#��ͼ
plot(scores1[, 1], scores1[, 2])
text(scores1[, 1], scores1[, 2], labels = rownames(management2), 
     cex = 0.8, pos = 2, col = "red")
lines(x = rep(x = 0, times = 31), y = seq(from = -3,to = 3,by = 0.2), lty = 2)
lines(x = seq(from = -8, to = 4, by = 0.2), y = rep(0, times = 61), lty = 2)
#ǰ����������Ϣ�ص�ͼ
biplot(scores1, management.fa$loadings)
################################################################################
################################################################################
################################################################################
library(psych)
fa(r = cor(management2), nfactors = 3, 
   n.obj = dim(management2)[1], rotate = "varimax", 
   covar = FALSE, fm = "pa")
#                   PA1   PA2   PA3   h2      u2 com
#��Ӫҵ��������     0.98  0.07  0.16 0.99  0.0068 1.1
#����ë����         0.99  0.08  0.14 1.01 -0.0053 1.1
#�ٶ�����           0.00 -1.03 -0.12 1.08 -0.0820 1.0
#�ʲ���ծ��         0.13  0.81  0.03 0.67  0.3328 1.1
#��Ӫҵ������������ 0.05  0.29  0.84 0.79  0.2116 1.2
#Ӫҵ����������     0.20 -0.10  0.72 0.56  0.4401 1.2
fa(r = cor(management2), nfactors = 3, 
   n.obj = dim(management2)[1], rotate = "none", 
   covar = FALSE, fm = "pa")
#                     PA1   PA2   PA3   h2      u2 com
#��Ӫҵ��������      0.82  0.51 -0.23 0.99  0.0068 1.9
#����ë����          0.83  0.51 -0.25 1.01 -0.0053 1.9
#�ٶ�����           -0.60  0.84  0.16 1.08 -0.0820 1.9
#�ʲ���ծ��          0.53 -0.58 -0.23 0.67  0.3328 2.3
#��Ӫҵ������������  0.56 -0.21  0.66 0.79  0.2116 2.2
#Ӫҵ����������      0.41  0.19  0.60 0.56  0.4401 2.0
##ע�⣬��ʱ��������ν��Heywood case:����һ���������ӵķ���Ϊ0.
#####################################################################################################
#####################################################################################################
#####################################################################################################
#�����ӽⷨ�����ӷ���,��Ȩ��С���˷��������ӵ÷�
fa1 <- fa(r = cor(management2), nfactors = 3, 
          n.obj = dim(management2)[1], rotate = "varimax", 
          covar = FALSE, fm = "pa", scores = "Bartlett")
scores1_pa <- factor.scores(x = management2, f = fa1, method = "Bartlett")
biplot(scores1_pa$scores, fa1$loadings)
#�����ӽⷨ�����ӷ���,�ع鷨�������ӵ÷�
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