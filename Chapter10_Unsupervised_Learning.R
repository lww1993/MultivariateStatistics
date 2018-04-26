##################################################################
#K-Means Clustering###############################################
##################################################################
set.seed(2)
#to get the simulated data
x <- matrix(rnorm(50 * 2), ncol = 2)
x[1:25, 1] <- x[1:25, 1] + 3
x[1:25, 2] <- x[1:25, 2] - 4
group <- c(rep("1", 25), rep("2", 25))
#to do k-means clustering with k=2
km.out <- kmeans(x = x, center = 2, nstart = 20)
km.out$cluster
plot(x, pch = group, col = (km.out$cluster + 1),
     main = "K-Means Clustering Results with K=2",
     xlab = "", ylab = "", cex = 2)
#to do k-means clustering with k=3
set.seed(4)
km.out <- kmeans(x = x, center = 3, nstart = 20)
km.out
#to compare the total sum of within-cluster variations
set.seed(3)
km.out <- kmeans(x = x, center = 3, nstart = 1)
km.out$tot.withinss
km.out2 <- kmeans(x = x, center = 3, nstart = 20)
km.out2$tot.withinss
####################################################################
#Hierarchical Clustering############################################
####################################################################
#The Interpretation of Dendrogram
set.seed(1)
y <- matrix(data = rnorm(18), ncol = 2)
par(mfrow = c(1, 2))
plot(y, pch = as.character(1:9), xlab = "", ylab = "")
plot(hclust(dist(y), method = "complete"), 
      main = "Complete Linkage", xlab = "", sub = "", cex = .9)
#####################################################################
##The incursion
x1 <- c(0, 0)
x2 <- c(1, 0)
x3 <- c(0.5, 1)
plot(hclust(dist(rbind(x1, x2, x3)), method = "centroid"), 
     main = "Centroid Linkage", ylim = c(-0.1, 1.1), 
     xlab = "", sub = "", cex = .9)
####################################################################
hc.complete <- hclust(dist(x), method = "complete")
hc.average <- hclust(dist(x), method = "average")
hc.single <- hclust(dist(x), method = "single")
opar <- par(no.readonly = TRUE)
par(mfrow = c(1, 3))
plot(hc.complete, main = "Complete Linkage",xlab = "", sub = "", cex = .9)
plot(hc.average, main = "Average Linkage", xlab = "", sub = "", cex = .9)
plot(hc.single, main = "Single Linkage", xlab = "", sub = "", cex = .9)
par(mfrow = c(1, 1))
cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)
cutree(hc.single, 4)
xsc<-scale(x)
plot(hclust(dist(xsc),method="complete"),main="Hierarchical Clustering with Scaled
     Features")

x<-matrix(rnorm(90),ncol=3)
dd<-as.dist(1-cor(t(x)))
plot(hclust(dd,method="complete"),main="Complete Linkage with Correlation-Based Distance",xlab="",sub="")

