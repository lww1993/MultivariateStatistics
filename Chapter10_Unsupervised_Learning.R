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
##################################################################################
##Principal COmponents Analysis###################################################
##################################################################################
#to get the row names
states <- row.names(USArrests)
states
#to get the column names
names(USArrests)
#to get the mean and variance of murder/assault/population/rape 
apply(USArrests, 2, mean)
apply(USArrests, 2, var)
#to carry the PCA with the standardized data 
pr.out <- prcomp(USArrests, scale = TRUE)
names(pr.out)
#to get the means and standard deviations of the variables that are used for scaling before implementing PCA
pr.out$center
pr.out$scale
#to get the principal component loadings
pr.out$rotation
#to get the principal component score vectors
dim(pr.out$x)
scale(USArrests) %*% pr.out$rotation
#to plot the first two principal components
biplot(pr.out, scale = 0)
#another representation
pr.out$rotation <- (-pr.out$rotation)
pr.out$x <- (-pr.out$x)
biplot(pr.out, scale = 1)
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)
par(mfrow = c(1, 2))
pr.out1 <- prcomp(USArrests, scale = TRUE)
pr.out1$rotation <- (-pr.out1$rotation)
pr.out1$x <- (-pr.out1$x)
biplot(pr.out1, scale = 0)
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)
pr.out2 <- prcomp(USArrests, scale = FALSE)
#pr.out2$rotation <- (-pr.out2$rotation)
#pr.out2$x <- (-pr.out2$x)
biplot(pr.out2, scale = 0)
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)
par(mfrow = c(1, 1))
#to calculate the standard deviation and variance of each principal component
pr.out$sdev
pr.var=pr.out$sdev^2
#To compute the proportion of variance explained by each principal component
pve<-pr.var/sum(pr.var)
par(mfrow = c(1,2))
plot(pve,xlab="Principal Component",ylab="Proportion of Variance Explained",
     ylim=c(0,1),type="b")
#To plot the cumulative proportion of variance explained
plot(cumsum(pve),xlab="Principal Component",ylab="Cumulative Proportion of Variance Explained",ylim=c(0,1),type="b") 
par(mfrow = c(1,1))
################################################################################
#NCI60 Data Example############################################################
###############################################################################
library(ISLR)
nci.labs <- NCI60$labs
nci.data <- NCI60$data
dim(nci.data)
nci.labs[1:4]
table(nci.labs)
#PCA on the NCI60 Data
pr.out <- prcomp(nci.data, scale = TRUE)
#to colour points in different types with different colours
Cols <- function(vec){
  cols <- rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}
#to plot each point with its scores, and label it with type-related colors
par(mfrow = c(1, 2))
plot(pr.out$x[, 1:2], col = Cols(nci.labs), pch = 19, xlab = "Z1", ylab = "Z2")
plot(pr.out$x[,c(1,3)], col = Cols(nci.labs), pch = 19, xlab = "Z1", ylab = "Z3")
par(mfrow = c(1, 1))
summary(pr.out)
plot(pr.out)
#to plot the scree plot
pve <- 100 * pr.out$sdev ^ 2 / sum(pr.out$sdev ^ 2)
par(mfrow = c(1,2))
plot(pve, type = "o", ylab = "PVE", xlab = "Principal Component", col = "blue")
plot(cumsum(pve), type = "o", ylab = "Cumulative PVE", 
     xlab = "Principal Component", col = "brown3")
par(mfrow = c(1, 1))
#hierarchical analysis
sd.data <- scale(nci.data)
par(mfrow = c(1, 3))
data.dist <- dist(sd.data)
plot(hclust(data.dist), labels = nci.labs, 
     main = "Complete Linkage", xlab = "", sub = "", ylab = "")
plot(hclust(data.dist, method = "average"),labels = nci.labs,
     main = "Average Linkage", xlab = "", sub = "", ylab = "")
plot(hclust(data.dist, method="single"), labels = nci.labs, 
     main = "Average Linkage", xlab = "", sub = "", ylab = "")
par(mfrow = c(1, 1))
#to cluster with complete linkage
hc.out <- hclust(dist(sd.data))
hc.clusters <- cutree(hc.out, 4)
table(hc.clusters, nci.labs)
#to plot the result
par(mfrow = c(1,1))
plot(hc.out, labels = nci.labs)
abline(h = 139, col = "red")
hc.out
#to compare the results with k-means clustering and hierarchical clustering
set.seed(2)
km.out <- kmeans(sd.data, 4, nstart = 20)
km.clusters <- km.out$cluster
table(km.clusters, hc.clusters)
#to do hierarchical clustering on the first five principal component score vectors
hc.out <- hclust(dist(pr.out$x[, 1:5]))
plot(hc.out, labels = nci.labs, 
     main = "Hier. Clust. on First Five Score Vectors")
table(cutree(hc.out, 4), nci.labs)





