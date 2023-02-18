# load in required packages and data
data <- read.table("winequality-red.csv", sep=";", header=TRUE)
library(psych)

# let's prepare the data
dim(data)
table(data$quality)
data$quality <- I(data$quality > 6) * 1
describe(data)[,1:9]
library(corrplot)
corrplot(cor(data), method="number")

# Try to omit missing data
dat <- data
dd <- describe(dat)[,1:9] 
dd
dim(dat)
dim(na.omit(dat))
# no missing data to be omitted

# scaling the data
dat <- scale(dat)
dat <- as.data.frame(dat)
dat[1:2,]
describe(dat)[,1:9]

# look at the dimendions and correlations again
describe(dat)[,1:4]
col3 <- colorRampPalette(c("red", "white", "blue"))
corrplot(cor(dat), tl.cex=.7, order="hclust", col=col3(50))

# the appropriate number of clusters, we evaluate how the within sum of squares varies by clusters
ss <- function(x)  sum( ( x-mean(x) )^2 )
wss <- NULL
wss[1] <- sum( apply(dat,2,ss) )
for (k in 2:10) {
  temp <- kmeans(dat, k)
  wss[k] <- sum(temp$withinss)
}

barplot(wss, col="dodgerblue", names.arg=1:length(wss)
        , xlab="Number of Clusters (k)"
        , ylab="Total Within Sum of Squares")
abline(h=0)
title("Sum-of-Squares Analysis", col.main="navy")

## start clustering
k <- 5
set.seed(652)
km <- kmeans(dat, k)
clust.km <- km$cluster

# dendrogram to show hierarchical relationship
dd <- dist(dat, method="euclidean")
hc1 <- hclust(dd, method="average")
hc1 <- hclust(dd, method="complete")
hc1 <- hclust(dd, method="ward.D")
plot(hc1, hang=-1)
rect.hclust(hc1, k=6, border="dodgerblue")
rect.hclust(hc1, k=5, border="blue")
rect.hclust(hc1, k=4, border="red")
rect.hclust(hc1, k=3, border="green")

hc1 <- hclust(dd, method="ward.D")
rect.hclust(hc1, k=5, border="dodgerblue")

clust.hc1 <- cutree(hc1,5)

# aid visualization
reord <- function(cluster){
  avg <- tapply(scored$quality, cluster, mean); avg
  ord <- order(avg); ord
  clus <- factor(cluster, levels=ord); table(clus)
  levels(clus) <- 1:length(clus)
  return( as.numeric(as.character(clus)) )
}


scored <- dat
scored$clust.km <- reord(clust.km)
scored$clust.hc <- reord(clust.hc1)

# check results
tapply(scored$quality, clust.km, mean)
tapply(scored$quality, reord(clust.km), mean)
table(clust.km, reord(clust.km))
tapply(scored$quality, clust.hc1, mean)
tapply(scored$quality, reord(clust.hc1), mean)
table(clust.hc1, reord(clust.hc1))
table(scored$clust.km, scored$clust.hc)
# results seem similar but htey are not equivalent at all

## PCA
# perform PCA
pc1 <- prcomp(dat)
pc1 <- prcomp(scale(dat))
pc1

# compute PCs and their correlation
pcs <- predict(pc1) 
describe(pcs)[,1:5]

dim(pcs); dim(dat)
corrplot(cor(pcs))  
# none of the Pcs are correlated which is expected

# prepare scree plot
vars <- apply(pcs, 2, var)
sum(vars); ncol(dat); ncol(pcs)
barplot(vars[1:12], col="lightblue", ylab="variance", las=1)
title("Principal Components Analysis Scree Plot", col.main="navy")
abline(h=1:7, col="darkcyan")
abline(h=0)

# more exploration of pc1
plot(pc1)
summary(pc1)

# The first PC seems to account for a lot of the variation
# illustrate through bi-plot
biplot(pc1, col=c("slategrey", "navy"), cex=c(.2, .8))
round(pc1$rotation, 4)[,1:2]

# visualize clusters in the PC space
col <- c("blue","dodgerblue","lightgreen","pink","red")
par(mfrow=c(1,2))

clust <- scored$clust.km
plot(pcs, type="n", main="k-means")
text(pcs, labels=clust, col=col[clust])
abline(h=0); abline(v=0)

clust <- scored$clust.hc
plot(pcs, type="n", main="hierarchical clustering")
text(pcs, labels=clust, col=col[clust])
abline(h=0); abline(v=0)

## Analyzing the clusters
# consider aggregate stats by variables of clusters
col <- c("blue", "dodgerblue", "lightgreen", "pink", "red", "maroon", "darkorange")
clust <- scored[, "clust.km"]
agg <- function(x) tapply(x, clust, mean)
summ <- apply(dat, 2, agg)
t(round(summ,2))

# next include the PCs
scored2 <- dat
scored2$PC1 <- pcs[,1]
scored2$PC2 <- pcs[,2]
scored2$PC3 <- pcs[,3]
clust <- scored[, "clust.km"]
agg <- function(x) tapply(x, clust, mean)
SUMMARY <- apply(scored2, 2, agg)
t(round(SUMMARY,2))

# visualize this
show <- 1:4
show <- 5:8
show <- 9:11
par(mfrow=c(2,4))
for(i in show){
  boxplot( scored[,i] ~ clust, col=col, varwidth=TRUE)
  abline(h=0, col="navy")
  title(names(scored)[i])
}

