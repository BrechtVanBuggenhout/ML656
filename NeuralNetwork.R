# read in csv file and appropriate libraries
library(Hmisc)
dat <- read.table("winequality-red.csv", sep=";", header=TRUE)
dim(dat)
describe(dat)[,1:9]
head(dat)

# defining high quality wines
table(dat$quality)
dat$quality <- I(dat$quality > 6) * 1
describe(dat)[,1:9]

# 	Some exploration
library(corrplot)
corrplot(cor(dat), method="number")

doPlot <- function(vvar){
  n <- xtabs(~dat[,vvar])
  x <- unique(dat[,vvar])
  x <- x[order(x)]
  y <- tapply(dat$quality, dat[,vvar], mean)
  plot(x,y, cex=(n/mean(n))^.5, col="navy", lwd=2
       , xlab=vvar, ylab="high quality", main=vvar, las=1)
  abline(h=mean(dat$quality), col="blue", lwd=2)
  abline(lm(y~x, weight=n), col="chartreuse3", lwd=2)
}

par(mfrow=c(1,1))
doPlot("fixed.acidity")
doPlot("volatile.acidity")
doPlot("citric.acid")
doPlot("residual.sugar")
doPlot("chlorides")
doPlot("free.sulfur.dioxide")
doPlot("total.sulfur.dioxide")
doPlot("density")
doPlot("pH")
doPlot("sulphates")
doPlot("alcohol")

# splitting the dataset --> 70% goes into training set
set.seed(42)
trn <- runif(nrow(dat)) < .7
train <- dat[trn==TRUE,]
tst <- dat[trn==FALSE,]
Y.tst <- test$quality

# create formula
form1 <- formula(quality~.)

# First, we need to get out data in the right format. 
# In particular, we need to scale the input variables for usage in neural nets:
dat2 <- dat
dat2[,1:12] <- scale(dat2[,1:12])
dat2$quality <- factor(dat2$quality)
train2 <- dat2[-tst,]
test2 <- dat2[tst,]
dim(train2); dim(test2)

# We commence with a good combination from prior studies: a size of 3, 
# maximial iterations of 100, and a decay= of 0.0001.
library(nnet)
set.seed(1)
n1 <- nnet(form1, data=train, size=3, maxit=500, decay=0.001)

# lets check predictions
yhat.n1 <- predict(n1, tst)

#Weird scale so let's rescale
yhat.n1 <- (yhat.n1-min(yhat.n1))/(max(yhat.n1)-min(yhat.n1))
table(yhat.n1[,1]>0.5, test$quality)

# check the ROC curve and AUC
library("pROC")
n1.roc <- roc(test$quality, yhat.n1[,1], direction="<")
n1.roc
plot(n1.roc, lwd=3)

# neural net 2
set.seed(4)
n2 <- nnet(form1, data=train, size=10, maxit=200, decay=0.001)
yhat.n2 <- predict(n2, test)
yhat.n2 <- (yhat.n2-min(yhat.n2))/(max(yhat.n2)-min(yhat.n2))
table(yhat.n2[,1]>0.5, test$quality)
n2.roc <- roc(test$quality, yhat.n2[,1], direction="<")
n2.roc
plot(n2.roc, add = TRUE, col="blue")

# neural net 3
set.seed(4)
n3 <- nnet(form1, data=train, size=10, maxit=500, decay=0.001)
yhat.n3 <- predict(n3, test)
yhat.n3 <- (yhat.n3-min(yhat.n3))/(max(yhat.n3)-min(yhat.n3))
table(yhat.n3[,1]>0.5, test$quality)
n3.roc <- roc(test$quality, yhat.n3[,1], direction="<")
n3.roc
plot(n3.roc, add = TRUE, col="red")

# lets use more iterations
set.seed(4)
n4 <- nnet(form1, data=train, size=5, maxit=1000, decay=0.0001)

# check predictions
yhat.n4 <- predict(n4, test)
yhat.n4 <- (yhat.n4-min(yhat.n4))/(max(yhat.n4)-min(yhat.n4))
table(yhat.n4[,1]>0.5, test$quality)

# plot ROC and look at AUC
n4.roc <- roc(test$quality, yhat.n4[,1], direction="<")
n4.roc
plot(n4.roc, add = TRUE, col="green")

# compare nets
temp <- data.frame(yhat.n1[,1],yhat.n2[,1],yhat.n3[,1],yhat.n4[,1],test$quality)
rho <- cor(temp)
corrplot(rho, method="number")
