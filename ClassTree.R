# read in csv file and aprropriate libraries
install.packages('Hmisc')
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
test <- dat[trn==FALSE,]
Y.tst <- test$quality

# create classification tree
library(rpart)
form1 <- formula(quality~.)
t1 <- rpart(form1, data=train, cp=.001, method="class")
plot(t1,uniform=T,compress=T,margin=.05,branch=0.3)
text(t1, cex=.7, col="navy",use.n=TRUE)

# plotting the t1
plotcp(t1)

# 
CP <- printcp(t1)

# the left variable is a good choice for pruning if below the line in classifiaction trees
cp <- CP[,1][CP[,2]==2]
cp

# prune according to cp calculate above
t2 <- prune(t1,cp=cp[1])
plot(t2,uniform=T,compress=T,margin=.05,branch=0.3)
text(t2, cex=.7, col="navy",use.n=TRUE)

# make predictions based on tree 2
yhat.t2 <- predict(t2, test, type="prob")[,2]
table(yhat.t2>0.5,Y.tst)

# 
install.packages("ROCR")
library(pROC)
library(ROCR)
TPR <- (21/(21+44))
TPR

# plotting the ROC curve and calculating the AUC
t2.roc <- roc(Y.tst, yhat.t2, direction="<")
t2.roc
plot(t2.roc, lwd=3)

# 