# read in csv file and aprropriate libraries
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

# Setting up the random forest
install.packages('randomForest')
library(randomForest)

X <- as.matrix(train[,-12])
Y <- factor(train$quality)

mtry <- round(ncol(X)^0.5); mtry

# Fitting random forest to the dataset
ntree <- 1000
set.seed(652)
rf1 <- randomForest(x=X, y=Y, ntree=ntree, mtry=mtry, importance=TRUE)
rf1

# summary of the random forest
summary(rf1)
names(rf1)

# look at "importance' since it gives us the influence of each feature
importance(rf1)

# look at the rf1 in a plot setting
varImpPlot(rf1)

# evaluating predictions
pred.rf1 <- predict(rf1, test)
table(pred.rf1, test$quality)
yhat.rf1 <- predict(rf1, test, type="prob")[,2]

# let's look at the ROC curve and AUC
library(pROC)
rf1.roc <- roc(test$quality, yhat.rf1, direction="<")
rf1.roc
plot(rf1.roc, lwd=3)

# ADA boosting set up
install.packages('gbm')
library(gbm)

# let's look at the fit with the ada boost
abt1 <- gbm(quality~., data = train, distribution = "adaboost", n.trees = 500, interaction.depth=6, shrinkage = 0.005)
summary(abt1)

# illustrate each variable's fit through a plot
plot(abt1, i.var = 11)

# check the predictions
yhat.abt1 <- predict(abt1,newdata = test, n.trees = 500, type="response")
yhat.abt1
table(yhat.abt1 > 0.5, test$quality)

# check AUC and ROC curve after ADA and compare to first random forest
abt1.roc <- roc(test$quality, yhat.abt1, direction="<")
abt1.roc
plot(rf1.roc, lwd=3)
lines(abt1.roc, col="green", lwd=3)


