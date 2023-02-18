# load in dataset and show some of the properties
train_df <- read.csv('train.csv')
dim(train_df)
train_df[1:2,]
train_df <- train_df[,-1]

# preparation of the data
loss <- train_df$loss
hist(loss)
hist(log(loss))
qqnorm(log(loss))
qqline(log(loss), col= "red", lwd = 3)

# investigate some of the deviations of normal distribution --> log-transformation made regression much better
# look for very small losses, below $100 and eliminate them
quantile(loss)
quantile(loss, p=seq(0,1,0.1))
sum(loss<100)
loss[loss<100]

# validate to see if the data looks better with small loss amounts removed
train_df <- train_df[train_df$loss>=100,]
dim(train_df)
loss <- train_df$loss 
hist(log(loss))
qqnorm(log(loss))
qqline(log(loss), col = "blue", lwd = 3)

# divide train dataset into train and test sets
train <- runif(nrow(train_df)) < 0.2
table(train)
trn_train_df <-  train_df[train == TRUE,]
test_train_df <- train_df[train == FALSE,]
dim(trn_train_df)
dim(test_train_df)

# predictive modeling
# let's start with a linear regression for comparison with LASSO regression
r1 <- lm(loss ~ ., data = train_df)
summary(r1)

y1 <- trn_train_df$loss
y1_test <- test_train_df$loss

RMSE.train <- function(yhat) sqrt(mean((y1 - yhat)^2))
RMSE.test <- function(yhat) sqrt(mean((y1_test - yhat)^2))

RMSE.train.OLS <- RMSE.train(predict(r1, data = trn_train_df))
RMSE.test.OLS <- RMSE.test(predict(r1, data = test_train_df))

RMS.train.OLS
RMS.test.OLS

# can also use stepwise regression to find best fit
# load in appropriate libraries and perform stepwise AIC
library(MASS)
r2 <- stepAIC(r1, data = trn_train_df)
summary

# compare results to LASSO regression
# load in required library 
install.packages('glmnet')
library(glmnet)

X1 <- model.matrix(loss ~ ., data = trn_train_df)
X1 <- X[,-1]
dim(X) ; dim(trn_train_df)
Y1 <- trn_train_df[,131]
gln1 <- glmnet(X1,Y1, family = "gaussian")
cvl1 <- cv.glmnet(X1,Y1, family = "gaussian")

# plot the LASSO regresion
par(mfrow=c(1,2))
plot(gln1, lwd = 3)
plot(cvl1)

# minimize lambda to create best possible regression
lambda1 <- cvl1$lambda.min
lambda1

# show coefficients for LASSO regression
beta <- coef(gnl, s = lambda1)
c(beta[,1], coef(r1))

# other gln2 and cvl2
X2 <- as.matrix(trn_train_df[,1:131])
X2 <- X[,-1]
X2_test <- as.matrix(test_train_df[,1:131])

dim(X2_test)
dim(X2) 
Y2 <- trn_train_df$loss

gn2 <- glmnet(X2,Y2, family = "gaussian")
cv2 <- cv.glmnet(X2,Y2, alpha = 1)

# find best lambda for second model
lambda2 <- cvl2$lambda.min
lambda2

# plot the 2nd LASSO rgeression
par(mfrow=c(1,2))
plot(gln2, lwd = 3)
plot(cvl2)

# analyze final model
best_model <- glmnet(X2, Y2, alpha = 1, lambda = lambda2)
coef(best_model)

# define new observations
yhat.gln2 <- predict(gln2, s = lambda2, newx = X2_test)




