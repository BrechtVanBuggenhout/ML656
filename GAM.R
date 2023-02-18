# load in csv file and appropriate packages
wage_df <- read.csv('Wage.csv')
install.packages("mgcv")
library(mgcv)

# preview first few records
head(wage_df)

# split into test and training sets -- 75% in training set
index <- sort(sample(1:nrow(wage_df), round(0.75*nrow(wage_df))))
train <- wage_df[index,]
test <- wage_df[-index,]
dim(train);dim(test)

# store wage variable in Y so that we can make predictions later on the test data
YTest <- test$wage
YTrain <- train$wage

# look for nonlinear effects for continuous variables
gam1 <- gam(wage ~ s(year, bs = 'cr', k =3) + s(age, bs = 'cr') + education, data = train)
summary(gam1)

# plot GAM regression
plot(gam1, scale = 0)

# make predictions 
yhat_gam1 <- predict(gam1, newdata = test)
SST1 <- sum((YTest - mean(YTest))^2)
SSE1 <- sum((yhat_gam2 - YTest)^2)
R2 <- 1 - (SSE1/SST1)
R2

# compare to gam 2 regression where we ignore nonlinear effect of year 
gam2 <- gam(wage ~ year + s(age, bs = 'cr') + education, data = train, family=gaussian)
summary(gam2)

# plot GAM regression 2 for continuous variables
plot(gam2, scale = 0)

# make predictions for 2nd regression
yhat_gam2 <- predict(gam2,newdata = test)
SST2 <- sum((YTest - mean(YTest))^2)
SSE2 <- sum((yhat_gam2 - YTest)^2)
R2 <- 1 - (SSE2/SST2)
R2

# compare to linear model
lm1 <- lm(wage ~ year + age + education, data = train)
summary(lm1)

# make predictions
yhat_lm1 <- predict(lm1, newdata = test)
SST3 <- sum((YTest-mean(YTest))^2)
SSE3 <- sum((yhat_lm1-YTest)^2)
R2 <- 1 - (SSE3/SST3)
R2

# install markdown packages
install.packages("rmarkdown")
