

rm(list=ls()); gc()
setwd("C:/Users/evany/Desktop/Yida Cai/CSUF/ISDS 574 Data Mining for Business Applications/Group Project")

#Remove noe, Organization.Name, X, and id

dat <- read.csv("train_v8.csv")
dat1 <- read.csv("test_v8.csv")
dat.train <- subset(dat, select = - c(noe, Organization.Name, X, id))
dat.test <- subset(dat1, select = - c(noe, Organization.Name, X, id))

#Covert missing values to 0
dat.train[is.na(dat.train)] <- 0

#Factor 


# In the training dataset, 1091 belongs to 0, 309 belongs to 1

table(dat.train$Y) 

# Random Forest
require(randomForest)


rf.ubd <- randomForest(Y~., mtry = 143, data=dat.train)
rf.ubd
plot(rf.ubd)


yhat <- predict(rf.ubd, newdata = dat.test, type = "response")
yhat


dichotomize = function(yhat, cutoff) {
  out = rep(0, length(yhat))
  out[yhat > cutoff] = 1
  out
}


# Misclassification Error Rate
yhat.class = dichotomize(yhat, .1)
err = mean(yhat.class != dat.test$Y)
err

# Sensitivity & Specificity
sen = function(ytrue, yhat) {
  ind.true1 = which(ytrue == 1)
  mean( ytrue[ind.true1] == yhat[ind.true1] )
}

spe = function(ytrue, yhat) {
  ind.true0 = which(ytrue == 0)
  mean( ytrue[ind.true0] == yhat[ind.true0] )
}

sen(dat.test$Y, yhat.class)
spe(dat.test$Y, yhat.class)


#Examine the variable importance
importance(rf.ubd)
varImpPlot(rf.ubd)

#The tree from the variable selection (first 13 of %IncMSE) of random forest

require(rpart)

tree.rf <- rpart(Y~ Days_after_Last_Funding+ Funding_Status_Seed+ Last_Funding_Amount+ Days_Founded+ category_20.+ Number_of_Funding_Rounds+ category_.500Mto.1B+ Lead_Investors_NA+ 
                   category_.10Mto.50M+ category_10to15+ Lead_Investors_5+ category_.1Mto.10M
                              , data = dat.train, method = "class")
library(rpart.plot)
rpart.plot(tree.rf)

yhat.rf <- predict(tree.rf, dat.test, type = "class")
err = mean(yhat.rf != dat.test$Y)
err
