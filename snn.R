rm(list=ls()); gc()
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Fullerton/Course/ISDS_574/ISDS-574-Group-Project")
## Read dataset
dat = read.csv('companies-2000_v8.csv', head=T, stringsAsFactors=F, na.strings='')

dat1 <- dat
dat1 <- mutate_all(dat1, function(x) as.numeric(as.character(x)))
dat1[is.na(dat1)] <- 0
##Neuralnet
library(neuralnet)
library(nnet)
library(caret)
library(e1071)

#Using total funding amount
dat_tf = read.csv('total_funding_amount.csv', head=T, stringsAsFactors=F, na.strings='')
dat2 <- subset(dat_tf, select = -c(X, id, tfa_1M, category))
dat2 <- mutate_all(dat2, function(x) as.numeric(as.character(x)))
dat2[is.na(dat2)] <- 0
dat1 <- dat2

#Using Acquisition Status
dat_ac = read.csv('Acquisition_Status_.csv', head=T, stringsAsFactors=F, na.strings='')
dat2 <- subset(dat_ac, select = -c(X))
dat1 <- cbind(dat1, dat2)

#Using number of funding rounds
dat_nof = read.csv('Number_of_Funding_Rounds.csv', head=T, stringsAsFactors=F, na.strings='')
dat2 <- subset(dat_nof, select = -c(X,Y,id))
dat1 <- cbind(dat1, dat2)

# Partition
set.seed(2020)
n.train = floor( nrow(dat1)*.7 )
id.train = sample(1:nrow(dat1), n.train) 
id.test = setdiff(1:nrow(dat1), id.train) 
train <- dat1[id.train,]
test <- dat1[id.test,]

nn <- neuralnet( Y~ Days_after_Last_Funding+ Funding_Status_Seed+ Last_Funding_Amount+ Days_Founded+ category_20.+ Number_of_Funding_Rounds+ category_.500Mto.1B+ Lead_Investors_NA+ 
                   category_.10Mto.50M+ category_10to15+ Lead_Investors_5+ category_.1Mto.10M, data = train, hidden = 12, linear.output = F)

## plot network
plot(nn, rep="best")

predict <- compute(nn, test[,-1])
result <- data.frame(actual = test$Y, prediction = predict$net.result)
#cutoff 0.5
result$prediction <- sapply(result$prediction,round,digits=0)
confusionMatrix(as.factor(result$prediction), as.factor(result$actual))
#everything, 144 columns, 0.7633