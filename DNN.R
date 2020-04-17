rm(list=ls()); gc()
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Fullerton/Course/ISDS_574/ISDS-574-Group-Project")
## Read dataset
dat = read.csv('companies-2000_v8.csv', head=T, stringsAsFactors=F, na.strings='')

library(readr) # CSV file I/O, e.g. the read_csv function
library(plyr)
library(dplyr)
library(mice)
library(tibble)
library(RWeka)
library(neuralnet)
library(deepnet)
library(caret)
library(e1071)

normalize <- function(x) {  return((x - min(x)) / (max(x) - min(x)))}

dat1 <- subset(dat, select = -c(id, Organization.Name, noe))
dat1 <- mutate_all(dat1, function(x) as.numeric(as.character(x)))
dat1[is.na(dat1)] <- 0
dat1 %>% cor() %>% round(2) %>% View()

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

#Using industry
dat_ind = read.csv('instdustry_dummy.csv', head=T, stringsAsFactors=F, na.strings='')
dat2 <- subset(dat_ind, select = -c(X, id, ind_f))
dat2 <- mutate_all(dat2, function(x) as.numeric(as.character(x)))
dat2[is.na(dat2)] <- 0
dat2 <- dat2
dat2 <- subset(dat2, select = -c(Y))
dat1 <- cbind(dat1, dat2)

#location
dat_lo = read.csv('location.csv', head=T, stringsAsFactors=F, na.strings='')
dat1 <- subset(dat_lo, select = -c(X, lo))

dat1 <- cbind(dat4, dat5)

dat1 <- mutate_all(dat1, function(x) as.factor(as.character(x)))
# Partition
set.seed(2020)
n.train = floor( nrow(dat1)*.7 )
id.train = sample(1:nrow(dat1), n.train) 
id.test = setdiff(1:nrow(dat1), id.train) 
train <- dat1[id.train,]
test <- dat1[id.test,]

x=as.matrix(train[,-1])
y=train[,1]

#SVM
y_svm <- as.factor(y)
x_svm <- as.factor(x)
SVM <- svm(Y ~ ., data = train, kernel = "sigmoid")
svm_prediction <- predict(SVM, test[,-1])
confusionMatrix(svm_prediction, as.factor(test$Y))

#DNN
DL <- dbn.dnn.train(x,y, hidden=c(52,52,52),activationfun="tanh",batchsize=200, numepochs=10, learningrate = 0.9)
prediction <- nn.predict(DL,test[,-1])
prediction <- normalize(prediction)
result <- data.frame(actual = test$Y, prediction = prediction)
#cutoff
result$prediction <- sapply(result$prediction,round,digits=0)
confusionMatrix(as.factor(result$prediction), as.factor(result$actual))
#everything, 144 columns, 0.7633

id_0 <- which(result$actual == 0)
id_1 <- setdiff(1:2000, id_0)
summary(result[id_0,])
summary(result[id_1,])
