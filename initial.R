rm(list=ls()); gc()
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Fullerton/Course/ISDS_574/ISDS-574-Group-Project")
## Read dataset
dat4 = read.csv('companies-2000_v5.csv', head=T, stringsAsFactors=F, na.strings='')

## Dataset overview, create column of id
str(dat)
id <- rep(1: nrow(dat))
dat2<- cbind(id,dat)

#Remove comma from number
dat3 <- dat2
dat3$CB.Rank..Company. <- as.numeric(gsub(",", "", dat3$CB.Rank..Company.))

#Best rank
min <- min(dat3$CB.Rank..Company.)
#Leasr rank
max <- max(dat3$CB.Rank..Company.)

# How many companies are in 10000 cb rank
rank_10000 <- which(dat4$CB.Rank..Company. <= 10000)
# Only 452 companies
length(rank_10000)

#Create outcome
dat4$Y <- rep(0, nrow(dat4))
dat4$Y[rank_10000] <- 1
write.csv(dat4, file = "companies-2000_v4.csv")


# Partition
set.seed(2020)
n.train = floor( nrow(dat4)*.7 )
id.train = sample(1:nrow(dat4), n.train) 
id.test = setdiff(1:nrow(dat4), id.train) 
train <- dat4[id.train,]
test <- dat4[id.test,]

#Write list of row in train/test set
#This is unique row number, all team follow only one set number
write.csv(id.train, file = "train_num.csv")
write.csv(id.test, file = "test_num.csv")

#Write train/test set
write.csv(train, file = "train.csv")
write.csv(test, file = "test.csv")

#One way to retrive train list 
list <- read.csv(file = "train_num.csv")
train_list <- list$x

#Model example: Naive Rule
#Which means no matter how companies make effects, they gonna fail anyway
#Cutoff = 0.85
#any rank_op > 0.85 as 1 in ytest list
#all rank_op as 0 in yprep list
ytest <- rep(0, nrow(test))
ytest[which(test$CB.Rank..Company. < 10000)] <- 1
ypred <- rep(0, nrow(test))

library(caret)
confusionMatrix(as.factor(ypred), as.factor(ytest))
