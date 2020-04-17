rm(list=ls()); gc()
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Fullerton/Course/ISDS_574/ISDS-574-Group-Project")
## Read dataset
dat = read.csv('companies-2000_v8.csv', head=T, stringsAsFactors=F, na.strings='')

# Partition
set.seed(2020)
n.train = floor( nrow(dat)*.7 )
id.train = sample(1:nrow(dat), n.train) 
id.test = setdiff(1:nrow(dat), id.train) 
train <- dat[id.train,]
test <- dat[id.test,]

#Write train/test set
write.csv(train, file = "train_v8.csv")
write.csv(test, file = "test_v8.csv")

#One way to retrive train list 
list <- read.csv(file = "train_num.csv")
id.train <- list$x