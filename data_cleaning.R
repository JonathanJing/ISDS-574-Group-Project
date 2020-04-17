rm(list=ls()); gc()
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Fullerton/Course/ISDS_574/ISDS-574-Group-Project")
## Read dataset
dat = read.csv('companies-2000_v7.csv', head=T, stringsAsFactors=F, na.strings='')
str(dat)
col_list <- colnames(dat)
write.csv(col_list, file = "column.csv")
write.csv(dat, file = "companies-2000_v6.csv")

library(tidyverse)

#loaction
lo <- dat$Headquarters.Location
lo_split <- strsplit(lo, ",")

first_lo <- function(i){
  l <- first(lo_split[i][[1]])
  return(l)
}
lo_first <- lapply(1:2000, first_lo) %>% as.character()

dat2 <- data.frame(Y = dat$Y, lo = lo_first)
library(fastDummies)
dat3 = dummy_columns(dat2, select_columns = 'lo', remove_most_frequent_dummy = F)
write.csv(dat3, file = "location.csv")


#Industry.Groups
#take only the first group in the list, which is the major business of the company
ind <- dat$Industry.Groups
ind_split <- strsplit(ind, ",")

ac <- dat$Acquisition.Status
ac_split <- strsplit(ac, ",")
first_ac <- function(i){
  l <- first(ac_split[i][[1]])
  return(l)
}
ac_first <- lapply(1:2000, first_ac) %>% as.character()

temp <- data.frame(dat$Acquisition.Status)
names(temp)[1] <- "a"
temp$a <- as.character(temp$a)
strsplit(temp$a, ",")
lapply(1:2000, strsplit(temp$a, ","))
  
first_ind <- function(i){
  l <- first(ind_split[i][[1]])
  return(l)
}
ind_first <- lapply(1:2000, first_ind) %>% as.character()
ind_f <- gsub(" ", "", ind_first)
table(ind_f)

dat2 <- data.frame(Y = dat$Y, id = dat$id, ind_f)
ggplot(dat2, aes(x=ind_f)) + geom_histogram(stat = "count")

library(fastDummies)
dat3 = dummy_columns(dat2, select_columns = 'ind_f', remove_most_frequent_dummy = F)
write.csv(dat3, file = "instdustry_dummy.csv")

# Estimated.Revenue.Range
rev <- dat$Estimated.Revenue.Range
rev_na <- which(rev == "NA")

tfa <- dat$Total.Funding.Amount
tfa_na <- which(is.na(tfa))
both_na <- which(rev == "NA" & is.na(tfa))
tfa_1M <- tfa/1000000

df <- data.frame(rev, "a" = as.numeric(tfa_1M))
df <- df %>% mutate(category=cut(a, breaks = c(0,1,10,50,100,500,1000,10000,Inf), 
                           labels = c("Less than $1M", "$1M to $10M", "$10M to $50M", 
                                      "$50M to $100M", "$100M to $500M", "$500M to $1B", 
                                      "$1B to $10B", "$10B+")))
df$rev[rev_na] <- df$category[rev_na]
df$rev <- gsub(" ", "", df$rev)
table(df$rev)
dat2 <- data.frame(Y = dat$Y, id = dat$id, rev = df$rev)
library(fastDummies)
dat3 = dummy_columns(dat2, select_columns = 'rev', remove_most_frequent_dummy = F)
write.csv(dat3, file = "estimated_revenue.csv")

#Number.of.Employees
noe <- dat$Number.of.Employees
table(noe)
dat2 <- data.frame(Y = dat$Y, id = dat$id, noe)
dat3 = dummy_columns(dat2, select_columns = 'noe', remove_most_frequent_dummy = F)
write.csv(dat3, file = "number_of_employees.csv")

#Total.Funding.Amount
tfa <- dat$Total.Funding.Amount
tfa_na <- which(is.na(tfa))
both_na <- which(rev == "NA" & is.na(tfa))
tfa_1M <- tfa/1000000

df <- data.frame(Y = dat$Y, id = dat$id, "tfa_1M" = as.numeric(tfa_1M))
df <- df %>% mutate(category=cut(tfa_1M, breaks = c(0,1,10,50,100,500,1000,10000,Inf), 
                                 labels = c("Less than $1M", "$1M to $10M", "$10M to $50M", 
                                            "$50M to $100M", "$100M to $500M", "$500M to $1B", 
                                            "$1B to $10B", "$10B+")))
df$category <- gsub(" ", "", df$category)
dat3 = dummy_columns(df, select_columns = 'category', remove_most_frequent_dummy = F)
write.csv(dat3, file = "total_funding_amount.csv")

# Number.of.Investors
df.noi <- data.frame(id = dat$id, noi = dat$Number.of.Investors)
noi_num <- as.numeric(dat$Number.of.Investors)
df.noi <- df.noi %>% mutate(category=cut(noi_num, breaks = c(0,5,10,15,20,Inf), 
                           labels = c("1to5", "5to10", "10to15", 
                                      "15to20", "20+")))
df.noi$category <- addNA(df.noi$category)
dat3 = dummy_columns(df.noi, select_columns = 'category', remove_most_frequent_dummy = F)
write.csv(dat3, file = "number_of_investors.csv")

# Number.of.Lead.Investors
df.nol <- data.frame(id = dat$id, Lead_Investors = dat$Number.of.Lead.Investors)
df.nol$Lead_Investors <- addNA(df.nol$Lead_Investors)
dat3 = dummy_columns(df.nol, select_columns = 'Lead_Investors', remove_most_frequent_dummy = F)
write.csv(dat3, file = "number_of_lead_investors.csv")


# Number.of.Founders
df.nof <- data.frame(id = dat$id, nof = dat$Number.of.Founders)
dat3 = dummy_columns(df.nof, select_columns = 'nof', remove_most_frequent_dummy = F)
write.csv(dat3, file = "number_of_founders.csv")

# Number.of.Events
df.noe <- data.frame(id = dat$id, noe = dat$Number.of.Events)
miss <- is.na(df.noe)
colMeans(miss)

#IPqwery...Total.Patents
df.pa <- data.frame(id = dat$id, pa = dat$IPqwery...Total.Patents)
pa <- dat$IPqwery...Total.Patents
pa <- gsub(" ", "", pa)
pa_num <- as.numeric(pa)
df.pa <- df.pa %>% mutate(Patents=cut(pa_num, breaks = c(-Inf,10,100,Inf), 
                                         labels = c("0to10", "10to100", "100+")))
df.pa$Patents <- addNA(df.pa$Patents)
dat3 = dummy_columns(df.pa, select_columns = 'Patents', remove_most_frequent_dummy = F)
write.csv(dat3, file = "total_patents.csv")

#IPqwery...Total.Trademarks
df.tr <- data.frame(id = dat$id, tr = dat$IPqwery...Total.Trademarks)
tr <- dat$IPqwery...Total.Trademarks
tr <- gsub(" ", "", tr)
tr_num <- as.numeric(tr)
df.tr <- df.tr %>% mutate(Trademarks=cut(tr_num, breaks = c(-Inf,10,100,Inf), 
                                      labels = c("0to10", "10to100", "100+")))
df.tr$Trademarks <- addNA(df.tr$Trademarks)
dat3 = dummy_columns(df.tr, select_columns = 'Trademarks', remove_most_frequent_dummy = F)
write.csv(dat3, file = "total_trademarks.csv")


# Partition
set.seed(2020)
n.train = floor( nrow(dat3)*.7 )
id.train = sample(1:nrow(dat3), n.train) 
id.test = setdiff(1:nrow(dat3), id.train) 
train <- dat3[id.train,]
test <- dat3[id.test,]
#One way to retrive train list 
list <- read.csv(file = "train_num.csv")
train_list <- list$x


##Neuralnet for test dummy variable
library(neuralnet)
library(nnet)
library(caret)
library(e1071)

nn <- neuralnet( Y ~ ., data = train[,-(2:3)], hidden = c(40,30,20,10,5), linear.output = F)

## plot network
plot(nn, rep="best")

predict <- compute(nn, test[,-(1:3)])
result <- data.frame(actual = test$Y, prediction = predict$net.result)
result$prediction <- sapply(result$prediction,round,digits=0)
confusionMatrix(as.factor(result$prediction), as.factor(result$actual))

id_0 <- which(result$actual == 0)
id_1 <- setdiff(1:2000, id_0)
summary(result[id_0,])
summary(result[id_1,])

#compare with naive rule
# 0.762
y_naive <- rep(0, nrow(test))
naive_result <- data.frame(actual = test$Y, prediction = y_naive)
confusionMatrix(as.factor(naive_result$prediction), as.factor(naive_result$actual))
