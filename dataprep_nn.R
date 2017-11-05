rm(list = ls())
library(data.table)
library(zoo)
library(forecast)
library(ggplot2)
library(h2o)
library(dplyr)

train <- fread("D:/EPAM/train.csv")
store <- fread("D:/EPAM/store.csv")
target<- fread("D:/EPAM/target.csv")
train=bind_rows(train, target)

#DATETIME EXTRACTION
train[, Date := as.Date(Date)]
train[,month:=as.integer(format(Date, "%m"))]
train[,year:=as.integer(format(Date, "%y"))]
train[,Store:=as.factor(as.numeric(Store))]
#Take log of sales
train[,logSales:=log1p(Sales)]
#Sundays are exceptionally important
train$Sunday=train$DayOfWeek==7
#December is also important
train$December=train$month==12
train <- train[order(Date)]
train$SPC=train$Sales/train$Customers
#train = train[(train["Open"] != 0) & (train['Sales'] != 0)]

train[, lapply(.SD, function(x) length(unique(x)))]
store$CompetitionOpenSince <- as.yearmon(paste(store$CompetitionOpenSinceYear, 
                                               store$CompetitionOpenSinceMonth, sep = "-"))
store$Promo2Since <- as.POSIXct(paste(store$Promo2SinceYear, 
                                      store$Promo2SinceWeek, 1, sep = "-"),
                                format = "%Y-%U-%u")
store$logComDist=log1p(store$CompetitionDistance)
train_store <- merge(train, store, by = "Store")
#StretypeB and Jun is also important
train_store$BJun=train_store$StoreType=="b" & train$month==6
salesByDist <- aggregate(train_store[Sales != 0 & !is.na(CompetitionDistance)]$Sales, 
                         by = list(train_store[Sales != 0 & !is.na(CompetitionDistance)]$CompetitionDistance), mean)
colnames(salesByDist) <- c("CompetitionDistance", "MeanSales")
# Sales before and after competition opens
train_store$DateYearmon <- as.yearmon(train_store$Date)
train_store <- train_store[order(Date)]
beforeAndAfterComp <- function(s) {
  x <- train_store[Store == s]
  daysWithComp <- x$CompetitionOpenSince >= x$DateYearmon
  if (any(!daysWithComp)) {
    compOpening <- head(which(!daysWithComp), 1) - 1
    if (compOpening > timespan & compOpening < (nrow(x) - timespan)) {
      x <- x[(compOpening - timespan):(compOpening + timespan), ] 
      x$Day <- 1:nrow(x)
      return(x)
    }
  }
}
beforeAndAfterPromo2 <- function(s) {
  x <- train_store[Store == s]
  daysWithPromo2 <- as.yearmon(x$Promo2Since) >= x$DateYearmon
  if (any(!daysWithPromo2)) {
    print("LOLMAX")
    Promo2Opening <- head(which(!daysWithPromo2), 1) - 1
    if (Promo2Opening > timespan & Promo2Opening < (nrow(x) - timespan)) {
      x <- x[(Promo2Opening - timespan):(Promo2Opening + timespan), ] 
      x$Day <- 1:nrow(x)
      return(x)
    }
  }
}

#timespan <- 100 # Days to collect before and after Opening of competition
#Comptemp <- lapply(unique(train_store[!is.na(CompetitionOpenSince)]$Store), beforeAndAfterComp)
#Comptemp <- do.call(rbind, Comptemp)
#length(unique(Comptemp$Store))

# 147 stores first had no competition but at least 100 days before the end
#promo2temp <- lapply(unique(train_store[!is.na(Promo2Since)]$Store), beforeAndAfterPromo2)
#promo2temp <- do.call(rbind, promo2temp)
train_store=train_store[,!"DateYearmon"]
taaaaarget=train_store[!is.na(train_store$Id),]
taaaaarget[taaaaarget$Open==0,]
## Use H2O's random forest
## Start cluster with all available threads
write.csv(train_store,"D:/EPAM/train_store_nnfeed.csv")