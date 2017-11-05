rm(list = ls())
library(data.table)
library(zoo)
library(forecast)
library(ggplot2)
library(h2o)
library(lubridate)
#h2o.shutdown()
train_store=read.csv("D:/EPAM/train_store_nnfeed.csv")
taaarget=train_store[!is.na(train_store$Id),]
#add features like sales in last month
#SALES last month average
train_store=train_store[train_store$Open==1,]
taaarget=train_store[!is.na(train_store$Id),]

train_store$YM=as.factor(paste0(train_store$year,train_store$month))
logSalesAvgMonth=aggregate(train_store$logSales,by=list(train_store$year,train_store$month,train_store$Store),mean)
names(logSalesAvgMonth)=c("year","month","Store","logSalesAvgMonth")
#add one to each month, so the average corresponds to the month before
logSalesAvgMonth$month=logSalesAvgMonth$month+2
logSalesAvgMonth[logSalesAvgMonth$month==13,]$year=logSalesAvgMonth[logSalesAvgMonth$month==13,]$year+1
logSalesAvgMonth[logSalesAvgMonth$month==13,]$month=1
logSalesAvgMonth[logSalesAvgMonth$month==14,]$year=logSalesAvgMonth[logSalesAvgMonth$month==14,]$year+1
logSalesAvgMonth[logSalesAvgMonth$month==14,]$month=2
taaarget=train_store[!is.na(train_store$Id),]

#same month last year
logSalesAvgYear=logSalesAvgMonth
logSalesAvgYear$year=logSalesAvgYear$year+1
names(logSalesAvgYear)=c("year","month","Store","logSalesAvgYear")
#Now lets see if there is competition around
taaarget=train_store[!is.na(train_store$Id),]

train_store$CompActive=as.yearmon(train_store$CompetitionOpenSince)<as.yearmon(train_store$Date)
train_store$CompActive[is.na(train_store$CompActive)]=1
train_store=merge(train_store,logSalesAvgMonth)
taaarget=train_store[!is.na(train_store$Id),]

#train_store=merge(train_store,logSalesAvgYear)
#Now separate the target and the given
#train_store_target=train_store[!is.na(train_store$Id),]
#train_store=train_store[is.na(train_store$Id),]
taaarget=train_store[!is.na(train_store$Id),]

#traindateEnd=round((max(as.Date(train_store$Date))-0.3*min(as.Date(train_store$Date))))
train_store_rf=train_store[,!names(train_store)%in%(c( "Date","Sales","Customers","SPC","CompetitionDistance","Promo2SinceYear","Promo2SinceWeek","Promo2Since","CompetitionOpenSinceYear","CompetitionOpenSinceMonth"))]
#train_store_rf$StateHoliday=as.factor(train_store_rf$StateHoliday)
#train_store_rf$SchoolHoliday=as.factor(train_store_rf$SchoolHoliday)
#train_store_rf=train_store_rf[train_store_rf$logSales>0.3,]
names(train_store_rf)
train_store_rf=train_store_rf[,c(11,23,19,5,3,7,8,9,12,13,14,15,16,20,22,10)]
#Scale Data
indx <- which(is.na(train_store_rf$logComDist), arr.ind = TRUE)
train_store_rf$logComDist[indx]= median(train_store_rf$logComDist,na.rm = TRUE)
train_store_rf$StateHoliday=train_store_rf$StateHoliday!="0"
train_store_rf$TypeB=train_store_rf$StoreType=="b"
train_store_rf$TypeD=train_store_rf$StoreType=="d"
train_store_rf=train_store_rf[,!(names(train_store_rf)%in%c("StoreType"))]
AssChars=c("a","b","c")
for(i in 1:3){
train_store_rf$Ass2=(train_store_rf$Assortment==AssChars[i])*i
}
train_store_rf=train_store_rf[,!names(train_store_rf)%in%c("Assortment","PromoInterval","CompetitionOpenSince","Store")]
names(train_store_rf)
train_store_rf=train_store_rf[,c(1:13,15:17,14)]
names(train_store_rf)

train_store_rf$SchoolHoliday=as.numeric(train_store_rf$SchoolHoliday)
maxs <- apply(train_store_rf[is.na(train_store_rf$Id),1:5], 2, max)
mins <- apply(train_store_rf[is.na(train_store_rf$Id),1:5], 2, min)
train_store_rf[,1:5] <- as.data.frame(scale(train_store_rf[,1:5],center = mins, scale = maxs - mins))


# Create Split (any column is fine)
#split = sample.split(train_store_rf$Store, SplitRatio = 0.70)
names(train_store_rf)
train_store_target=train_store_rf[!is.na(train_store_rf$Id),]
train_store_rf=train_store_rf[is.na(train_store_rf$Id),1:16]
#remove outliers
train_store_rf=train_store_rf[train_store_rf$logSales>0.35,]
# Split based off of split Boolean Vector
train_nn = train_store_rf[1:651890,]
test_nn = train_store_rf[651890:779722,]

#feats <- names(train_store_rf)[2:16]

h2o.init(nthreads = -1)
tr_h2o=as.h2o(train_nn)
test_h2o=as.h2o(test_nn)
#nn <- neuralnet(f,train,hidden=c(2,2),linear.output=FALSE,learningrate=0.5)
#model <- h2o.deeplearning(   x=names(train_nn)[2:16], adaptive_rate=TRUE,  y="logSales",   training_frame=tr_h2o,   nfold=5,   distribution="AUTO",  activation="Tanh",   hidden=c(3,3),   input_dropout_ratio=0.2,   sparse=TRUE,   l1=1e-5,   epochs=30)
#h2o.performance(model)
#test_pred_nn=h2o.predict(model,test_h2o)
#linear regression for benchmark
benchmark=h2o.glm(x=names(train_nn)[2:16],y="logSales",training_frame=tr_h2o)
h2o.performance(benchmark)
test_pred_glm=h2o.predict(benchmark,test_h2o)

####RANDOM FOREST#####
model2 <- h2o.randomForest(   x=names(train_nn)[2:16],ntrees = 30,
                             max_depth = 15,
                             nbins_cats = 1115,  y="logSales",   training_frame=tr_h2o  )


#plots to analyze results
plot(as.matrix(test_nn[,1]),as.matrix(as.list(test_pred_glm)),col="red",xlim=c(0,1),ylim=c(0,1))
points(as.matrix(test_nn[,1]),as.matrix(test_nn[,1]),col="black")
#plot(as.matrix(test[,1]),as.matrix(as.list(test_pred_nn)),col="red",xlim=c(0,1),ylim=c(0,1))
# points(as.matrix(test_nn[,1]),as.matrix(as.list(test_pred_nn)),col="blue")
 
 #predict for target using glm
 train_store_target[,1]=0
 target_h2o=as.h2o(train_store_target[,1:16])
 target_pred_glm=h2o.predict(benchmark,target_h2o)
 prediction_backscale=as.numeric(as.matrix(as.list(target_pred_glm)))*as.numeric((maxs[1]-mins[1])+mins[1])
 #exponentiate
prediction_backscale=expm1(prediction_backscale)
finalsol=cbind(train_store_target,prediction_backscale)
 write.csv(finalsol,"D:/EPAM/prediction_final2.csv")