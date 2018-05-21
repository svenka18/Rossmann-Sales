library(xts)
library(forecast)
names(store_train)
View(store_train)
store_sales_customers_type <- store_train[,c(1,2,7,11,12,13,14,15,16,17,18,20,22)]
names(store_sales_customers_type)
View(store_sales_customers_type)

store_sales_customers_type %>% filter(StoreType=="c") %>% group_by(Store) %>% summarise(n=n()) %>% arrange(desc(n)) #4

###############store 530
store_1<-subset(store_sales_customers_type,Store==1)
names(store_1)
View(store_1)
################Inclusion of dummies to accomodate end of year peaks every year
# store_1$Jan<-ifelse(store_1$Month==1,1,0)
# store_1$Feb<-ifelse(store_1$Month==2,1,0)
# store_1$Mar<-ifelse(store_1$Month==3,1,0)
# store_1$Apr<-ifelse(store_1$Month==4,1,0)
# store_1$May<-ifelse(store_1$Month==5,1,0)
# store_1$Jun<-ifelse(store_1$Month==6,1,0)
# store_1$Jul<-ifelse(store_1$Month==7,1,0)
# store_1$Aug<-ifelse(store_1$Month==8,1,0)
# store_1$Sep<-ifelse(store_1$Month==9,1,0)
# store_1$Oct<-ifelse(store_1$Month==10,1,0)
# store_1$Nov<-ifelse(store_1$Month==11,1,0)
# store_1$Dec<-NULL

store_1$week50<-ifelse(store_1$WeekOfYear==50,1,0)
store_1$week51<-ifelse(store_1$WeekOfYear==51,1,0)

names(store_1)
store_1<-store_1[,c(5,6,9,14:15)]

#replacing 0 sales with NA's
store_1$Sales<-ifelse(store_1$Sales==0,NA,store_1$Sales)
View(store_1)
summary(store_1$Sales)

#check for seasonality
yearly <- ts(store_1[,2], frequency=365)
fit_yearly <- tbats(yearly)
seasonal <- !is.null(fit_yearly$seasonal)
#yearly seasonality exists
weekly <- ts(store_1[,2], frequency=7)
fit_weekly <- tbats(weekly)
seasonal <- !is.null(fit_weekly$seasonal)
#weekly seasonality exists

store_1_xts <- xts(store_1[,2], order.by=as.Date(store_1[,1], "%Y-%m-%d"))
autoplot(store_1_xts)

#above results shows, there is both yearly and multiple seasonality
names(store_1)
store_1_ts <- msts(store_1[,-1], seasonal.periods=c(7,365))
store_1_ts <- ts(store_1[,-1], frequency=365)
View(store_1_ts)
start(store_1_ts)
end(store_1_ts)
frequency(store_1_ts)
autoplot(store_1_ts[,1])

#So, the model has multiple seasonality(weekly and yearly), also has end of year (christmas,ne year efect)
# and promotional effect
#Hence, we fit a dynamic harmonic regression model with fourier terms for seasonalities
#and also include the dummmy variables as the predictors
# dummy<-cbind(Jan=store_1_ts[,"Jan"],Feb=store_1_ts[,"Feb"],Mar=store_1_ts[,"Mar"],Apr=store_1_ts[,"Apr"],May=store_1_ts[,"May"],Jun=store_1_ts[,"Jun"],Jul=store_1_ts[,"Jul"]
#               ,Aug=store_1_ts[,"Aug"],Sep=store_1_ts[,"Sep"],Oct=store_1_ts[,"Oct"],Nov=store_1_ts[,"Nov"],
#               week50=store_1_ts[,"week50"],week51=store_1_ts[,"week51"],week52=store_1_ts[,"week52"],promo=store_1_ts[,"Promo"])

xreg <- cbind(fourier(store_1_ts[,"Sales"], K = c(1,3)),week50=store_1_ts[,"week50"],week51=store_1_ts[,"week51"],promo=store_1_ts[,"Promo"])
#fit ARIMA
fit_1<-auto.arima(store_1_ts[,"Sales"],xreg = xreg,seasonal = FALSE)
fit_1
checkresiduals(fit_1)

#forecasting sales
ross_test<-read.csv("C:/Users/saipr/Desktop/Sai Priya/kaggle/Rossmann/test.csv",stringsAsFactors = FALSE)
View(ross_test)
str(ross_test)
n_distinct(ross_test$Store) #856
ross_test$Date<-as.Date(ross_test$Date,format="%Y-%m-%d")
test_1<-subset(ross_test,Store==1)
dim(test_1)
test_1$WeekOfYear<-week(test_1$Date)
test_1$week50<-ifelse(test_1$WeekOfYear==50,1,0)
test_1$week51<-ifelse(test_1$WeekOfYear==51,1,0)
test_1$week52<-ifelse(test_1$WeekOfYear==52,1,0)
week50<-test_1$week50
week51<-test_1$week51
week52<-test_1$week52
promo_future<-test_1$Promo

new_xreg <- cbind(fourier(store_1_ts[,"Sales"], K = c(1,3),h=48),week50=week50,week51=week51,promo=promo_future)
fc<-forecast(fit_1,xreg=new_xreg)
autoplot(fc)

