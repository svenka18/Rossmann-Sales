library(xts)
library(forecast)
names(store_train)
store_sales_customers_type <- store_train[,c(1,2,7,11,12,13,14,15,16,17,18,20,22)]
names(store_sales_customers_type)
View(store_sales_customers_type)

store_sales_customers_type %>% filter(StoreType=="c") %>% group_by(Store) %>% summarise(n=n()) %>% arrange(desc(n)) #4

###############store 530
store_578<-subset(store_sales_customers_type,Store==578)
names(store_578)
store_578$Sales<-ifelse(store_578$Sales==0,NA,store_578$Sales)
View(store_578)
summary(store_578$Sales)

#check for seasonality
yearly <- ts(store_578[,6], frequency=365)
fit_yearly <- tbats(yearly)
seasonal <- !is.null(fit_yearly$seasonal)
#yearly seasonality exists
weekly <- ts(store_578[,6], frequency=7)
fit_weekly <- tbats(weekly)
seasonal <- !is.null(fit_weekly$seasonal)
#weekly seasonality exists
autoplot(weekly)
acf2(weekly)
autoplot(yearly)

names(store_578)
store_578<-store_578[,c(5,6,9,4)]

#Sales are very high during monday when promotion is applied and very low on sundays when no promotion applied
#lets create dummies to include these effects in our model

store_578$day1<-ifelse(store_578$DayOfWeek==1,1,0)
store_578$day2<-ifelse(store_578$DayOfWeek==2,1,0)
store_578$day3<-ifelse(store_578$DayOfWeek==3,1,0)
store_578$day4<-ifelse(store_578$DayOfWeek==4,1,0)
store_578$day5<-ifelse(store_578$DayOfWeek==5,1,0)
store_578$day6<-ifelse(store_578$DayOfWeek==6,1,0)

#So, the model has multiple seasonality(weekly and yearly), also has day of week
# and promotional effect
#Hence, we fit a dynamic harmonic regression model with fourier terms for seasonalities
#and also include the dummmy variables as the predictors
names(store_578)
store_578_ts <- msts(store_578[,-1], seasonal.periods=c(7,365))
store_578_ts <- ts(store_578[,-1], frequency=7)
store_578_ts <- ts(store_578[,-1], frequency=365)

View(store_578_ts)
start(store_578_ts)
end(store_578_ts)
frequency(store_578_ts)
autoplot(store_578_ts[,5])
acf2(store_578_ts[,5])

#fit ARIMA
dummy<-cbind(Day1=store_578_ts[,"day1"],Day2=store_578_ts[,"day2"],Day3=store_578_ts[,"day3"],Day4=store_578_ts[,"day4"]
             ,Day5=store_578_ts[,"day5"],Day6=store_578_ts[,"day6"],promo=store_578_ts[,"Promo"])
xreg <- cbind(fourier(store_578_ts[,"Sales"], K = c(1,3)),dummy)
fit_578 <- auto.arima(store_578_ts[,"Sales"], xreg = xreg, lambda=0,seasonal = FALSE)
fit_578
checkresiduals(fit)

#forecasting sales
ross_test<-read.csv("C:/Users/saipr/Desktop/Sai Priya/kaggle/Rossmann/test.csv",stringsAsFactors = FALSE)
View(ross_test)
str(ross_test)
n_distinct(ross_test$Store) #856
ross_test$Date<-as.Date(ross_test$Date,format="%Y-%m-%d")
test_578<-subset(ross_test,Store==578)
dim(test_578)
day1<-ifelse(test_578$DayOfWeek==1,1,0)
day2<-ifelse(test_578$DayOfWeek==2,1,0)
day3<-ifelse(test_578$DayOfWeek==3,1,0)
day4<-ifelse(test_578$DayOfWeek==4,1,0)
day5<-ifelse(test_578$DayOfWeek==5,1,0)
day6<-ifelse(test_578$DayOfWeek==6,1,0)
promo_future<-test_578$Promo
dummy_test<-cbind(Day1=day1,Day2=day2,Day3=day3,Day4=day4,Day5=day5,Day6=day6,promo=promo_future)
new_xreg <- cbind(fourier(store_578_ts[,"Sales"], K = c(1,3),h=48),dummy_test)
fc<-forecast(fit_578,xreg=new_xreg)
autoplot(fc)
