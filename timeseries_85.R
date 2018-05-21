library(xts)
library(forecast)
names(store_train)
store_sales_customers_type <- store_train[,c(1,2,7,11,12,13,14,15,16,17,18,20,22)]
names(store_sales_customers_type)
View(store_sales_customers_type)

store_sales_customers_type %>% filter(StoreType=="a") %>% group_by(Store) %>% summarise(n=n()) %>% arrange(desc(n)) #530
store_sales_customers_type %>% filter(StoreType=="b") %>% group_by(Store) %>% summarise(n=n()) %>% arrange(desc(n)) #85
store_sales_customers_type %>% filter(StoreType=="c") %>% group_by(Store) %>% summarise(n=n()) %>% arrange(desc(n)) #4
store_sales_customers_type %>% filter(StoreType=="d") %>% group_by(Store) %>% summarise(n=n()) %>% arrange(desc(n)) #578
#################################

store_85<-subset(store_sales_customers_type,Store==85)
names(store_85)
store_85$Sales<-ifelse(store_85$Sales==0,NA,store_85$Sales)
View(store_85)
summary(store_85$Sales)

#intitially set frequency to default value 1. plotted acf, showed strong weekly pattern
#so change the frequency to 7
store_85<-store_85[,c(5,6)]
store_85_ts <- ts(store_85[,-1], frequency = 7)
View(store_85_ts)
start(store_85_ts)
end(store_85_ts)
frequency(store_85_ts)
autoplot(store_85_ts)
acf2(store_85_ts)

#This is a simple odel with only heavy weekly seasonality and no other factors involved. 
#so let auto.arima pick the best model. the range of sales is high., so do log transformation
#fit ARIMA
fit_85<-auto.arima(store_85_ts,lambda=0)
fit_85
checkresiduals(fit_85)
fc<-forecast(fit_85,h=48)
autoplot(fc)

fit<-auto.arima(store_85_ts,xreg = fourier(store_85_ts,K=3),seasonal = FALSE)
fit
checkresiduals(fit)
fc<-forecast(fit,xreg = fourier(store_85_ts,K=3,h=48),h=48)
autoplot(fc)

tbats(store_85_ts)


