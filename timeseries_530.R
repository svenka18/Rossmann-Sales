library(xts)
library(forecast)
names(store_train)
store_sales_customers_type <- store_train[,c(1,2,12,13,14,15,16)]
names(store_sales_customers_type)
View(store_sales_customers_type)

###############store 530
store_530<-subset(store_sales_customers_type,Store==530)
names(store_530)
store_530<-store_530[,c(3,4,7)]
store_530$Sales<-ifelse(store_530$Sales==0,NA,store_530$Sales)
View(store_530)
summary(store_530$Sales)

#check for yearly seasonality
yearly <- ts(store_530[,2], frequency=365)
fit_yearly <- tbats(yearly)
seasonal <- !is.null(fit$seasonal) 
#yearly seasonality exist
#check for weekly seasonality
weekly <- ts(store_530[,2], frequency=7)
fit_weekly <- tbats(weekly)
seasonal <- !is.null(fit$seasonal)
#weekly seasonality exists

#above results shows, there is both yearly and multiple seasonality
store_530_ts <- msts(store_530[,-1], seasonal.periods = c(7,365))
View(store_530_ts)
start(store_530_ts)
end(store_530_ts)
frequency(store_530_ts)
plot(store_530_ts[,1])
acf(store_530_ts)

#It looks like you have some high sales days, possibly associated with promotions.
promo.ts<- ts(store_530_ts,
              start=start(store_530_ts),frequency=frequency(store_530_ts))
View(promo.ts)
promo.ts[,"Sales"]<-ifelse(promo.ts[,"Promo"]==0,NA,promo.ts[,"Sales"])
plot(store_530_ts[,1])
points(promos.ts,pch=19,col="red")

#So, the model has multiple seasonality(weekly and yearly), also has promotional effect
#Hence, we fit a dynamic harmonic regression model with fourier terms for seasonalities
#and also the "Promo" variable as one of the predictors
xreg <- cbind(fourier(store_530_ts[,"Sales"], K = c(3,150)),promo=store_530_ts[,"Promo"])
#fit ARIMA
fit_530<-auto.arima(store_530_ts[,"Sales"],xreg = xreg,seasonal = FALSE,lambda=0)
fit_530
checkresiduals(fit_530)

#forecasting sales
ross_test<-read.csv("C:/Users/saipr/Desktop/Sai Priya/kaggle/Rossmann/test.csv",stringsAsFactors = FALSE)
test_538<-subset(ross_test,Store==530)
promo_future<-test_538$Promo
new_xreg <- cbind(fourier(store_530_ts[,"Sales"], K = c(3,150),h=48),promo=promo_future)
fc<-forecast(fit_530,xreg=new_xreg,h=48)
autoplot(fc)



