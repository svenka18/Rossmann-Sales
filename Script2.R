require(dplyr)
library(ggplot2)
require(ggplot2)
library(tidyr)
require(tidyr)
library(lubridate)
require(lubridate)
library(zoo)
require(zoo)
# test Data
ross_test<-read.csv("C:/Users/saipr/Desktop/Sai Priya/kaggle/Rossmann/test.csv",stringsAsFactors = FALSE)
dim(ross_test)
str(ross_test)
View(ross_test)
#converting datatypes
ross_test$Date<-as.Date(ross_test$Date,format="%Y-%m-%d")

#no missing values
sapply(ross_test,function(x) sum(is.na(x)))

#####################

#Store data
Store<-read.csv("C:/Users/saipr/Desktop/Sai Priya/kaggle/Rossmann/store.csv",na.strings=c("","NA"))
dim(Store)
str(Store)

#converting datatypes
Store$CompetitionDistance<-as.numeric(Store$CompetitionDistance)

View(Store)
# there are few missing values,but we will deal with it later
sapply(Store,function(x) sum(is.na(x)))

#lets convert it into a single variable that contains the total no of months
#assume that competition was open on 1st of month
Store$CompetitionOpenSince<-as.yearmon(as.Date(paste(Store$CompetitionOpenSinceYear,Store$CompetitionOpenSinceMonth,1,sep = "-"),format="%Y-%m-%d"))
#assume that promo starts on day 1 of the week
Store$Promo2Since<- as.POSIXct(paste(Store$Promo2SinceYear,Store$Promo2SinceWeek, 1, sep = "-"),format = "%Y-%U-%u")


##########################

#check if all the stores in store has data in test
n_distinct(ross_test$Store) #856
n_distinct(Store$Store) #1115
setdiff(Store$Store,ross_test$Store)
setdiff(ross_test$Store,Store$Store)
ross_test$Store[!(ross_test$Store %in% Store$Store)]

##########################


#joining the two datasets
store_test <- left_join(ross_test,Store,by="Store")
dim(store_test)
str(store_test)
sapply(store_test,function(x) sum(is.na(x)))
View(store_test)

#no of years to current date
store_test$CompetitionOpenSinceYears <-  as.yearmon(store_test$Date) - store_test$CompetitionOpenSince
#no of days to current date
store_test$Promo2StartSinceDays <-  as.Date(store_test$Date, format = "%Y-%m-%d") - as.Date(store_test$Promo2Since,format="%Y-%m-%d")
names(store_test)
store_test<-store_test[,-c(12,13,15,16,18,19)]
View(store_test)
str(store_test)

########################## Data Cleaning

store_test$DayOfWeek<-as.factor(store_test$DayOfWeek)
levels(store_test$DayOfWeek) <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")

store_test$CompetitionOpenSinceYears<-round(store_test$CompetitionOpenSinceYears)

#is the sales > 0 when the store is not open, if yes then it should be made 0 : fortunately no!
store_test %>% filter(Open==0 & Sales>0)
#is the customers > 0 when the store is not open, if yes then it should be made 0 : fortunately no!
store_test %>% filter(Open==0 & Customers>0)
#Store is open, but 0 Customers?? returns 52 rows ########################## to be checked
store_test %>% filter(Open==1 & Customers==0)
#Is competition distance 0 for any open stores, no
store_test %>% filter(Open==1 & CompetitionDistance==0)


#store id is duplicated, but by any chance is the combination of Store/Date duplicated? : fortunately no!
store_test %>% group_by(Store,Date) %>% summarise(c=n()) %>% filter(c>1)



############ lets deal with missing values now
sapply(store_test,function(x) sum(is.na(x)))

#for simplicity lets replace the NAs with "No promotion applied"
store_test$PromoInterval<-as.character(store_test$PromoInterval)
store_test$PromoInterval<-ifelse(is.na(store_test$PromoInterval),"No Promo Applied",store_test$PromoInterval)
store_test$PromoInterval<-as.factor(store_test$PromoInterval)
store_test$Promo2StartSinceDays<-ifelse(is.na(store_test$Promo2StartSinceDays),0,store_test$Promo2StartSinceDays)

store_test %>% filter(is.na(Open)) %>% select(DayOfWeek,StateHoliday)
#looks like all the days fall on week days and there is no state holidays on these days
#lets replace them with 1
store_test$Open<-ifelse(is.na(store_test$Open),1,store_test$Open)

################################# BINNING
store_test$CompetitorType[store_test$CompetitionOpenSinceYears<=5 & store_test$CompetitionOpenSinceYears>=0]<-"New"
store_test$CompetitorType[store_test$CompetitionOpenSinceYears>5 & store_test$CompetitionOpenSinceYears<=15]<-"Middle"
store_test$CompetitorType[store_test$CompetitionOpenSinceYears>15 & store_test$CompetitionOpenSinceYears<=45]<-"Old"
store_test$CompetitorType[store_test$CompetitionOpenSinceYears>45]<-"Very Old"
store_test$CompetitorType[is.na(store_test$CompetitionOpenSinceYears)]<-"Competitor Not Available Yet"

store_test$Promo2Period[store_test$Promo2StartSinceDays<=180 & store_test$Promo2StartSinceDays>0]<-"New"
store_test$Promo2Period[store_test$Promo2StartSinceDays>180 & store_test$Promo2StartSinceDays<=800]<-"Middle"
store_test$Promo2Period[store_test$Promo2StartSinceDays>800 & store_test$Promo2StartSinceDays<=1500]<-"Old"
store_test$Promo2Period[store_test$Promo2StartSinceDays>1500]<-"Very Old"
store_test$Promo2Period[store_test$Promo2StartSinceDays==0]<-"No Promo Applied"


store_test$CompetitorDistanceCategory[store_test$CompetitionDistance<=5000]<-"Very Near"
store_test$CompetitorDistanceCategory[store_test$CompetitionDistance>5000 & store_test$CompetitionDistance<=10000]<-"Near"
store_test$CompetitorDistanceCategory[store_test$CompetitionDistance>10000 & store_test$CompetitionDistance<=20000]<-"Middle"
store_test$CompetitorDistanceCategory[store_test$CompetitionDistance>20000 & store_test$CompetitionDistance<=40000]<-"Far"
store_test$CompetitorDistanceCategory[store_test$CompetitionDistance>40000]<-"Very Far"
store_test$CompetitorDistanceCategory[is.na(store_test$CompetitionDistance)]<-"Competitor Not Available Yet"

str(store_test)
store_test$Open<-as.factor(store_test$Open)
store_test$Promo<-as.factor(store_test$Promo)
store_test$StateHoliday<-as.factor(store_test$StateHoliday)
store_test$SchoolHoliday<-as.factor(store_test$SchoolHoliday)
store_test$Promo2<-as.factor(store_test$Promo2)
store_test$CompetitorType<-as.factor(store_test$CompetitorType)
store_test$Promo2Period<-as.factor(store_test$Promo2Period)
store_test$CompetitorDistanceCategory<-as.factor(store_test$CompetitorDistanceCategory)

sapply(store_test,function(x) sum(is.na(x)))

store_test_open<- store_test %>% filter(Open==1)

#################### Modelling

#lets create a new df with the redundant columns removed
names(store_test_open)
testing<-store_test_open[,-c(11,14,15)]




View(store_test_open)

