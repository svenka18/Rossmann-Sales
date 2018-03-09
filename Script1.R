require(dplyr)
library(ggplot2)
require(ggplot2)
library(tidyr)
require(tidyr)
library(lubridate)
require(lubridate)
library(zoo)
require(zoo)
# Train Data
ross_train<-read.csv("C:/Users/saipr/Desktop/Sai Priya/kaggle/Rossmann/train.csv",stringsAsFactors = FALSE)
dim(ross_train)
str(ross_train)

#converting datatypes
ross_train$Date<-as.Date(ross_train$Date,format="%Y-%m-%d")
#Sales,customers are integer? must be numeric
ross_train$Sales<-as.numeric(ross_train$Sales)
ross_train$Customers<-as.numeric(ross_train$Customers)

#no missing values
sapply(ross_train,function(x) sum(is.na(x)))

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

#check if all the stores in store has data in train
n_distinct(ross_train$Store) #1115
n_distinct(Store$Store) #1115
setdiff(Store$Store,ross_train$Store)
setdiff(ross_train$Store,Store$Store)
ross_train$Store[!(ross_train$Store %in% Store$Store)]

##########################


#joining the two datasets
store_train <- inner_join(ross_train,Store,by="Store")
dim(store_train)
str(store_train)
sapply(store_train,function(x) sum(is.na(x)))
View(store_train)

#no of years to current date
store_train$CompetitionOpenSinceYears <-  as.yearmon(store_train$Date) - store_train$CompetitionOpenSince
#no of days to current date
store_train$Promo2StartSinceDays <-  as.Date(store_train$Date, format = "%Y-%m-%d") - as.Date(store_train$Promo2Since,format="%Y-%m-%d")
names(store_train)
store_train<-store_train[,-c(13,14,16,17,19,20)]

########################## Data Cleaning

store_train$DayOfWeek<-as.factor(store_train$DayOfWeek)
levels(store_train$DayOfWeek) <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")

store_train$CompetitionOpenSinceYears<-round(store_train$CompetitionOpenSinceYears)
#there are stores where promo started after the current date. that will not have any impact on the sales.
#so lets make it 0
i<-which(store_train$Promo2StartSinceDays<0)
store_train$Promo2[i]<-0
store_train$PromoInterval<-as.character(store_train$PromoInterval)
store_train$PromoInterval[i]<-"No Promo Applied"
store_train$Promo2StartSinceDays[i]<-0

#competitors opened after current date, replace with Nas
j<-which(store_train$CompetitionOpenSinceYears<0)
store_train$CompetitionOpenSinceYears[j]<-NA
store_train$CompetitionDistance[j]<-NA

#is the sales > 0 when the store is not open, if yes then it should be made 0 : fortunately no!
store_train %>% filter(Open==0 & Sales>0)
#is the customers > 0 when the store is not open, if yes then it should be made 0 : fortunately no!
store_train %>% filter(Open==0 & Customers>0)
#Store is open, but 0 Customers?? returns 52 rows ########################## to be checked
store_train %>% filter(Open==1 & Customers==0)
#Is competition distance 0 for any open stores, no
store_train %>% filter(Open==1 & CompetitionDistance==0)


#store id is duplicated, but by any chance is the combination of Store/Date duplicated? : fortunately no!
store_train %>% group_by(Store,Date) %>% summarise(c=n()) %>% filter(c>1)



############ lets deal with missing values now
sapply(store_train,function(x) sum(is.na(x)))

#Promo2Sinceweek/year
#check if its missing only where promo2=0, if yes then it makes sense
Store %>% filter(Promo2==0) %>% select(Promo2SinceWeek,Promo2SinceYear) %>% nrow() #awesome!544 rows exactly
Store %>% filter(Promo2==0) %>% select(Promo2SinceWeek,Promo2SinceYear) %>% filter(!is.na(Promo2SinceWeek)| !is.na(Promo2SinceYear)) #returns 0 rows, all are Nas

#for simplicity lets replace the NAs with "No promotion applied"
store_train$PromoInterval<-ifelse(is.na(store_train$PromoInterval),"No Promo Applied",store_train$PromoInterval)
store_train$PromoInterval<-as.factor(store_train$PromoInterval)
store_train$Promo2StartSinceDays<-ifelse(is.na(store_train$Promo2StartSinceDays),0,store_train$Promo2StartSinceDays)


################################## Lets examine individual variables for possible outliers


#but first lets remove rows where open=0
store_train_open<- store_train %>% filter(Open==1)

#Store Type
ggplot(store_train_open,aes(x=StoreType)) + geom_bar()

#Day of Week : very few stores are open on sunday, Outlier?? no lets keep it
ggplot(store_train_open,aes(x=DayOfWeek)) + geom_bar()

#Promo1
ggplot(store_train_open,aes(x=Promo)) + geom_bar()

#StateHoliday: indicates very few stores are open on holidays a,b,c
ggplot(store_train_open,aes(x=StateHoliday)) + geom_bar()

#SchoolHoliday: very few stores are open on school holidays
ggplot(store_train_open,aes(x=SchoolHoliday)) + geom_bar()

#StoreType: very few stores are of type "b", outlier?? no lets keep it
ggplot(store_train,aes(x=StoreType)) + geom_bar()

#Assortment; again very few assortment b's. outlier??
ggplot(store_train,aes(x=Assortment)) + geom_bar()

#Promo2
ggplot(store_train_open,aes(x=Promo2)) + geom_bar()

#Customers
ggplot(store_train_open,aes(x=Customers)) + geom_histogram()
ggplot(store_train_open,aes(x=Customers)) + geom_density() #a little right skewed
ggplot(store_train_open,aes(x=1,y=Customers)) + geom_boxplot()
#there is an outler with customers > 6000 
store_train %>% filter(Customers>6000) #returns only one row, but lets not remove it now
#lets remove it after we construct models
store_train<-store_train %>% filter(Customers<=6000)
store_train_open<-store_train_open %>% filter(Customers<=6000)
#IQR
summary(store_train$Customers)
IQR<-IQR(store_train$Customers)
lowerquar <- 405 - 1.5; lowerquar
upperquar <- 837 + 1.5; upperquar

#Competitiondistance
ggplot(store_train_open,aes(x=CompetitionDistance)) + geom_histogram() #distribution is right skewed 
ggplot(store_train_open,aes(x=CompetitionDistance)) + geom_density() #distribution is right skewed 
ggplot(store_train_open,aes(x=1,y=CompetitionDistance)) + geom_boxplot()
#though distance is not 0, when store is open, some competitors are very near(less than 0.5 meters), 
#those stores need additional analysis
#there are outliers too, distance > 60000 meters, but only 1 store is like that
store_train_open %>% filter(CompetitionDistance>60000) %>% summarise(n_distinct(Store))
#if we remove then we lose the entire information for this store, so better to apply min-max normalisation
#but lets do a modela dn then will see

#Sales
ggplot(store_train_open,aes(x=Sales)) + geom_histogram()  
ggplot(store_train_open,aes(x=Sales)) + geom_density() #distribution is right skewed 
ggplot(store_train_open,aes(x=1,y=Sales)) + geom_boxplot()
#there are stores with sales more than 40000, but thats ok, lets keep it
#and there are open stores with 0 sales?

#Competition open since
ggplot(store_train_open,aes(x=CompetitionOpenSinceYears)) + geom_histogram()
ggplot(store_train_open,aes(x=CompetitionOpenSinceYears)) + geom_density()
ggplot(store_train,aes(x=1,y=CompetitionOpenSinceYears))+geom_boxplot()
#there are some competitiors opened since 50years

#Promo2 since
ggplot(store_train_open,aes(x=Promo2StartSinceDays)) + geom_histogram()
ggplot(store_train_open,aes(x=Promo2StartSinceDays)) + geom_density() #heavily skewed
ggplot(store_train%>%filter(Promo2StartSinceDays>0),aes(x=1,y=Promo2StartSinceDays))+geom_boxplot() #to be normalized

#Promo2 intervals
table(store_train_open$PromoInterval)
ggplot(store_train_open,aes(x=PromoInterval)) + geom_bar()


#################################### Effect of variables on Sales


############ Store Type
#no of stores in each type: "a" highest. "b" lowest
store_train %>% group_by(StoreType) %>% summarise(n_distinct(Store)) 

#which store type has the highest sales: type a
store_train %>% group_by(StoreType) %>% summarise(sales=sum(Sales)) %>% mutate(rnk=rank(desc(sales))) %>% filter(rnk==1)

#which store type has the largest no of customers : type a
store_train %>% group_by(StoreType) %>% summarise(cust=sum(Customers)) %>% mutate(rnk=rank(desc(cust))) %>% filter(rnk==1)

ggplot(store_train_open,aes(x=Sales,fill=StoreType)) + geom_density(alpha=0.5) 

############# Effect of Promo 1

#lets analyse the store that has the highest no of records.
store_train %>% group_by(Store) %>% summarise(n=n()) %>% arrange(desc(n))

#many stores have the max count, lets pick one : store 1 and compare the sales when promo 1 was applied and not applied
store_train %>% filter(Store==1) %>% group_by(Promo) %>% summarise(tot_sales=sum(Sales)) 

#looks like sales are more when promo=0, but is this the case for all the stores? lets compare the overall sales for all stores
store_train %>% group_by(Promo) %>% summarise(tot_sales=sum(Sales)) %>% arrange(desc(tot_sales))
#Sales is more when promo1 is applied

ggplot(store_train_open,aes(x=Sales))+geom_histogram()+facet_wrap(~Promo)
ggplot(store_train_open,aes(x=Sales,fill=as.factor(Promo)))+geom_density(alpha=0.9)
#density plot also shows High sales occurs when promo1 is applied

#################### Effect of Promo2
#Does all the stores in the datatset has promo2 appplied from the first record?
promo0<-store_train %>% filter(Promo2==0) %>% group_by(Store) %>% summarise(n1=n())
promo0 %>% full_join(store_train %>% filter(Promo2==1) %>% group_by(Store) %>% summarise(n2=n()),by="Store") %>% filter(!is.na(n1) & !is.na(n2))
#yes, none of the store has both the records(0&1) 

store_train %>% group_by(Promo2) %>% summarise(tot_sales=sum(Sales)) %>% arrange(desc(tot_sales))
#Promo2 when appied has less sales

ggplot(store_train_open,aes(x=Sales,fill=as.factor(Promo2))) + geom_density(alpha=1)
#Promo2 when appied has less sales

#################### Effect of School Holiday

#does more sales happen when there is a school holiday: YES!!, the difference is huge
store_train %>% group_by(SchoolHoliday) %>% summarise(tot_sales=sum(Sales)) %>% arrange(desc(tot_sales))
ggplot(store_train_open,aes(x=Sales,fill=as.factor(SchoolHoliday))) + geom_density(alpha=0.8)

#################### Effect of State Holiday

#how does sales differ when there is a state holiday:  
store_train %>% group_by(StateHoliday) %>% summarise(tot_sales=sum(Sales)) %>% arrange(desc(tot_sales))

ggplot(store_train_open,aes(x=Sales,fill=as.factor(StateHoliday))) + geom_density(alpha=0.8)
#sales is more when there is no state holiday, but
#among all state holidays, sales is more on public holidays and less on day of christmas

################### Customers

#lets see the relationship btw customers and sales
cor(store_train_open$Customers,store_train_open$Sales)
#0.82..it means that as customers increase sales increase
ggplot(store_train_open,aes(x=Customers,y=Sales)) +geom_point()

###################CompetitionDistance

##lets see the relationship btw CompetitionDistance and sales
#for now lets remove the records with missing distance
store_train1<-store_train_open %>% filter(!is.na(CompetitionDistance)) 
cor(store_train1$CompetitionDistance,store_train1$Sales)
#-0.03 looks liek the relationship is negative and very week. lets plot it
ggplot(store_train_open,aes(x=CompetitionDistance,y=Sales)) +geom_point()

#yes the plot is very poor, no corellation btw distance and sales
#sales decrease as the distance increases

###################DayOfWEEk

#which day of week has more sales? High sales on monday, low on sunday
store_train %>% group_by(DayOfWeek) %>% summarise(tot_sales=sum(Sales)) %>% arrange(desc(tot_sales))
ggplot(store_train_open,aes(x=Sales,fill=DayOfWeek)) + geom_density(alpha=0.8)

################### PromoInterval: Jan,Apr,Jul,Oct has the highest
store_train %>% group_by(PromoInterval) %>% summarise(tot_sales=sum(Sales)) %>% arrange(desc(tot_sales))
ggplot(store_train_open,aes(x=Sales,fill=PromoInterval)) + geom_density(alpha=0.8)


#################### Competition open since
cor(!is.na(store_train_open$CompetitionOpenSinceYears),store_train_open$Sales) #very loww
ggplot(store_train,aes(x=CompetitionOpenSinceYears,y=Sales))+geom_point()
#sales decrease as the years increases
#looks like competitiors has an positive effect on sales in the early stages but fades as the time passes
#may the competitior started to perform better

#################### Promo2 since
cor(store_train$Promo2StartSinceDays,store_train$Sales) #very loww, negative
ggplot(store_train,aes(x=Promo2StartSinceDays,y=Sales))+geom_point()
ggplot(store_train,aes(x=Promo2StartSinceDays,y=Sales))+geom_boxplot()
#there seems to be no offect of promo2 on sales over the days
cor(!is.na(store_train$CompetitionOpenSinceYears),!is.na(store_train$CompetitionDistance))

################################# BINNING
store_train$CompetitorType[store_train$CompetitionOpenSinceYears<=5 & store_train$CompetitionOpenSinceYears>=0]<-"New"
store_train$CompetitorType[store_train$CompetitionOpenSinceYears>5 & store_train$CompetitionOpenSinceYears<=15]<-"Middle"
store_train$CompetitorType[store_train$CompetitionOpenSinceYears>15 & store_train$CompetitionOpenSinceYears<=45]<-"Old"
store_train$CompetitorType[store_train$CompetitionOpenSinceYears>45]<-"Very Old"
store_train$CompetitorType[is.na(store_train$CompetitionOpenSinceYears)]<-"Competitor Not Available Yet"

store_train$CompetitorDistanceCategory[store_train$CompetitionDistance<=5000]<-"Very Near"
store_train$CompetitorDistanceCategory[store_train$CompetitionDistance>5000 & store_train$CompetitionDistance<=10000]<-"Near"
store_train$CompetitorDistanceCategory[store_train$CompetitionDistance>10000 & store_train$CompetitionDistance<=20000]<-"Middle"
store_train$CompetitorDistanceCategory[store_train$CompetitionDistance>20000 & store_train$CompetitionDistance<=40000]<-"Far"
store_train$CompetitorDistanceCategory[store_train$CompetitionDistance>40000]<-"Very Far"
store_train$CompetitorDistanceCategory[is.na(store_train$CompetitionDistance)]<-"Competitor Not Available Yet"


store_train$Promo2Period[store_train$Promo2StartSinceDays<=180 & store_train$Promo2StartSinceDays>0]<-"New"
store_train$Promo2Period[store_train$Promo2StartSinceDays>180 & store_train$Promo2StartSinceDays<=800]<-"Middle"
store_train$Promo2Period[store_train$Promo2StartSinceDays>800 & store_train$Promo2StartSinceDays<=1500]<-"Old"
store_train$Promo2Period[store_train$Promo2StartSinceDays>1500]<-"Very Old"
store_train$Promo2Period[store_train$Promo2StartSinceDays==0]<-"No Promo Applied"
str(store_train)
store_train$Open<-as.factor(store_train$Open)
store_train$Promo<-as.factor(store_train$Promo)
store_train$StateHoliday<-as.factor(store_train$StateHoliday)
store_train$SchoolHoliday<-as.factor(store_train$SchoolHoliday)
store_train$Promo2<-as.factor(store_train$Promo2)
store_train$CompetitorType<-as.factor(store_train$CompetitorType)
store_train$Promo2Period<-as.factor(store_train$Promo2Period)
store_train$CompetitorDistanceCategory<-as.factor(store_train$CompetitorDistanceCategory)


############################### perform ANOVA to test if the binning was valid or not
store_train %>% group_by(Promo2Period) %>% summarise(sd=sd(Sales),mean=mean(Sales))
store_train %>% group_by(CompetitorType) %>% summarise(sd=sd(Sales),mean=mean(Sales))
library(broom)
a1<-aov(Sales~Promo2Period,store_train)
a2<-aov(Sales~CompetitorType,store_train)
summary(a1)
summary(a2)

################################ Normalizations
str(store_train)
store_train$CompetitionDistance_minmax<-(store_train$CompetitionDistance-min(store_train$CompetitionDistance,na.rm = TRUE))/diff(range(store_train$CompetitionDistance,na.rm = TRUE))
store_train_open<- store_train %>% filter(Open==1)

#Looks like thae partition is valid
#################### Modelling

#lets create a new df with the redundant columns removed
names(store_train_open)
training<-store_train_open[,-c(15,16,12)]
#training1<-store_train_open[,-c(17,18)]

sapply(training,function(x) sum(is.na(x)))

