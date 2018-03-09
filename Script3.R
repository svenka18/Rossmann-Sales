library(rpart)
library(Metrics)

######################## Linear Regression
mod <-lm(Customers~.-Store -Date -Sales,train[,-c(6,17)])
summary(mod) #RSquared only 32%
p<-predict(mod,test)
length(p)
mod1<-augment(mod)
View(mod1)
rmse(test$Customers,p) #rmse = 330.4204

mod <-lm(Sales~.-Store -Date,train[,-6])
summary(mod) #RSquared 83%
p<-predict(mod,validation)
length(p)
mod1<-augment(mod)
View(mod1)


#PromoInterval- No Promo Applied coefficient is NA! lets do a pairwaise t-test 
pairwise.t.test(training$Sales,training$PromoInterval,p.adjust="none") %>% tidy()
store_train%>%group_by(PromoInterval)%>%summarise(n())

sapply(train,function(x) sum(is.na(x)))
Store %>% filter(is.na(Promo2SinceWeek)) %>% group_by(Store,Promo2) %>% summarise(m=n()) %>% filter(m>1)
Store %>% filter(is.na(Promo2SinceWeek)) %>% filter(Promo2==1)

############Splitting
sample_rows<-sample(nrow(training),nrow(training)*0.75)
test<-training[-sample_rows,]
train<-training[sample_rows,]

############################### Regression Trees
library(rpart)
names(train)
reg_tree<-rpart(Customers~.-Store -Date -Sales -Open -CompetitionDistance_minmax,train,method = "anova")
reg_tree<-rpart(Customers~DayOfWeek+Promo+CompetitorType+CompetitorDistanceCategory+Promo2Period+PromoInterval+Promo2
                +Assortment+StoreType+SchoolHoliday+StateHoliday,train,method = "anova")
summary(reg_tree)
pred<-predict(reg_tree,test)
rmse(test$Customers,pred) #rmse = 344.5733

############################Grid Search for regression trees
# Establish a list of possible values for minsplit and maxdepth
minsplit <- seq(1, 4, 1)
maxdepth <- seq(1, 6, 1)

# Create a data frame containing all combinations 
hyper_grid <- expand.grid(minsplit = minsplit, maxdepth = maxdepth)

#create an empty list to save results
models <- list()

# Write a loop over the rows of hyper_grid to train the grid of models
for (i in 1:nrow(hyper_grid)) {
  
  # Get minsplit, maxdepth values at row i
  minsplit <- hyper_grid$minsplit[i]
  maxdepth <- hyper_grid$maxdepth[i]
  
  # Train a model and store in the list
  models[[i]] <- rpart(formula = Customers~DayOfWeek+Promo+CompetitorType+CompetitorDistanceCategory+Promo2Period+PromoInterval+Promo2
                             +Assortment+StoreType+SchoolHoliday+StateHoliday, 
                             data = train, 
                             method = "anova",
                             minsplit = minsplit,
                             maxdepth = maxdepth)
}

# Create an empty vector to store RMSE values
rmse <- c()

# Write a loop over the models to compute validation RMSE
for (i in 1:length(models)) {
  # Retreive the i^th model from the list
model <- models[[i]]
# Generate predictions on grade_valid 
pred <- predict(object = model,newdata = test)
# Compute validation RMSE and add to the 
rmse_values[i] <- rmse(actual = test$Customers, 
                         predicted = pred)
}