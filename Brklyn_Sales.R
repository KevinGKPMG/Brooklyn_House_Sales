setwd("C:/Users/kevingregory/Desktop/ML Series/CaseComp1/Data")
#install.packages('ggmap')
#install.packages('randomForest')
library(ggmap)
library(randomForest)

train <- readRDS('Brooklyn_House_Train.rds')
dim(train)
train<-subset(train,building_class_category=="01 ONE FAMILY HOMES" | building_class_category=="02 TWO FAMILY HOMES"| building_class_category=="03 THREE FAMILY HOMES")
dim(train)
train<-subset(train, train$sale_price > 50000)
train<-subset(train, train$gross_sqft > 1000)
dim(train)

colnames(train)
summary(train$neighborhood)

##########################super train to play around
small_train=train[sample(nrow(train), 1000),]
small_train$address[1]
get_map(location="houston", api_key="AIzaSyD9E5bV-r3el3y-T2fPhUhwodJkbdzcIJw")

#hold text

mod1<-lm(sale_price~gross_sqft+neighborhood + gross_sqft*neighborhood, data=train)
summary(mod1)


hist(train$sale_price)
hist(train$gross_sqft)

summary(train$sale_price)
summary(train$gross_sqft)

plot(train$sale_price~train$gross_sqft)
##########################
small_train<-subset(train, train$gross_sqft<5000)
hist(small_train$gross_sqft)

#ideas..2 models? one for small and one for big?
mod1<-lm(sale_price~gross_sqft+neighborhood + gross_sqft*neighborhood, data=small_train)
summary(mod1)












