setwd("C:/Users/kevingregory/Desktop/ML Series/CaseComp1/Data")
library(magrittr)
library(ggmap)
#library(randomForest)

###############################################################################################################
###Initial Data Setup
###############################################################################################################
train <- readRDS('Brooklyn_House_Train.rds')
dim(train)
train<-subset(train,building_class_category=="01 ONE FAMILY HOMES" | building_class_category=="02 TWO FAMILY HOMES"| building_class_category=="03 THREE FAMILY HOMES")
dim(train)




###############################################################################################################
###Sale Price
###############################################################################################################

hist(train$sale_price)

######Look at only really large sales prices
train_large <- subset(train, train$sale_price > 5000000)
dim(train_large)
hist(train_large$sale_price)

nrow(train_large)/nrow(train)

#Thoughts: Remove sales price > 5M
#####################################

######Look at sales price between 10K and 5M
train_3=subset(train, train$sale_price>10000)
train_3=subset(train_3, train_3$sale_price<5000000)
dim(train_3)

hist(train_3$sale_price)
nrow(train_3)/nrow(train)

##############################
#Look at small prices
train_4=subset(train_3, train_3$sale_price< 20000)
hist(train_4$sale_price)

##################################
#Final thought: limit data to prices btw 10K and 5M

###############################################################################################################
###Square Footage
###############################################################################################################
hist(train_4$gross_sqft)

#Remove that pesky outlier
train_5<-subset(train_4, train_4$gross_sqft<8000)
hist(train_5$gross_sqft)

##############Look at small ones
train_6<-subset(train_5, train_5$gross_sqft<1000)
hist(train_6$gross_sqft)

train_7 <- subset(train_5, train_5$gross_sqft>0)
hist(train_7$gross_sqft)

###Thoughts: Eliminate sqft of 0













