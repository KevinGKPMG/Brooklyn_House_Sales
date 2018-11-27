setwd("C:/Users/kevingregory/Desktop/ML Series/CaseComp1/Data")
#install.packages('ggmap')
#install.packages('randomForest')
library(ggmap)
library(magrittr)
library(dplyr)
library(ggplot2)


###############################################################################################################
###Initial Data Setup
###############################################################################################################
train <- readRDS('Brooklyn_House_Train.rds')
dim(train)
#Building Class
train<-subset(train,building_class_category=="01 ONE FAMILY HOMES" | building_class_category=="02 TWO FAMILY HOMES"| building_class_category=="03 THREE FAMILY HOMES")

#Price
train<- subset(train, train$sale_price<5000000)
train<- subset(train, train$sale_price<10000)

#Sqft
train<-subset(train, train$gross_sqft>0)

###############################################################################################################
###Neighborhood
###############################################################################################################
unique(train$neighborhood)

train %>% select(neighborhood, sale_price) %>% ggplot(aes(factor(neighborhood), sale_price)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust =1)) + xlab('Neighborhoods')

















