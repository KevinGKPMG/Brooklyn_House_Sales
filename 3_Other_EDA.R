setwd("C:/Users/kevingregory/Desktop/ML Series/CaseComp1/Data")
#install.packages('ggmap')
#install.packages('randomForest')
library(ggmap)
library(magrittr)
library(dplyr)
library(ggplot2)
library(sqldf)
library(corrplot)


###############################################################################################################
###Initial Data Setup
###############################################################################################################
train <- readRDS('Brooklyn_House_Train.rds')
dim(train)
#Building Class
train<-subset(train,building_class_category=="01 ONE FAMILY HOMES" | building_class_category=="02 TWO FAMILY HOMES"| building_class_category=="03 THREE FAMILY HOMES")
train[is.na(train)] <- 0

#Price
train<- subset(train, train$sale_price<5000000)
train<- subset(train, train$sale_price>10000)

#Sqft
train<-subset(train, train$gross_sqft>0)

###############################################################################################################
###Further shrink training data
###############################################################################################################

train <- select(train, gross_sqft, year_built, sale_price, sale_date, year_of_sale, YearAlter1, YearAlter2,
                Landmark, GarageArea, LotArea,	BldgArea,	ComArea,	ResArea,	OfficeArea,	RetailArea,
                StrgeArea,	FactryArea,	OtherArea,	NumBldgs,	NumFloors,	UnitsRes,	UnitsTotal, HistDist,
                XCoord, YCoord, land_sqft)






###############################################################################################################
###Missing
###############################################################################################################

plot_Missing <- function(data_in, title = NULL){
  temp_df <- as.data.frame(ifelse(is.na(data_in), 0, 1))
  temp_df <- temp_df[,order(colSums(temp_df))]
  data_temp <- expand.grid(list(x = 1:nrow(temp_df), y = colnames(temp_df)))
  data_temp$m <- as.vector(as.matrix(temp_df))
  data_temp <- data.frame(x = unlist(data_temp$x), y = unlist(data_temp$y), m = unlist(data_temp$m))
  ggplot(data_temp) + geom_tile(aes(x=x, y=y, fill=factor(m))) + scale_fill_manual(values=c("white", "black"), name="Missing\n(0=Yes, 1=No)") + theme_light() + ylab("") + xlab("") + ggtitle(title)
}


plot_Missing(train[,colSums(is.na(train)) > 0])

###############################################################################################################
###Size variables
###############################################################################################################
size_vars <- select(train, gross_sqft, NumBldgs,	NumFloors,	UnitsRes,	UnitsTotal, land_sqft, LotArea, BldgArea)

size_vars[is.na(size_vars)] <- 0

M<-cor(size_vars)
M
corrplot(M, method = 'circle')

###############################################################################################################
###Dummy Train to play with
###############################################################################################################
train_eda<-train

###############################################################################################################
###GarageArea
###############################################################################################################
hist(train$GarageArea)
sample<-subset(train, train$GarageArea>0)
hist(sample$GarageArea)
dim(sample)

dummy_garage <-function(GarageArea){
  if (GarageArea>0){
    return(1)
  }
  else{
    return(0)
  }
}

train_eda$Dummy_Garage<-unlist(lapply(train_eda$GarageArea, dummy_garage))

barplot(train_eda$Dummy_Garage)

ggplot(train_eda, aes(x=as.factor(Dummy_Garage) )) + geom_bar()

sum(train_eda$Dummy_Garage)

#Only 60 places have a garage.....

###############################################################################################################
###Sale Price
###############################################################################################################
#Plot of sale price by month 
head(train_eda$sale_date)
aux<-as.Date(train_eda$sale_date[1], format="%Y-%m-%d")
format(aux, "%m")
train_eda$month<-format(as.Date(train_eda$sale_date, format="%Y-%m-%d"), "%m")
train_eda$year<-format(as.Date(train_eda$sale_date, format="%Y-%m-%d"), "%Y")
train_eda$day<-format(as.Date(train_eda$sale_date, format="%Y-%m-%d"), "%d")


x<-sqldf('select 
            avg(sale_price) as average_price,
            year,
            month
         from train_eda
         group by year, month')
head(x)

myDate<-NULL
for (i in seq(1:dim(x)[1])){
  myDate<-c(myDate, paste(x$year[i], x$month[i],sep=''))
}
myDate
x$myDate<-myDate





paste(x$year[1], x$month[1], sep='')

ggplot(x, aes(x=myDate, y=average_price)) + geom_point(color='steelblue')

#########################Reading in FRED
HPI<-read.csv('CSUSHPINSA.csv')
head(HPI)
