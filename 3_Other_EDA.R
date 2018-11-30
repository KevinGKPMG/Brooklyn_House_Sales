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

#Year Built
train<-subset(train, train$year_built>1875)

###############################################################################################################
###Only keep interesting columns
###############################################################################################################

train <- select(train, address, gross_sqft, year_built, sale_price, sale_date, year_of_sale, YearAlter1, YearAlter2,
                Landmark, GarageArea, LotArea,	BldgArea,	ComArea,	ResArea,	OfficeArea,	RetailArea,
                StrgeArea,	FactryArea,	OtherArea,	NumBldgs,	NumFloors,	UnitsRes,	UnitsTotal, HistDist,
                XCoord, YCoord, land_sqft)


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
hist(train_eda$GarageArea)
sample<-subset(train_eda, train$GarageArea>0)
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
  myDate<-c(myDate, paste(x$year[i], x$month[i], "01",sep='-'))
}
myDate
x$myDate<-myDate





paste(x$year[1], x$month[1], sep='')

ggplot(x, aes(x=myDate, y=average_price)) + geom_point(color='steelblue')

#########################Reading in FRED
HPI<-read.csv('NYXRSA.csv')[193:372,]

ggplot(HPI, aes(x=DATE, y=NYXRSA)) + geom_point(color='orange')

Merged_Price_HPI <- sqldf('
                          select 
                            HPI.DATE as Date,
                            HPI.NYXRSA as HPI,
                            x.average_price as Avg_Price
                          from HPI inner join x on HPI.DATE=x.myDate')

head(Merged_Price_HPI)

normalize_HPI <- function(x) {
  return ((x - 180.7487) / 17.23574)
}

normalize_Price <- function(x) {
  return ((x - 657459.4) / 161466.6)
}

Merged_Price_HPI$HPI_n<-unlist(lapply(Merged_Price_HPI$HPI, normalize_HPI))
Merged_Price_HPI$Avg_Price_n<-unlist(lapply(Merged_Price_HPI$Avg_Price, normalize_Price))

mean(Merged_Price_HPI$HPI_n)
sd(Merged_Price_HPI$HPI_n)


mean(Merged_Price_HPI$Avg_Price_n)
sd(Merged_Price_HPI$Avg_Price_n)

Merged_Price_HPI$Date <- as.Date(Merged_Price_HPI$Date, format="%Y-%m-%d")

ggplot(Merged_Price_HPI, aes(Date, y = Standardized_Value, color = variable)) + 
  geom_point(aes(y = HPI_n, col = "HPI")) + 
  geom_point(aes(y = Avg_Price_n, col = "Avg_Price")) +
  ggtitle('Average Price vs. HPI') +
  theme(plot.title = element_text(hjust = 0.5))

colnames(train_eda)


past_08 <-function(x){
  if (x>2008){
    return(1)
  }
  else{
    return(0)
  }
}


train_eda$past_08<-unlist(lapply(train_eda$year, past_08))


ggplot(train_eda, aes(sale_date, y = Price)) + 
  geom_point(aes(y = sale_price)) +
  ggtitle('Price vs. Sale Date') +
  theme(plot.title = element_text(hjust = 0.5))


###############################################################################################################
###Year Built
###############################################################################################################
head(train_eda$year_built)
summary(train_eda$year_built)
hist(train_eda$year_built)

price_by_yr <- sqldf('
                     select
                      year_built,
                      avg(sale_price) as avg_price,
                      count(*) as count
                     from train_eda
                     group by year_built')
ggplot(price_by_yr, aes(x=year_built, y=avg_price)) + 
  geom_point(color='steel blue') +
  ggtitle('Average Price vs. Year Built') +
  theme(plot.title = element_text(hjust = 0.5))


price_by_yr_large <- subset(price_by_yr, price_by_yr$count>100)
ggplot(price_by_yr_large, aes(x=year_built, y=avg_price)) + 
  geom_point(color='steel blue') +
  ggtitle('Average Price vs. Year Built') +
  theme(plot.title = element_text(hjust = 0.5))


price_by_yr_small <- subset(price_by_yr, price_by_yr$count<100)
ggplot(price_by_yr_small, aes(x=year_built, y=avg_price)) + 
  geom_point(color='steel blue') +
  ggtitle('Average Price vs. Year Built') +
  theme(plot.title = element_text(hjust = 0.5))

###############################################################################################################
###Altered
###############################################################################################################
head(train_eda$YearAlter1)

is_altered <- function(x){
  if(x>0){
    return(1) 
  }
  else{
    return(0)
  }
}

train_eda$Alter1_Ind<-unlist(lapply(train_eda$YearAlter1, is_altered))
sum(train_eda$Alter1_Ind)

train_eda$Alter2_Ind<-unlist(lapply(train_eda$YearAlter2, is_altered))
sum(train_eda$Alter2_Ind)


train_eda %>% select(Alter1_Ind, sale_price) %>% ggplot(aes(factor(Alter1_Ind), sale_price)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust =1)) + 
  xlab('Alter 1')

train_eda %>% select(Alter2_Ind, sale_price) %>% ggplot(aes(factor(Alter2_Ind), sale_price)) + 
  geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust =1)) + 
  xlab('Alter 2')

Alter1_Pr <- sqldf('
                   select
                    avg(sale_price) as Avg_Price,
                    Alter1_Ind
                   from train_eda
                   group by Alter1_Ind
                   ')
Alter1_Pr

#Any Alter2s but not Alter1s?
disc <- NULL
for(i in 1:dim(train_eda)[1]){
  if(train_eda$Alter2_Ind[i]==1 & train_eda$Alter1_Ind[i]==0){
    disc<-c(disc,1)
  }
  else{
    disc<-c(disc,0)
  }
}
sum(disc)

###############################################################################################################
###Gross_Sqft
###############################################################################################################

ggplot(train_eda, aes(x=gross_sqft, y=sale_price)) + 
  geom_point(color='steel blue') +
  ggtitle('Sale Price vs. Gross Sqft') +
  theme(plot.title = element_text(hjust = 0.5))
  
###############################################################################################################
###Coordinates
###############################################################################################################
head(train_eda)
train_eda <- subset(train_eda, train_eda$XCoord!=0)

ggplot(train_eda, aes(x=XCoord, y=YCoord)) + 
  geom_point(color='steel blue') +
  ggtitle('Sale Price vs. Gross Sqft') +
  theme(plot.title = element_text(hjust = 0.5))


###############################################################################################################
###Landmark
###############################################################################################################
head(train_eda)

is.na(train_eda$Landmark[1])

has_landmark <- function(x){
  if(is.na(x)){
    return(0) 
  }
  else{
    return(1)
  }
}


train_eda$has_Landmark<-unlist(lapply(train_eda$Landmark, has_landmark))
sum(train_eda$has_Landmark)

###############################################################################################################
###Merge HPI into Training DS
###############################################################################################################
head(HPI)
HPI$month<-format(as.Date(HPI$DATE, format="%Y-%m-%d"), "%m")
HPI$year<-format(as.Date(HPI$DATE, format="%Y-%m-%d"), "%Y")
head(HPI)

head(train_eda)

train_w_hpi <- sqldf('
                     select 
                      a.*,
                      b.NYXRSA as NY_HPI
                     from train_eda a left join HPI b on a.month=b.month and a.year=b.year
                     ')
head(train_w_hpi)
dim(train_w_hpi)
dim(train_eda)

train_w_hpi$current_HPI<-rep(read.csv('NYXRSA.csv')$NYXRSA[381],dim(train_w_hpi)[1])
head(train_w_hpi)

train_w_hpi$hpi_diff<-train_w_hpi$NY_HPI-train_w_hpi$current_HPI
head(train_w_hpi)

train_w_hpi$hpi_pct<-train_w_hpi$hpi_diff/train_w_hpi$current_HPI
head(train_w_hpi)

(max(HPI$NYXRSA)-min(HPI$NYXRSA))/min(HPI$NYXRSA)

the_dundies<-read.csv('the_dundies.csv')

test <- sqldf('
              select 
                a.*,
                b.Price as Zillow_Price
              from
                train_w_hpi a left outer join the_dundies b on a.address=b.Address')

head(test)
test[is.na(test)] <- 0
test$adjusted_zillow=test$Zillow_Price*(1+test$hpi_pct)

compare_zillow<-function(i){
  if (test$adjusted_zillow[i]==0){
    print(i)
    final_adjusted_price=c(final_adjusted_price,test$sale_price[i])
  }
  else if(test$sale_price[i]<(0.5*test$adjusted_zillow[i])){
    final_adjusted_price = c(final_adjusted_price, test$adjusted_zillow[i])
  }
  else{
    final_adjusted_price = c(final_adjusted_price, test$sale_price[i])
  }
}

final_adjusted_price <- NULL
for (i in seq(1:30)){
  compare_zillow(i)
}










write.csv(test,'clean_train_w_hpi')




  
  

