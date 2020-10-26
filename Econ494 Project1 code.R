#######Cleaning Data########

install.packages("readxl") #install readxl 
library("readxl") 
HPDS<-read_excel("C:/Users/zx616/Desktop/BA data.xlsx") #import excel data and call it HPDS
plot(HPDS$price, HPDS$bedrooms) #plot scatter plots for all variables related to house price
plot(HPDS$price, HPDS$bathrooms)
plot(HPDS$price, HPDS$sqft_living)
plot(HPDS_b$price, HPDS_b$sqft_lot)
plot(HPDS_b$price, HPDS_b$floors)
plot(HPDS_b$price, HPDS_b$view)
plot(HPDS_b$price, HPDS_b$condition)
plot(HPDS_b$price, HPDS_b$sqft_above)
plot(HPDS_b$price, HPDS_b$sqft_basement)
plot(HPDS_b$price, HPDS_b$yr_built)
HPDS_b <- HPDS #create a new variable called HPDS_b that equals to HPDS
HPDS_b$price[HPDS_b$price>4000000]<-NA #set all housing price larger than 8million as NA (outlier)
HPDS_b$price[HPDS_b$price<80000]<-NA #set all housing price less than 80000 as NA (invalid)
HPDS_b$bedrooms[HPDS_b$bedrooms==0]<-NA #set all invalid bedrooms value as NA
HPDS_b$bathrooms[HPDS_b$bathrooms==0]<-NA #set all invalid bathrooms value as NA
HPDS_b$sqft_basement[HPDS_b$sqft_basement==0]<-NA
HPDS_b$bathrooms[HPDS_b$bathrooms>7]<-NA
HPDS_b$bedrooms[HPDS_b$bedrooms>7]<-NA
HPDS_b$sqft_lot[HPDS_b$sqft_lot>400000]<-NA
HPDS_b$waterfront<-as.factor(HPDS_b$waterfront)
HPDS_b$condition<-as.factor(HPDS_b$condition)
HPDS_b$view<-as.factor(HPDS_b$view)
avg_bedrooms<-aggregate(price ~ bedrooms, HPDS_b, mean)
avg_bathrooms<-aggregate(price ~ bathrooms, HPDS_b, mean)
avg_floors<-aggregate(price ~ floors, HPDS_b, mean)
avg_condition<-aggregate(price ~ condition, HPDS_b, mean)
avg_view<-aggregate(price ~ view, HPDS_b, mean)
library(plyr)
means <- ddply(HPDS_b, "waterfront", summarise, meanyr=mean(yr_built))
View(HPDS_b) #check out the dataset
mean(HPDS_b$price) #check if NA's are there, output should be na since mean cannot compute NA
mean(HPDS_b$price, na.rm=TRUE) #mean value ignore NA's
mean(HPDS_b$bedrooms)
mean(HPDS_b$bedrooms, na.rm=TRUE)
mean(HPDS_b$bathrooms)
mean(HPDS_b$bathrooms, na.rm=TRUE)
mean(HPDS_b$sqft_basement)
mean(HPDS_b$sqft_basement, na.rm=TRUE)
dim(HPDS_b) #check if any variable are lost, nope

######Exploratory analysis#########
######very basic checks############
View(HPDS_b)  ##View data in spreadsheet format
summary(HPDS_b[,2:14])  ##generates summary statistics for all variables
plot(HPDS_b$price, HPDS_b$bedrooms, col=factor(HPDS_b$waterfront))  ##plot of price vs bedrooms colored by the waterfront variable
mean(HPDS_b$price[HPDS_b$waterfront==0], na.rm=TRUE) #check how waterfront would change price, or does it make sense?
mean(HPDS_b$price[HPDS_b$waterfront==1], na.rm=TRUE)
hist(HPDS_b$price) 
summary(HPDS_b$price)
hist(HPDS_b$sqft_living)  
summary(HPDS_b$sqft_living) 
hist(HPDS_b$sqft_lot)  
summary(HPDS_b$sqft_lot) 
hist(HPDS_b$sqft_above)  
summary(HPDS_b$sqft_above) 
hist(HPDS_b$sqft_basement)  
summary(HPDS_b$sqft_basement)  
#######Fancy stuff######
install.packages('ggplot2')
library(ggplot2)
ggplot(HPDS_b, aes(sqft_living, price, color=waterfront)) +
  geom_point(size=1, na.rm=TRUE)+
  geom_smooth(method = "lm", color="red")+ 
  xlab('Living area(square feet)') + 
  ylab('Price')+
  ggtitle('Price vs Living area')+
  theme(plot.title = element_text(hjust = .5))+
  scale_y_continuous(labels = scales::comma) +
  theme(legend.position = "none")+
  xlim(0,7500)

ggplot(HPDS_b, aes(sqft_lot, price,color=waterfront)) +
  geom_point(na.rm=TRUE)+
  geom_smooth(method = "lm", color="red")+ 
  xlab('Lot area(square feet)') + 
  ylab('Price')+
  ggtitle('Price vs Lot area')+
  theme(plot.title = element_text(hjust = .5))+
  scale_x_continuous(labels = scales::comma)+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "none")

ggplot(HPDS_b, aes(sqft_above, price, color=waterfront)) +
  geom_point(na.rm=TRUE)+
  geom_smooth(method = "lm", color="red")+ 
  xlab('Area above ground(square feet)') + 
  ylab('Price')+
  ggtitle('Price vs Area above ground')+
  theme(plot.title = element_text(hjust = .5))+
  scale_x_continuous(labels = scales::comma)+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "none")+
  xlim(0,7500)

ggplot(HPDS_b, aes(sqft_basement, price, color=waterfront)) +
  geom_point(na.rm=TRUE)+
  geom_smooth(method = "lm", color="red")+ 
  xlab('Basement area(square feet)') + 
  ylab('Price')+
  ggtitle('Price vs Basement area')+
  theme(plot.title = element_text(hjust = .5))+
  scale_x_continuous(labels = scales::comma)+
  scale_y_continuous(labels = scales::comma)+
  theme(legend.position = "none")+
  xlim(0,3000)

ggplot(avg_bathrooms, aes(bathrooms, price))+
  geom_line()+
  geom_smooth(method = "lm", color="steelblue2",se=FALSE)+ 
  xlab('Numbers of Bathrooms') + 
  ylab('Average Price')+
  ggtitle('Average Price vs Numbers of bathrooms')+
  theme(plot.title = element_text(hjust = .5))+
  scale_y_continuous(labels = scales::comma)

ggplot(avg_bedrooms, aes(bedrooms, price))+
  geom_line()+
  geom_smooth(method = "lm", color="steelblue2",se=FALSE)+ 
  xlab('Numbers of Bedrooms') + 
  ylab('Average Price')+
  ggtitle('Average Price vs Numbers of bedrooms')+
  theme(plot.title = element_text(hjust = .5))+
  scale_y_continuous(labels = scales::comma)

ggplot(avg_condition, aes(condition, price))+
  geom_line()+
  geom_smooth(method = "lm", color="steelblue2",se=FALSE)+ 
  xlab('Condition') + 
  ylab('Average Price')+
  ggtitle('Average Price vs condition')+
  theme(plot.title = element_text(hjust = .5))+
  scale_y_continuous(labels = scales::comma)

#####ggplot(avg_floors, aes(floors, price))+    ####didnt have enough space to talk about it#####
  #geom_line()+
  #geom_smooth(method = "lm", color="steelblue2",se=FALSE)+ 
  #xlab('Floors') + 
  #ylab('Average Price')+
  #ggtitle('Average Price vs Floors')+
  #theme(plot.title = element_text(hjust = .5))+
 # scale_y_continuous(labels = scales::comma)

ggplot(avg_view, aes(view, price))+
  geom_line()+
  geom_smooth(method = "lm", color="steelblue2",se=FALSE)+ 
  xlab('View') + 
  ylab('Average Price')+
  ggtitle('Average Price vs View')+
  theme(plot.title = element_text(hjust = .5))+
  scale_y_continuous(labels = scales::comma)

ggplot(HPDS_b, aes(waterfront, price)) +
  geom_violin(fill='steelblue2',na.rm=TRUE)+ 
  geom_boxplot(width=0.05, na.rm=TRUE)+
  scale_y_continuous(labels = scales::comma)+ 
  xlab('Waterfront') + 
  ylab('Price')+
  ggtitle('Price vs Being Waterfront')+
  theme(plot.title = element_text(hjust = .5))

ggplot(HPDS_b, aes(yr_built)) +
  geom_density(aes(fill = waterfront), alpha = 0.4) + 
  geom_vline(data=means, aes(xintercept=meanyr,  col=factor(waterfront)), linetype="dashed", size=1, show.legend = FALSE)+ 
  xlab('Year built') + 
  ylab('Density')+
  ggtitle('Year built density according to waterfront')+
  theme(plot.title = element_text(hjust = .5))

ggplot(HPDS_b, aes(yr_built)) +
  geom_area(stat = "bin", color = "black", fill = "#00AFBB")+ 
  xlab('Year Built') + 
  ylab('Count')+
  ggtitle('House sell according to year built')+
  theme(plot.title = element_text(hjust = .5))

ggplot(HPDS_b, aes(view, price)) + 
  geom_boxplot(color = "black", fill = "steelblue",na.rm=TRUE)+
  scale_y_continuous(labels = scales::comma)+ 
  xlab('View Rank') + 
  ylab('Price')+
  ggtitle('Price vs View')+
  theme(plot.title = element_text(hjust = .5))

ggplot(HPDS_b, aes(condition, price)) + 
  geom_boxplot(color = "black", fill = "steelblue",na.rm=TRUE)+ 
  xlab('House Condition') + 
  ylab('Price')+
  ggtitle('Price vs Condition of house')+
  theme(plot.title = element_text(hjust = .5))+
  scale_y_continuous(labels = scales::comma)

