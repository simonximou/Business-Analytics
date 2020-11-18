
#####Section 1#####

HPDS<-read.csv("C:/Users/zx616/Desktop/Econ494/Econ494 Project1 TIDY data.csv") #import excel data and call it HPDS
library(ggplot2)
HPDS$sqft_living2<-HPDS$sqft_living^2
HPDS$sqft_above2<-HPDS$sqft_above^2
HPDS$sqft_basement2<-HPDS$sqft_basement^2
p<-.7#fraction of sample to be used for training
obs_count<-dim(HPDS)[1]#number of observations (rows) in the dataframe
training_size <- floor(p * obs_count)
training_size
set.seed(1234)
train_ind <- sample(obs_count, size = training_size)
Training <- HPDS[train_ind, ] #pulls random rows for training
Testing <- HPDS[-train_ind, ] #pulls random rows for testing
dim(Training)
dim(Testing)
plot(price ~ sqft_living, HPDS) #PLOT ENTIRE DATASET
plot(price ~ sqft_living, Training, col ='blue') #PLOTS THE IN-SAMPLE TRAINING PARTITION
plot(price ~ sqft_living, Testing,   col ='red', pch=3) #PLOTS THE OUT-OF-SAMPLE TESTING PARTITION
points(Training$sqft_living, Training$price, col='blue') #PLOTS THE OUT-OF-SAMPLE TESTING PARTITION
points(Testing$sqft_living, Testing$price, col='red', pch=3) #PLOTS THE OUT-OF-SAMPLE TESTING PARTITION

######Section 2 building Linear model#######

M1 <- lm(price ~ sqft_living+sqft_basement+bedrooms+bathrooms+floors+waterfront+view+condition, Training)
M2 <- lm(price ~ sqft_living+sqft_living2+sqft_basement+bedrooms+bathrooms+floors+waterfront+view+condition, Training)
M3 <- lm(price ~ sqft_living+sqft_living2+sqft_basement+sqft_basement2+bedrooms+bathrooms+floors+waterfront+view+condition, Training)
M4 <- lm(price ~ sqft_living+sqft_living2+sqft_above+bedrooms+bathrooms+floors+waterfront+view+condition, Training)
M5 <- lm(price ~ sqft_living+sqft_living2+sqft_above+sqft_above2+bedrooms+bathrooms+floors+waterfront+view+condition, Training)

######Section 3 Regression result#####

options(scipen=999) #force to not use scientific notations
summary(M1) 
summary(M2) 
summary(M3)  
summary(M4) 
summary(M5)

######Section 4 Predictions results######
#GENERATING PREDICTIONS ON THE TRAINING DATA

PRED_1_IN <- predict(M1, Training) #generate predictions on the (in-sample) training data
View(PRED_1_IN)
View(M1$fitted.values) #these are the same as the fitted values
PRED_2_IN <- predict(M2, Training) #generate predictions on the (in-sample) training data
View(PRED_2_IN)
View(M2$fitted.values)
PRED_3_IN <- predict(M3, Training) #generate predictions on the (in-sample) training data
View(PRED_3_IN)
View(M3$fitted.values)
PRED_4_IN <- predict(M4, Training) #generate predictions on the (in-sample) training data
View(PRED_4_IN)
View(M4$fitted.values)
PRED_5_IN <- predict(M5, Training) #generate predictions on the (in-sample) training data
View(PRED_5_IN)
View(M5$fitted.values)

#GENERATING PREDICTIONS ON THE TEST DATA FOR BENCHMARKING

PRED_1_OUT <- predict(M1, Testing) #generate predictions on the (out-of-sample) testing data
PRED_2_OUT <- predict(M2, Testing)
PRED_3_OUT <- predict(M3, Testing)
PRED_4_OUT <- predict(M4, Testing)
PRED_5_OUT <- predict(M5, Testing)

#COMPUTING IN-SAMPLE AND OUT-OF-SAMPLE ROOT MEAN SQUARED ERROR
RMSE_1_IN<-sqrt(sum((PRED_1_IN-Training$price)^2, na.rm = TRUE)/length(PRED_1_IN))  #computes in-sample error
RMSE_1_OUT<-sqrt(sum((PRED_1_OUT-Testing$price)^2, na.rm = TRUE)/length(PRED_1_OUT)) #computes out-of-sample 

RMSE_2_IN<-sqrt(sum((PRED_2_IN-Training$price)^2, na.rm = TRUE)/length(PRED_2_IN))  #computes in-sample error
RMSE_2_OUT<-sqrt(sum((PRED_2_OUT-Testing$price)^2, na.rm = TRUE)/length(PRED_2_OUT)) #computes out-of-sample 

RMSE_3_IN<-sqrt(sum((PRED_3_IN-Training$price)^2, na.rm = TRUE)/length(PRED_3_IN))  #computes in-sample error
RMSE_3_OUT<-sqrt(sum((PRED_3_OUT-Testing$price)^2, na.rm = TRUE)/length(PRED_3_OUT)) #computes out-of-sample 

RMSE_4_IN<-sqrt(sum((PRED_4_IN-Training$price)^2, na.rm = TRUE)/length(PRED_4_IN))  #computes in-sample error
RMSE_4_OUT<-sqrt(sum((PRED_4_OUT-Testing$price)^2, na.rm = TRUE)/length(PRED_4_OUT)) #computes out-of-sample 

RMSE_5_IN<-sqrt(sum((PRED_5_IN-Training$price)^2, na.rm = TRUE)/length(PRED_5_IN))  #computes in-sample error
RMSE_5_OUT<-sqrt(sum((PRED_5_OUT-Testing$price)^2, na.rm = TRUE)/length(PRED_5_OUT)) #computes out-of-sample 

RMSE_1_IN 
RMSE_2_IN 
RMSE_3_IN
RMSE_4_IN 
RMSE_5_IN 

RMSE_1_OUT 
RMSE_2_OUT 
RMSE_3_OUT 
RMSE_4_OUT 
RMSE_5_OUT 

c(RMSE_1_IN,RMSE_2_IN,RMSE_3_IN,RMSE_4_IN,RMSE_5_IN)
c(RMSE_1_OUT,RMSE_2_OUT,RMSE_3_OUT,RMSE_4_OUT,RMSE_5_OUT)






