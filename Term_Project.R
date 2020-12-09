#Stat 350 Term Project:
#Dong Cai
#301281277


#####1.Abstract#####
#In this project, I am using the data from CarDekho.com, to get a find a good 
#regression model to predict the selling price of the second-hand car in the 
#second-hand car market. The data file that we are using is a CSV file which 
#includes the following columns: car name, year, selling price, showroom price, 
#kilometers driven, fuel type, seller type, transmission, and number of 
#previous owners. In this project, the selling price is the response variables, 
#and the rest are the potential regressor in our model. I will use a bunch of 
#regression analysis technique, step by step, to determine the best regression 
#model for predicting the sell price of the used automobile.

#####2. Introduction#####
#The question of interest in our project is to find a good regression model to 
#predict the selling price of the second-hand car in the second-hand car market. 
#Here are the following procedure: 
#•	Firstly, I do a data description on the raw data to get some general idea 
#   about the data set. 
#•	Secondly, we use paired graph and vif value test any multicollinearity. 
#•	Thirdly I use forward selection method to briefly get rid of some variable. 
#•	Fourthly, I find the point with high leverage and high standardized 
#   residuals. These points are regard the influential point. I treat them as 
#   outliers and taken them out of my data set. 
#•	Fifthly, I did a cross validation the test the validity of my regression 
#   model. I set 80% for training the model and 20% for testing the model, and 
#   we repeat it for 10 times.
#•	In the end, I did robust regression to improve the model.



install.packages("ggplot2")
library(ggplot2)
install.packages("faraway")
library(faraway)
install.packages("caret")
library(caret)
install.packages("psych")
library(psych)
install.packages("tidyverse")
library(tidyverse)
install.packages("car")
library(car)
install.packages("packagename")
library(MASS)


#####data description:#####
mydata = read.csv ("car_data.csv")
head(mydata)

par(mfrow=c(2,2))
hist(mydata$Year)
hist(mydata$Selling_Price)
hist(mydata$Present_Price)
hist(mydata$Kms_Driven)
#The above is the data distribution of the four continuous variables in our 
#data, we can see except for the Year, all data are screw to the right.


#three different boxplots:
#‘Selling price’ vs ‘Seller type’
#‘Selling price’ vs ‘Transmission’
#‘Selling price’ vs ‘Car fuel type’
geom_boxplot(outlier.colour="black", outlier.shape=16,
             outlier.size=2, notch=FALSE)

p1<-ggplot(mydata, aes(x=Transmission, y=Sellprice, color=Transmission)) +
  geom_boxplot()

p2<-ggplot(mydata, aes(x=Fuel_Type, y=Sellprice, color=Fuel_Type)) +
  geom_boxplot()

p3<-ggplot(mydata, aes(x=Seller_Type, y=Sellprice, color=Seller_Type)) +
  geom_boxplot()

#By looking at the graph for ‘Selling price’ vs ‘Transmission’, we do not find 
#a big difference in the data distribution between the car automatic car and 
#manual car. Therefore, we exclude the ‘Transmission’ from our equation. For the
#other two graphs, we can see obvious difference in the distribution for 
#difference categories. Therefore, we keep ‘Seller type’ and ‘Fuel type’ in our 
#equation for now.


#####4.Method & Result#####

###multicolinearlity###

#We first try to find the any sign of multicollinearity in the continuous 
#independent variables.
table<-cbind(mydata$Present_Price,mydata$Year,mydata$Km)
pairs(table)
#from the paired graph, we do not find obvious multicolinearity 
#between the continuous independent variable.

###claculateing vif###
Sellprice<-mydata$Selling_Price
Presentprice<-mydata$Present_Price
Year<-mydata$Year
Km<-mydata$Kms_Driven

mdl.lm<-lm(Sellprice~Presentprice+Year+Km)
vif(mdl.lm)
#there is not any vif value that is bigger than 10, 
#which , once again, shows no evidence of multicollinearity.



###selection method---forward selection method###

###step 1
FUll_Matrix<-cbind(Sellprice,Presentprice,Year,Km)
cor(FUll_Matrix)
first_model_lm<-lm(Sellprice~Presentprice)
summary(first_model_lm)
#we fisrtly find the variable with largest correlation with Sellprice is Present price
#since p-vlaue < 2.2e-16,Presentprice is included in the model

###step 2
e1=Sellprice-(0.71853+0.51685*Presentprice)
reduced_matrix_1 <-cbind(e1,Year,Km)
cor(reduced_matrix_1)
second_model_lm<-lm(Sellprice~Presentprice+Year)
summary(second_model_lm)
#we find year has the largest correlation with e1, we put it into the equation
#since p-vlaue < 2.2e-16,Year is included in the model
#also the p-value for the intercept and Presentprice is less than 2.2e-16,
#no variable is taken out


###step 3
third_modle_lm<-lm(Sellprice~Presentprice+Year+Km)
summary(third_modle_lm)
#the p-value for the Km is 0.731 which is quite large,
#km is not included in the model

#fit the modle with the two indicater variable
#We fit the model with ‘Presentprice’, ‘Year’, x1,x2,x3,
#•	If x1=1, then the car will be a diesel car
#•	If x2=1, then the car will be a patrol car
#•	If both x1 & x2 equal to 0, the car will be a CNG car 
#•	If x3=1, then the car was selling by a dealer
#•	If x3=0, then the car was selling by an individual
mydata2 = read.csv("car_data2.csv")
head(mydata2)

Sellprice<-mydata2$Selling_Price
Presentprice<-mydata2$Present_Price
Year<-mydata2$Year
x1<-mydata2$x1
x2<-mydata2$x2
x3<-mydata2$x3

mdl.lm1=lm(Sellprice~Presentprice+Year+x1+x2+x3)
summary(mdl.lm1)
#After we fit the regression, we find there x1 & x2 is not significant. 
#Therefore we remove ‘Fuel_Type’ from the model. And do the regression 
#once again.

mdl.lm2=lm(Sellprice~Presentprice+Year+x3)
summary(mdl.lm2)
#Now we find all the regressors are significant in the model.


###check regression assumption###

#1Check the constant variance assumption for the errors
#2Check the normality assumption
#3Check for large leverage points
#4Check for outliers
#5Check for influential points

par(mfrow=c(2,2))
plot(mdl.lm2)
#By looking at the Scale-Location plot and the ‘residual vs fitted’ graph, 
#we find there is a slight curvature. Therefore, constant variance 
#assumption for the errors is not perfectly met, the point 25, point 37 
#might be potential outliers. From the normal Q-Q plot, the residuals do 
#adequately meet the normality assumption, especially in the middle range 
#of the graph. From the ‘Residuals vs leverage’ points, the point, 37 and 
#point 25 seems have a high cook’s distance value, which could be 
#potential outliers.



###find the influential point###

# fit model
mdl.lm2=lm(Sellprice~Presentprice+Year+x3)

#compute the X matrix and then the hat-matrix... get h_ii's
delX<-cbind(rep(1,length(Sellprice)), Presentprice,Year,x3)
hii<-diag(delX%*%solve(t(delX)%*%delX)%*%t(delX))

# Identify points of high Leverage
p<-ncol(delX) # number of betas in the model (beta0,beta1,beta2)
n<-nrow(delX) # number of observations
which(hii>2*p/n) #points with high leverage
#we find the following point with high leverage 
#19  20  22  23  24  25  31  36  37  39  62  83  90 106 112 181 198 199 200 290 301

# Calculate the residuals for additional information 
resid(mdl.lm2)
rstandard(mdl.lm2)
rstudent(mdl.lm2)
residtable<-cbind(resid(mdl.lm2),rstandard(mdl.lm2), rstudent(mdl.lm2))
summary(residtable)
#With those point we determine the one that has standardized residual 
#bigger than 3.
rstandard(mdl.lm2)[19]
rstandard(mdl.lm2)[20]
rstandard(mdl.lm2)[22]
rstandard(mdl.lm2)[23]
rstandard(mdl.lm2)[24]
rstandard(mdl.lm2)[25]
rstandard(mdl.lm2)[31]
rstandard(mdl.lm2)[36]
rstandard(mdl.lm2)[37]
rstandard(mdl.lm2)[39]
rstandard(mdl.lm2)[62]
rstandard(mdl.lm2)[83]
rstandard(mdl.lm2)[90]
rstandard(mdl.lm2)[106]
rstandard(mdl.lm2)[112]
rstandard(mdl.lm2)[181]
rstandard(mdl.lm2)[198]
rstandard(mdl.lm2)[199]
rstandard(mdl.lm2)[200]
rstandard(mdl.lm2)[290]
rstandard(mdl.lm2)[301]

#We find the high leverage point with high standardized deviation is 
#point 20 25 39 200
#We will regard these point as outlier and removie these data from data 
#and form car_data3.csv




###Cross Validation###
#we are setting 80% for training the model and 20% for testing the model, 
#and we do it for 10 times
mydata3 = read.csv("car_data3.csv")
par(mfrow=c(5,2))
set.seed(71168)
nsamp=ceiling(0.8*length(mydata3$Selling_Price))
for(i in 1:10){
training_samps=sample(c(1:length(mydata3$Selling_Price)),nsamp)
training_samps=sort(training_samps)
train_data<-mydata3[training_samps, ]
test_data<-mydata3[-training_samps, ]


train.lm <- lm(formula = Selling_Price~Present_Price +Year+x3, data = train_data)
#summary(train.lm)

preds <- predict(train.lm,test_data)
plot(test_data$Selling_Price,preds)
abline(c(0,1))

R.sq = R2(preds, test_data$Selling_Price)
RMSPE = RMSE(preds, test_data$Selling_Price)
std_normal_RMSPE = RMSPE/sd(test_data$Selling_Price)
MAPE = MAE(preds, test_data$Selling_Price)

print(c(i,R.sq,RMSPE,std_normal_RMSPE,MAPE))
}
#From the 10 ‘prediction vs true value’ plot we can see that, 
#we can see that our prediction value and the true value fits pretty well. 
#The R^2 in each case are very large, showing the true selling price are 
#explained by our regression value quite well. Also we standardized the 
#RMSPE, and the value are quite close to 0, which is another good sign 
#showing our model make a good prediction. 



###Robust regression###
#since right now, there are still some data have relatively large residual,
#there are sign of unequal variance
#sicne we do not have a good idea of data transformation
#we will do a robust regression to improve our model
mydata4 = read.csv("car_data3.csv")
summary(mdl.lm4)
Sellprice<-mydata4$Selling_Price

Presentprice<-mydata4$Present_Price
Year<-mydata4$Year
x1<-mydata4$x1
x2<-mydata4$x2
x3<-mydata4$x3
mdl.lm4=lm(Sellprice~Presentprice+Year+x3)
rr.lm=rlm(Sellprice~Presentprice+Year+x3,psi=psi.huber)
summary(rr.lm)
#by comparing the plots from the original regression and the robust regression
#we can see, the model get little bit better in terms of less high residuals 
#and better normality.



#####5.conclusion#####
#In the end, the regression model we get is the following:
#  Sellprice = -728.1508 + 0.5080* Presentprice+ 0.3616*Year+ 0.9558*x3
#Where x3 is the indicator variable for the sell type. When x3=0, the 
#seller type is individual. When x3 = 1 when seller type is dealer. 
#From the equation we can see that the selling price is positively relate 
#to its present price and the year of the car. This make sense because the 
#more expensive the new car is, then the second-hand corresponding car will 
#also be more expensive. Also, when the second-hand car is newer, the more 
#expensive it is. I also find the second-hand car sell by the dealer tend to be 
#more expensive than the car sells by individual, which is also make sense, because 
#the dealer has to make some money between the buyer and the seller. 

#I also find some problems in our model. The first problem is that the 
#intercept of the model is negative. In the real world, there will not 
#be a case that having negative price. The second problem is that the 
#variable Kms_driven is not include in the model, which is taken out in 
#the forward selection. My personal opinion about it is that, usually Year 
#of the car is relate to the Kms_drive. The newer the car, the less km it 
#is driven. This makes me only include year in the model. Lastly, I am surprised
#the Fuel_Type is not included in the model as indicator variable, because, 
#in common sense, diesel car is more expensive than petrol car. 
