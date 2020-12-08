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



mydata = read.csv ("car_data.csv")
head(mydata)

#pick the categorical variable
geom_boxplot(outlier.colour="black", outlier.shape=16,
             outlier.size=2, notch=FALSE)

p1<-ggplot(mydata, aes(x=Transmission, y=Sellprice, color=Transmission)) +
  geom_boxplot()

p2<-ggplot(mydata, aes(x=Fuel_Type, y=Sellprice, color=Fuel_Type)) +
  geom_boxplot()

p3<-ggplot(mydata, aes(x=Seller_Type, y=Sellprice, color=Seller_Type)) +
  geom_boxplot()



#We first did three different boxplots, which are Selling price vs Seller type, 
#selling price vs Transmission, and Selling price vs Car fuel type. By looking at
#the graph for Selling price vs Transmission, we do not find a big difference in the data distribution
#between the car automatic car and manual car. Therefore, we exclude the transmission from our equation. 
#For the other two graphs, we can see obvious difference in the distribution for difference categories. 
#Therefore, we keep Seller type and fuel type in our equation for now.

###multicolinearlity
table<-cbind(mydata$Present_Price,mydata$Year,mydata$Km)
pairs(table)
#from the paired graph, we do not find obvious multicolinearity between the contineous independent variable.

Sellprice<-mydata$Selling_Price
Presentprice<-mydata$Present_Price
Year<-mydata$Year
Km<-mydata$Kms_Driven

mdl.lm<-lm(Sellprice~Presentprice+Year+Km)
vif(mdl.lm)



#selection mehod
#calculate simple correlation of each xi with y and selec the on 
#with the largest correlation

###---stepwise selection---###


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
#also the p-value for the intercept and Presentprice is less than 2.2e-16,no variable is taken out


###step 3
third_modle_lm<-lm(Sellprice~Presentprice+Year+Km)
summary(third_modle_lm)
#the p-value for the Km is 0.731 which is quite large,km is not included in the model








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

mdl.lm2=lm(Sellprice~Presentprice+Year+x3)
summary(mdl.lm2)

###check regression assumption
#1Check the constant variance assumption for the errors
#2Check the normality assumption
#3Check for large leverage points
#4Check for outliers
#5Check for influential points
par(mfrow=c(2,2))
plot(mdl.lm2)


###find the influential point

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






###crossvalidation
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



###Robust regression:
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


#after we get the robust regression
#from the graph, we can see, the model get little bit better
#in terms of less high residuals and better normality.
