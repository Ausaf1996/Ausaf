weather=read.csv('weatherHistory.csv',stringsAsFactors = F)
View(weather_short)

#data preprocessing

#missing values

sum(is.na(weather))#no missing values

#distribution

median(weather$Apparent.Temperature..C.)# little left skewed,median greater than mean

hist(n_weather$Apparent.Temperature..C.)
hist(n_weather$Temperature..C.)

#IQR, removing outliers

iqr=IQR(weather$Apparent.Temperature..C.)
iqr1=iqr*1.5
fifty_per=quantile(weather$Apparent.Temperature..C.,0.5)
u_limit=fifty_per+iqr1
l_limit=fifty_per-iqr1
l_limit
sd(weather$Apparent.Temperature..C.)
n_weather=weather[weather$Apparent.Temperature..C.>l_limit&weather$Apparent.Temperature..C.<u_limit,]

#EDA

library('ggplot2')
weather_short=n_weather[sample(nrow(weather),0.01*nrow(weather)),]
ggplot(weather_short,aes(Humidity,Apparent.Temperature..C.))+geom_point()
ggplot(weather_short,aes(Apparent.Temperature..C.,Humidity))+geom_point()
cor(n_weather$Temperature..C.,n_weather$Humidity)
scatterplot(weather_short$Apparent.Temperature..C.~weather_short$Humidity)
library('car')

ggplot(weather_short,aes(Apparent.Temperature..C.,Wind.Speed..km.h.))+geom_point()
cor(n_weather$Apparent.Temperature..C.,n_weather$Pressure..millibars.)
scatterplot(weather_short$Temperature..C.,weather_short$Wind.Speed..km.h.)
table(weather$Precip.Type)
ggplot(n_weather,aes(Precip.Type,Apparent.Temperature..C.))+geom_boxplot()
length(table(weather$Summary))
ggplot(n_weather,aes(Daily.Summary,Apparent.Temperature..C.))+geom_point()
sv_weather=n_weather[,c(12,3,6,4)]
View(sv_weather)

#train and test

set.seed(675)
ids=sample(nrow(sv_weather),0.8*nrow(sv_weather))
train=sv_weather[ids,]
test=sv_weather[-ids,]
lin_model_weather=lm(Temperature..C.~.,data=train)
s=summary(lin_model_weather)
test$pred=predict(lin_model_weather,newdata=test)
View(test)

#RMSE error

test$error=test$Temperature..C.-test$pred
test$error_sq=test$error**2
sqrt(sum(test$error_sq)/length(test$error_sq))

#checking for  autocorrelation
plot(lin_model_weather,which=1)
plot(lin_model_weather,which=3)
durbinWatsonTest(lin_model_weather)

#checkng for normality of errors
qqPlot(lin_model_weather,main="QQplot") #library needed to be installed
plot(lin_model_weather,which=2)
hist(lin_model_weather$residuals)  

#checking for outliers and leverages
plot(lin_model_weather,which=4) # leverages
outlierTest(lin_model_weather)

#checking for multicollinearity
vif(lm(Temperature..C.~.,data=sv_weather))
