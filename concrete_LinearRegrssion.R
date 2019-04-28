concrete=read.csv('Concrete_Data.csv',stringsAsFactors = F)
View(concrete)

#data preprocessing
# missing values

sum(is.na(concrete))

#Distribution

hist(m_concrete$Concrete.compressive.strength.MPa..megapascals..)
mean(m_concrete$Concrete.compressive.strength.MPa..megapascals..)
median(m_concrete$Concrete.compressive.strength.MPa..megapascals..)

#removing outliers

quantile(concrete$Concrete.compressive.strength.MPa..megapascals..,c(0.02,0.98))
m_concrete=concrete[concrete$Concrete.compressive.strength.MPa..megapascals..>8.0348&concrete$Concrete.compressive.strength.MPa..megapascals..<74.26,]
nrow(concrete)
nrow(m_concrete)

# EDA
library('ggplot2')
library('car')
cor(m_concrete)
ggplot(m_concrete,aes(com$com4,com$com9))+geom_point()
scatterplot(com$com4,com$com9)
com=m_concrete
names(com)=c('com1','com2','com3','com4','com5','com6','com7','com8','com9')
View(com)
cor(com)

#train and test

m_com=com[,c(1,2,3,4,8,9)]

set.seed(675)
ids=sample(nrow(m_com),0.8*nrow(m_com))
train=m_com[ids,]
test=m_com[-ids,]
lin_model_concrete=lm(com9~.,data=train)
summary(lin_model_concrete)
test$pred=predict(lin_model_concrete,newdata=test)
View(test)


#RMSE error

test$error=test$com9-test$pred
test$error_sq=test$error**2
sqrt(sum(test$error_sq)/length(test$error_sq))

#Mape error

test$abserr=abs(test$error) # absolute error, positive error
test$per_err=(test$abserr/test$com9)*100
mean(test$per_err)

#checking for  autocorrelation
plot(lin_model_concrete,which=1)
plot(lin_model_concrete,which=3)
durbinWatsonTest(lin_model_concrete)

#checkng for normality of errors
qqPlot(lin_model_concrete,main="QQplot") #library needed to be installed
plot(lin_model_concrete,which=2)
hist(lin_model_concrete$residuals)  

#checking for outliers and leverages
plot(lin_model_concrete,which=4) # leverages
outlierTest(lin_model_concrete)

#checking for multicollinearity
vif(lm(com9~.,data=m_com))
