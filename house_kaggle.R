house=read.csv('c:/Users/LENOVO/My Documents/data sets/housePrice/train.csv')

#checking for missing values
length(house)
sum(is.na(house))
is.na(house)

#removing columns which has more tha  50% missing values
colSums((is.na(house)))
mis=colSums(is.na(house))
nam=colnames(house)
t=NA
n=1
for(i in 1:length(house)){
  if(mis[[i]]>(nrow(house)*0.4)){
    t[n]=nam[i]
    n=n+1
  }
}

#removing selcted columns
t
house[,t]=NULL
colSums((is.na(house)))

#using KNN to find missing values
library(DMwR)
house=knnImputation(house,k=3)

sum(is.na(house))

#data types of house
str(house)
house$Id=NULL

#converting factors to dummy variables
n=1
t=0
for(i in 1:length(house)){
  if(class(house[,i])=='factor'){
    t[n]=colnames(house)[i]
    n=n+1
  }
}

library(dummies)
house=dummy.data.frame(house,names=c(t))
length(house)



#*********************************************************************************



#importing test data set
test=read.csv('c:/Users/LENOVO/My Documents/data sets/housePrice/test.csv')

#checking for missing values
length(test)
sum(is.na(test))

#removing columns which has more tha  50% missing values
colSums((is.na(test)))
mis=colSums(is.na(test))
nam=colnames(test)
t=NA
n=1
for(i in 1:length(test)){
  if(mis[[i]]>(nrow(test)*0.4)){
    t[n]=nam[i]
    n=n+1
  }
}

#removing selcted columns
t
test[,t]=NULL
colSums((is.na(test)))

#using KNN to find missing values
library(DMwR)
test=knnImputation(test,k=3)

sum(is.na(test))

#data types of house
str(test)

test$Id=NULL

#converting factors to dummy variables
n=1
t=0
for(i in 1:length(test)){
  if(class(test[,i])=='factor'){
    t[n]=colnames(test)[i]
    n=n+1
  }
}

library(dummies)
test=dummy.data.frame(test,names=c(t))

length(test)

#making sure train and test have same variables
length(house)
length(test)

SalePrice=house$SalePrice

nt=0
n=1
for(i in 1:length(house)){
  flag=0
  for(j in 1:length(test)){
    if(colnames(house)[i]==colnames(test)[j]){
      flag=1
    }
  }
  if(flag==0){
    nt[n]=colnames(house)[i]
    n=n+1
  }
}
nt
house[,nt]=NULL
length(house)
length(test)

house=cbind(house,SalePrice)


#******************************************************************************

#building simple function for xgb
#libraries required
library(xgboost)
library(ggplot2)
library(Matrix)
library(ROCR)
#initial preprocessing for xgb

#function
cm_xgb=function(data,target,z=0.005){
  for(i in 1){
    #creating train and test models
    index=sample(nrow(data),round(0.7*nrow(data)))
    train=data[index,]
    test=data[-index,]
    #creating seperate dependent and independent variables
    x=match(target,names(data))
    train_x=train[,-x]
    train_y=train[,x]
    test_x=test[,-x]
    test_y=test[,x]
    #creating xgb matrix
    dtrain=xgb.DMatrix(data=as.matrix(train_x),label=train_y)
    dtest=xgb.DMatrix(data=as.matrix(test_x),label=test_y)
    #creating parameters for xgboost
    params=list(booster='gbtree',objective='reg:linear',
                eta=0.005,gamma=0,maxdepth=10,min_child_weight=3,
                subsample=0.8,colsample_bytree=0.7,lamda=1)
    #creating the watchlist
    watchlist=list(train=dtrain,test=dtest)
    #buidling the model
    
    model<<-xgb.train(params=params,data=dtrain,nrounds=2000,watchlist=watchlist,
                    print_every_n=1,early_stopping_rounds=15,maximize=F)
    it[i]=model$best_iteration
    graph=model$evaluation_log
    #making graphs using ggplot
    
    pl=ggplot(graph)+geom_point(aes(graph$iter,graph$test_rmse,color="test"))+
      geom_point(aes(graph$iter,graph$train_rmse,color='train'))+
      xlab('Number of iterations')+ylab('RMSE error')
    print(pl)
    #predicting values of test data 
    pred=predict(model,dtest)
    #calculating RMSE for test data
    error=pred-test_y
    error1=error^2
    error1=sum(error1)/length(error)
    rmsi[i]=sqrt(error1)
    #calculating MAPE error for trin data
    error=abs(error)
    error1=(error/test_y)*100
    mape[i]=mean(error1)
    #predicting values for train data  
    pred=predict(model,dtrain)
    #calculating RMSE for train data
    error=pred-train_y
    error1=error^2
    error1=sum(error1)/length(error)
    rmsi1[i]=sqrt(error1)
    #calculating MAPE error for test data
    error=abs(error)
    error1=(error/train_y)*100
    mape1[i]=mean(error1)
    }
  train_cm=c(mean(rmsi1),mean(mape1))
  test_cm=c(mean(rmsi),mean(mape))
  nam=c('RMSI error','MAPE error')
  k=sprintf('the average best iteration is %f',mean(it))
  df=data.frame(nam,train_cm,test_cm)
  n=list(k,df)
  return(n)
}
cm_xgb(house,'SalePrice')

?xgboost
model

#*********************************************************************************

#creating xgb matrix
dtest=xgb.DMatrix(data=as.matrix(test))

#predicting values of test data 
pred=predict(model,dtest)
pred

#creating the dataframe 
Id=1461:2919
sub=cbind(Id,pred)
sub=as.data.frame(sub)
colnames(sub)=c('Id','SalePrice')

#saving table
write.table(sub,file='sub',sep=',',row.names = FALSE)


