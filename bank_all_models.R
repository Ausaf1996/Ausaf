bank=read.csv('UniversalBank.csv')
View(bank)

#extracting information from zipcode
s=bank$ZIP.Code/1000
s=floor(s)
sort(table(s),decreasing = T)
bank$ZIP.Code=s
bank$ZIP.Code=as.character(bank$ZIP.Code)

for(i in 1:nrow(bank)){
  if(bank$ZIP.Code[i]=='94'){
    bank$ZIP.Code[i]='Brooklyn'
  }else if(bank$ZIP.Code[i]=='92'|bank$ZIP.Code[i]=='95'){
    bank$ZIP.Code[i]='Manhattan'
  }else{
    bank$ZIP.Code[i]='Queens'
  }
}


#checking for missing values
sum(is.na(bank))

#removing useless variables
bank$ID=NULL


#checking distribution
hist(bank$Age)
hist(bank$Experience)
hist(bank$Income)#using sqrt
bank$Income=sqrt(bank$Income)

table(bank$ZIP.Code)
table(bank$Family)
hist(bank$CCAvg)#using sqrt
bank$CCAvg=sqrt(bank$CCAvg)

table(bank$Education)
hist(bank$Mortgage)
prop.table(table(bank$Personal.Loan))# imbalanced
prop.table(table(bank$Securities.Account))#imbalanced
prop.table(table(bank$CD.Account))#imbalanced
prop.table(table(bank$Online))
prop.table(table(bank$CreditCard))

#converting the eligible varibales to factors
str(bank)
for(i in c(4,9,10,11,12,13)){
  bank[[i]]=as.factor(bank[[i]])
}

str(bank)

#checking for outliers
library(ggplot2)

ggplot(bank,aes(1,bank$Age))+geom_boxplot()
ggplot(bank,aes(1,bank$Experience))+geom_boxplot()
ggplot(bank,aes(1,bank$Income))+geom_boxplot()
ggplot(bank,aes(1,bank$CCAvg))+geom_boxplot()#remove outliers
ggplot(bank,aes(1,bank$Mortgage))+geom_boxplot()#remove outliers

#removing outliers
quantile(bank$CCAvg,0.99)
bank$CCAvg[bank$CCAvg>quantile(bank$CCAvg,0.99)]=quantile(bank$CCAvg,0.99)

quantile(bank$Mortgage,0.995)
bank$Mortgage[bank$Mortgage>quantile(bank$Mortgage,0.995)]=quantile(bank$Mortgage,0.995)

#finding average sampling for appropriate index
'''library(rpart)
for(i in 1:100){
  #train and test
  index=sample(nrow(bank),0.7*nrow(bank))
  train=bank[index,]
  test=bank[-index,]
  #buidling model
  model=rpart(Personal.Loan~.,data=train)#binary split, uses gini index
  pred=predict(model,train)
  pred=as.data.frame(pred)
  pred=ifelse(pred$'1'>0.5,1,0)
  mat=table(pred,train$Personal.Loan)
  recall[i]=mat[2,2]/(mat[2,2]+mat[2,1])
  precision[i]=mat[2,2]/(mat[2,2]+mat[1,2])
  f1_score[i]=(2*precision[i]*recall[i])/(precision[i]+recall[i])
  if(precision[i]>0.890&precision[i]<0.910&recall[i]>0.944&recall[i]<0.964&
     f1_score[i]>0.916&f1_score[i]<0.936){
    index1=index
  }
}
mean(precision)
mean(recall)
mean(f1_score)'''

#using my confm function and tuning
#function for returning confusion matrix and other parameters for decison tree
library(rpart)
library(rpart.plot)
cm_dt=function(data){
  for(i in 1:10){
    index=sample(nrow(data),0.7*nrow(data))
    train=data[index,]
    test=data[-index,]
    model=rpart(Personal.Loan~.,data=train)
    pred=predict(model,test)
    pred=as.data.frame(pred)
    pred=ifelse(pred$'1'>0.5,1,0)
    mat=table(pred,test$Personal.Loan)
    accuracy[i]=(mat[1,1]+mat[2,2])/(mat[1,2]+mat[2,1]+mat[1,1]+mat[2,2])
    recall[i]=mat[2,2]/(mat[2,2]+mat[2,1])
    precision[i]=mat[2,2]/(mat[2,2]+mat[1,2])
    f1_score[i]=(2*precision[i]*recall[i])/(precision[i]+recall[i])
    pred=predict(model,train)
    pred=as.data.frame(pred)
    pred=ifelse(pred$'1'>0.5,1,0)
    mat=table(pred,train$Personal.Loan)
    accuracy1[i]=(mat[1,1]+mat[2,2])/(mat[1,2]+mat[2,1]+mat[1,1]+mat[2,2])
    recall1[i]=mat[2,2]/(mat[2,2]+mat[2,1])
    precision1[i]=mat[2,2]/(mat[2,2]+mat[1,2])
    f1_score1[i]=(2*precision1[i]*recall1[i])/(precision1[i]+recall1[i])
    
  }
  train_cm=c(mean(accuracy1),mean(precision1),mean(recall1),mean(f1_score1))
  test_cm=c(mean(accuracy),mean(precision),mean(recall),mean(f1_score))
  nam=c('accuracy','precision','recall','f1_score')
  df=data.frame(nam,train_cm,test_cm)
  rpart.plot(model)
  print(model)
  return(df)
  
}
cm_dt(bank)
plotcp(model)
?rpart
model
library(rpart.plot)
rpart.plot(model)


#function for returning confusion matrix and other parameters for bagging
library(adabag)
cm_bag=function(data){
  for(i in 1:10){
    index=sample(nrow(data),0.7*nrow(data))
    train=data[index,]
    test=data[-index,]
    model=bagging(Personal.Loan~.,data=train,mfinal=20)
    pred=predict(model,test)
    mat=table(pred$class,test$Personal.Loan)
    accuracy[i]=(mat[1,1]+mat[2,2])/(mat[1,2]+mat[2,1]+mat[1,1]+mat[2,2])
    recall[i]=mat[2,2]/(mat[2,2]+mat[2,1])
    precision[i]=mat[2,2]/(mat[2,2]+mat[1,2])
    f1_score[i]=(2*precision[i]*recall[i])/(precision[i]+recall[i])
    pred=predict(model,train)
    mat=table(pred$class,train$Personal.Loan)
    accuracy1[i]=(mat[1,1]+mat[2,2])/(mat[1,2]+mat[2,1]+mat[1,1]+mat[2,2])
    recall1[i]=mat[2,2]/(mat[2,2]+mat[2,1])
    precision1[i]=mat[2,2]/(mat[2,2]+mat[1,2])
    f1_score1[i]=(2*precision1[i]*recall1[i])/(precision1[i]+recall1[i])
    
  }
  train_cm=c(mean(accuracy1),mean(precision1),mean(recall1),mean(f1_score1))
  test_cm=c(mean(accuracy),mean(precision),mean(recall),mean(f1_score))
  nam=c('accuracy','precision','recall','f1_score')
  df=data.frame(nam,train_cm,test_cm)
  return(df)
}
cm_bag(bank)
?bagging

#function for returning confusion matrix and other parameters for random forest
library(randomForest)
cm_rf=function(data){
  for(i in 1:10){
    index=sample(nrow(data),0.7*nrow(data))
    train=data[index,]
    test=data[-index,]
    model=randomForest(Personal.Loan~.,data=train,ntree=100,mtry=7,nodesize=30,maxnodes=14)
    pred=predict(model,test)
    mat=table(pred,test$Personal.Loan)
    accuracy[i]=(mat[1,1]+mat[2,2])/(mat[1,2]+mat[2,1]+mat[1,1]+mat[2,2])
    recall[i]=mat[2,2]/(mat[2,2]+mat[2,1])
    precision[i]=mat[2,2]/(mat[2,2]+mat[1,2])
    f1_score[i]=(2*precision[i]*recall[i])/(precision[i]+recall[i])
    pred=predict(model,train)
    mat=table(pred,train$Personal.Loan)
    accuracy1[i]=(mat[1,1]+mat[2,2])/(mat[1,2]+mat[2,1]+mat[1,1]+mat[2,2])
    recall1[i]=mat[2,2]/(mat[2,2]+mat[2,1])
    precision1[i]=mat[2,2]/(mat[2,2]+mat[1,2])
    f1_score1[i]=(2*precision1[i]*recall1[i])/(precision1[i]+recall1[i])
    
  }
  train_cm=c(mean(accuracy1),mean(precision1),mean(recall1),mean(f1_score1))
  test_cm=c(mean(accuracy),mean(precision),mean(recall),mean(f1_score))
  nam=c('accuracy','precision','recall','f1_score')
  df=data.frame(nam,train_cm,test_cm)
  return(df)
}
cm_rf(bank)
tuneRF(bank[,-9],bank[,9])
?randomForest

#function for returning confusion matrix and other parameters for adaptive boositng
library(adabag)
cm_ab=function(data){
  for(i in 1){
    index=sample(nrow(data),0.7*nrow(data))
    train=data[index,]
    test=data[-index,]
    model=boosting(Personal.Loan~.,data=train,mfinal=10,control=c(maxdepth=1))
    pred=predict(model,test)
    mat=table(pred$class,test$Personal.Loan)
    accuracy[i]=(mat[1,1]+mat[2,2])/(mat[1,2]+mat[2,1]+mat[1,1]+mat[2,2])
    recall[i]=mat[2,2]/(mat[2,2]+mat[2,1])
    precision[i]=mat[2,2]/(mat[2,2]+mat[1,2])
    f1_score[i]=(2*precision[i]*recall[i])/(precision[i]+recall[i])
    pred=predict(model,train)
    mat=table(pred$class,train$Personal.Loan)
    accuracy1[i]=(mat[1,1]+mat[2,2])/(mat[1,2]+mat[2,1]+mat[1,1]+mat[2,2])
    recall1[i]=mat[2,2]/(mat[2,2]+mat[2,1])
    precision1[i]=mat[2,2]/(mat[2,2]+mat[1,2])
    f1_score1[i]=(2*precision1[i]*recall1[i])/(precision1[i]+recall1[i])
    
  }
  train_cm=c(mean(accuracy1),mean(precision1),mean(recall1),mean(f1_score1))
  test_cm=c(mean(accuracy),mean(precision),mean(recall),mean(f1_score))
  nam=c('accuracy','precision','recall','f1_score')
  df=data.frame(nam,train_cm,test_cm)
  return(df)
}
cm_ab(bank)
?boosting
?rpart

#function for returning confusion matrix and other parameters for gradient boositng
library(gbm)
library(caret)
library(e1071)

bank$Personal.Loan=as.numeric(bank$Personal.Loan)
bank$Personal.Loan=ifelse(bank$Personal.Loan>1,1,0)

str(bank)
cm_gbm=function(data){
  for(i in 1){
    index=sample(nrow(data),0.7*nrow(data))
    train=data[index,]
    test=data[-index,]
    ntree=1000
    model=gbm(Personal.Loan~.,data=train,n.trees=ntree,
              distribution='bernoulli',
              interaction.depth =5,
              n.minobsinnode = 10,
              shrinkage = 0.005,
              bag.fraction = 0.6)
    test$Personal.Loan=as.numeric(test$Personal.Loan)
    pred=predict(model,test,n.trees=ntree,type='response')
    pred=ifelse(pred>0.5,1,0)
    preds=prediction(pred,test$Personal.Loan)
    AUC_test[i]=performance(preds,measure='auc')@y.values[[1]]
    mat=table(pred,test$Personal.Loan)
    accuracy[i]=(mat[1,1]+mat[2,2])/(mat[1,2]+mat[2,1]+mat[1,1]+mat[2,2])
    recall[i]=mat[2,2]/(mat[2,2]+mat[2,1])
    precision[i]=mat[2,2]/(mat[2,2]+mat[1,2])
    f1_score[i]=(2*precision[i]*recall[i])/(precision[i]+recall[i])
    pred=predict(model,train,n.trees=ntree,type='response')
    pred=ifelse(pred>0.5,1,0)
    preds=prediction(pred,train$Personal.Loan)
    AUC_train[i]=performance(preds,measure='auc')@y.values[[1]]
    mat=table(pred,train$Personal.Loan)
    accuracy1[i]=(mat[1,1]+mat[2,2])/(mat[1,2]+mat[2,1]+mat[1,1]+mat[2,2])
    recall1[i]=mat[2,2]/(mat[2,2]+mat[2,1])
    precision1[i]=mat[2,2]/(mat[2,2]+mat[1,2])
    f1_score1[i]=(2*precision1[i]*recall1[i])/(precision1[i]+recall1[i])
    
  }
  train_cm=c(mean(accuracy1),mean(precision1),mean(recall1),mean(f1_score1),mean(AUC_train))
  test_cm=c(mean(accuracy),mean(precision),mean(recall),mean(f1_score),mean(AUC_test))
  nam=c('accuracy','precision','recall','f1_score','AUC')
  df=data.frame(nam,train_cm,test_cm)
  return(df)
}
cm_gbm(bank)
?gbm


