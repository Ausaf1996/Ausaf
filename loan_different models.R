loan=read.csv('LoanData.csv')
View(loan)

#checking for any missing values
sum(is.na(loan))
summary(loan)

#using KNN imputation to fill up missing values
library(DMwR)
loan=knnImputation(data=loan,k=5)
sum(is.na(loan))
summary(loan)

#removing useless values
loan$Loan.AF8.ID=NULL

#checking for outliers and normality
prop.table(table(loan$Gender))
prop.table(table(loan$Married))
table(loan$Dependents)
prop.table(table(loan$Education))
prop.table(table(loan$Self.AF8.Employed))
prop.table(table(loan$Credit.AF8.History))
prop.table(table(loan$Property.AF8.Area))
prop.table(table(loan$Loan.AF8.Status))

#using log to normalize applicant income
hist(loan$ApplicantIncome)
hist(log(loan$ApplicantIncome))
loan$ApplicantIncome=log(loan$ApplicantIncome)

#using log to normalize co-applicant income
hist(loan$CoapplicantIncome)
hist(sqrt(loan$CoapplicantIncome))
loan$CoapplicantIncome=sqrt(loan$CoapplicantIncome)

#using sqrt to normalize loan amount
hist(loan$LoanAmount)
hist(sqrt(loan$LoanAmount))
loan$LoanAmount=sqrt(loan$LoanAmount)

#loan term
table(loan$Loan.AF8.Amount.AF8.Term)
hist(loan$Loan.AF8.Amount.AF8.Term)

library(ggplot2)
#income outliers
ggplot(loan,aes(1,loan$ApplicantIncome))+geom_boxplot()
quantile(loan$ApplicantIncome,c(0.01,0.99))
loan$ApplicantIncome[loan$ApplicantIncome>quantile(loan$ApplicantIncome,0.99)]=quantile(loan$ApplicantIncome,0.99)
loan$ApplicantIncome[loan$ApplicantIncome<quantile(loan$ApplicantIncome,0.01)]=quantile(loan$ApplicantIncome,0.01)

#co applicant income outliers test
ggplot(loan,aes(1,loan$CoapplicantIncome))+geom_boxplot()

#loan amount outliers
ggplot(loan,aes(1,loan$LoanAmount))+geom_boxplot()
quantile(loan$LoanAmount,c(0.01,0.99))
loan$LoanAmount[loan$LoanAmount>quantile(loan$LoanAmount,0.99)]=quantile(loan$LoanAmount,0.99)
loan$LoanAmount[loan$LoanAmount<quantile(loan$LoanAmount,0.01)]=quantile(loan$LoanAmount,0.01)

#loan term
ggplot(loan,aes(1,loan$Loan.AF8.Amount.AF8.Term))+geom_boxplot()

#factors check
str(loan)
loan$Credit.AF8.History=as.factor(loan$Credit.AF8.History)


#finding average sampling for appropriate index
library(rpart)
for(i in 1:100){
  #train and test
  index=sample(nrow(loan),0.7*nrow(loan))
  train=loan[index,]
  test=loan[-index,]
  #buidling model
  model=rpart(Loan.AF8.Status~.,data=train)#binary split, uses gini index
  pred=predict(model,train)
  pred=as.data.frame(pred)
  pred=ifelse(pred$Y>0.5,1,0)
  mat=table(pred,train$Loan.AF8.Status)
  recall[i]=mat[2,2]/(mat[2,2]+mat[2,1])
  precision[i]=mat[2,2]/(mat[2,2]+mat[1,2])
  f1_score[i]=(2*precision[i]*recall[i])/(precision[i]+recall[i])
  if(precision[i]>0.921&precision[i]<0.941&recall[i]>0.800&recall[i]<0.820&
     f1_score[i]>0.856&f1_score[i]<0.876){
    index1=index
  }
}
mean(precision)
mean(recall)
mean(f1_score)

#train and test
index=sample(nrow(loan),0.7*nrow(loan))
train=loan[index1,]
test=loan[-index1,]
#buidling model
model=rpart(Loan.AF8.Status~.,data=train)#binary split, uses gini index
pred=predict(model,train)
pred=as.data.frame(pred)
pred=ifelse(pred$Y>0.5,1,0)
mat=table(pred,train$Loan.AF8.Status)
recall=mat[2,2]/(mat[2,2]+mat[2,1])
precision=mat[2,2]/(mat[2,2]+mat[1,2])
f1_score=(2*precision*recall)/(precision+recall)
precision
recall
f1_score

#using my confm function and tuning
#function for returning confusion matrix and other parameters for decison tree
library(rpart)
cm_dt=function(data,index){
  train=data[index,]
  test=data[-index,]
  model=rpart(Loan.AF8.Status~.,data=train,control=c(maxdepth=1))
  pred=predict(model,test)
  pred=as.data.frame(pred)
  pred=ifelse(pred$Y>0.5,1,0)
  mat=table(pred,test$Loan.AF8.Status)
  accuracy=(mat[1,1]+mat[2,2])/(mat[1,2]+mat[2,1]+mat[1,1]+mat[2,2])
  recall=mat[2,2]/(mat[2,2]+mat[2,1])
  precision=mat[2,2]/(mat[2,2]+mat[1,2])
  f1_score=(2*precision*recall)/(precision+recall)
  test_cm=c(accuracy,precision,recall,f1_score)
  pred=predict(model,train)
  pred=as.data.frame(pred)
  pred=ifelse(pred$Y>0.5,1,0)
  mat=table(pred,train$Loan.AF8.Status)
  accuracy=(mat[1,1]+mat[2,2])/(mat[1,2]+mat[2,1]+mat[1,1]+mat[2,2])
  recall=mat[2,2]/(mat[2,2]+mat[2,1])
  precision=mat[2,2]/(mat[2,2]+mat[1,2])
  f1_score=(2*precision*recall)/(precision+recall)
  train_cm=c(accuracy,precision,recall,f1_score)
  nam=c('accuracy','precision','recall','f1_score')
  df=data.frame(nam,train_cm,test_cm)
  rpart.plot(model)
  print(model)
  return(df)
}
cm_dt(loan,index1)
plotcp(model)
?rpart
model
library(rpart.plot)
rpart.plot(model)

#function for returning confusion matrix and other parameters for bagging
library(adabag)
cm_bag=function(data,index){
  train=data[index,]
  test=data[-index,]
  model=bagging(Loan.AF8.Status~.,data=train)
  pred=predict(model,test)
  mat=table(pred$class,test$Loan.AF8.Status)
  accuracy=(mat[1,1]+mat[2,2])/(mat[1,2]+mat[2,1]+mat[1,1]+mat[2,2])
  recall=mat[2,2]/(mat[2,2]+mat[2,1])
  precision=mat[2,2]/(mat[2,2]+mat[1,2])
  f1_score=(2*precision*recall)/(precision+recall)
  test_cm=c(accuracy,precision,recall,f1_score)
  pred=predict(model,train)
  mat=table(pred$class,train$Loan.AF8.Status)
  accuracy=(mat[1,1]+mat[2,2])/(mat[1,2]+mat[2,1]+mat[1,1]+mat[2,2])
  recall=mat[2,2]/(mat[2,2]+mat[2,1])
  precision=mat[2,2]/(mat[2,2]+mat[1,2])
  f1_score=(2*precision*recall)/(precision+recall)
  train_cm=c(accuracy,precision,recall,f1_score)
  nam=c('accuracy','precision','recall','f1_score')
  df=data.frame(nam,train_cm,test_cm)
  return(df)
}
cm_bag(loan,index1)
?bagging

#function for returning confusion matrix and other parameters for random forest
library(randomForest)
cm_rf=function(data,index){
  train=data[index,]
  test=data[-index,]
  model=randomForest(Loan.AF8.Status~.,data=train,ntree=200,mtry=3,strata=loan$Loan.AF8.Status,sampsize=c('N'=150,'Y'=150),nodesize=30,maxnodes=5) #i think strata works like this
  pred=predict(model,test)
  mat=table(pred,test$Loan.AF8.Status)
  accuracy=(mat[1,1]+mat[2,2])/(mat[1,2]+mat[2,1]+mat[1,1]+mat[2,2])
  recall=mat[2,2]/(mat[2,2]+mat[2,1])
  precision=mat[2,2]/(mat[2,2]+mat[1,2])
  f1_score=(2*precision*recall)/(precision+recall)
  test_cm=c(accuracy,precision,recall,f1_score)
  pred=predict(model,train)
  mat=table(pred,train$Loan.AF8.Status)
  accuracy=(mat[1,1]+mat[2,2])/(mat[1,2]+mat[2,1]+mat[1,1]+mat[2,2])
  recall=mat[2,2]/(mat[2,2]+mat[2,1])
  precision=mat[2,2]/(mat[2,2]+mat[1,2])
  f1_score=(2*precision*recall)/(precision+recall)
  train_cm=c(accuracy,precision,recall,f1_score)
  nam=c('accuracy','precision','recall','f1_score')
  df=data.frame(nam,train_cm,test_cm)
  return(df)
}
cm_rf(loan,index1)
?randomForest

#function for returning confusion matrix and other parameters for adaptive boositng
library(adabag)
cm_ab=function(data,index){
  train=data[index,]
  test=data[-index,]
  model=boosting(Loan.AF8.Status~.,data=train,mfinal=100,control=c(maxdepth=1))
  pred=predict(model,test)
  mat=table(pred$class,test$Loan.AF8.Status)
  accuracy=(mat[1,1]+mat[2,2])/(mat[1,2]+mat[2,1]+mat[1,1]+mat[2,2])
  recall=mat[2,2]/(mat[2,2]+mat[2,1])
  precision=mat[2,2]/(mat[2,2]+mat[1,2])
  f1_score=(2*precision*recall)/(precision+recall)
  test_cm=c(accuracy,precision,recall,f1_score)
  pred=predict(model,train)
  mat=table(pred$class,train$Loan.AF8.Status)
  accuracy=(mat[1,1]+mat[2,2])/(mat[1,2]+mat[2,1]+mat[1,1]+mat[2,2])
  recall=mat[2,2]/(mat[2,2]+mat[2,1])
  precision=mat[2,2]/(mat[2,2]+mat[1,2])
  f1_score=(2*precision*recall)/(precision+recall)
  train_cm=c(accuracy,precision,recall,f1_score)
  nam=c('accuracy','precision','recall','f1_score')
  df=data.frame(nam,train_cm,test_cm)
  return(df)
}
cm_ab(loan,index1)
?boosting
?rpart

#function for returning confusion matrix and other parameters for gradient boositng
library(gbm)
loan$Loan.AF8.Status=as.numeric(loan$Loan.AF8.Status)
loan$Loan.AF8.Status=ifelse(loan$Loan.AF8.Status>1,1,0)
str(loan)
#cm_gbm=function(data,index){
  index=sample(nrow(loan),0.7*nrow(loan))
  train=loan[index,]
  test=loan[-index,]
  model=gbm(Loan.AF8.Status~.,data=train,distribution='bernoulli',n.trees=1000,interaction.depth =5,
            n.minobsinnode = 10,
            shrinkage = 0.005,
            bag.fraction = 0.6)
  model
  test$Loan.AF8.Status=as.factor(test$Loan.AF8.Status)
  pred=predict(model,test,n.trees=100)
  pred
  pred=ifelse(pred>0.5,1,0)
  mat=table(pred,test$Loan.AF8.Status)
  accuracy=(mat[1,1]+mat[2,2])/(mat[1,2]+mat[2,1]+mat[1,1]+mat[2,2])
  recall=mat[2,2]/(mat[2,2]+mat[2,1])
  precision=mat[2,2]/(mat[2,2]+mat[1,2])
  f1_score=(2*precision*recall)/(precision+recall)
  test_cm=c(accuracy,precision,recall,f1_score)
  pred=predict(model,train)
  mat=table(pred,train$Loan.AF8.Status)
  accuracy=(mat[1,1]+mat[2,2])/(mat[1,2]+mat[2,1]+mat[1,1]+mat[2,2])
  recall=mat[2,2]/(mat[2,2]+mat[2,1])
  precision=mat[2,2]/(mat[2,2]+mat[1,2])
  f1_score=(2*precision*recall)/(precision+recall)
  train_cm=c(accuracy,precision,recall,f1_score)
  nam=c('accuracy','precision','recall','f1_score')
  df=data.frame(nam,train_cm,test_cm)
  df
  return(df)
#}
cm_gbm(loan,index1)
?gbm
