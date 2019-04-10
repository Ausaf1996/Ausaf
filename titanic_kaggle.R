titanic=read.csv('c:/Users/LENOVO/My Documents/data sets/titanic/titanic_train.csv')
titanic

#extracting new information from name
#adding marriage status
#creating id colum
nrow(titanic)
l=1:891
titanic$id=l


#taking all married as 0
k=rep(0,891)
titanic$Married_women=k


#replacing married with 1
x=grep('Miss',titanic[,'Name'])
titanic$Married_women[x]=1


#identifying doctorates
titanic$doctorate=k
y=grep('Dr',titanic[,'Name'])
titanic$doctorate[y]=1


#identifying cabin
A=grep('A',titanic$Cabin)
B=grep('B',titanic$Cabin)
C=grep('C',titanic$Cabin)
D=grep('D',titanic$Cabin)
E=grep('E',titanic$Cabin)
FF=grep('F',titanic$Cabin)
G=grep('G',titanic$Cabin)

titanic$Cabin=as.character(titanic$Cabin)
titanic$Cabin=rep('general',891)

titanic$Cabin[A]='A'
titanic$Cabin[B]='B'
titanic$Cabin[C]='C'
titanic$Cabin[D]='D'
titanic$Cabin[E]='E'
titanic$Cabin[FF]='FF'
titanic$Cabin[G]='G'


#removing unnecessary variables
titanic$id=NULL
titanic$Name=NULL
titanic$Ticket=NULL
titanic$PassengerId=NULL

str(titanic)
summary(titanic)
class(titanic$Pclass)

#converting to factors
titanic$Pclass=as.factor(titanic$Pclass)
titanic$Survived=as.factor(titanic$Survived)
titanic$Cabin=as.factor(titanic$Cabin)
titanic$Married_women=as.factor(titanic$Married_women)
titanic$doctorate=as.factor(titanic$doctorate)

#finding missing values using knn
library(DMwR)
titanic=knnImputation(data=titanic,k=5)
sum(is.na(titanic))

str(titanic)


#checking for normality of varibales
hist(titanic$Age)
hist(titanic$SibSp)
hist(titanic$Parch)
hist(titanic$Fare)

#plots
library(ggplot2)
ggplot(titanic,aes(1,titanic$Fare))+geom_boxplot()

#removing outlier from fare
quantile(titanic$Fare,0.997)
titanic[titanic$Fare>quantile(titanic$Fare,0.997),'Fare']=quantile(titanic$Fare,0.997)
hist(titanic$Fare)
titanic$Fare=sqrt(titanic$Fare)
hist(titanic$Fare)


library(caret)
titanic=sparse.model.matrix(~.,data=titanic)
titanic=as.matrix(titanic)
titanic=as.data.frame(titanic)
titanic$`(Intercept)`=NULL
length(titanic)

titanic$EmbarkedC=NULL
colnames(titanic)
colnames(titanic1)

#*********************************************************************************
titanic1=read.csv('c:/Users/LENOVO/My Documents/data sets/titanic/titanic_test.csv')

#extracting new information from name
#adding marriage status
#creating id colum
nrow(titanic1)
l=1:418
titanic1$id=l


#taking all married as 0
k=rep(0,418)
titanic1$Married_women=k


#replacing married with 1
x=grep('Miss',titanic1[,'Name'])
titanic1$Married_women[x]=1


#identifying doctorates
titanic1$doctorate=k
y=grep('Dr',titanic1[,'Name'])
titanic1$doctorate[y]=1


#identifying cabin
A=grep('A',titanic1$Cabin)
B=grep('B',titanic1$Cabin)
C=grep('C',titanic1$Cabin)
D=grep('D',titanic1$Cabin)
E=grep('E',titanic1$Cabin)
FF=grep('F',titanic1$Cabin)
G=grep('G',titanic1$Cabin)

titanic1$Cabin=as.character(titanic1$Cabin)
titanic1$Cabin=rep('general',418)

titanic1$Cabin[A]='A'
titanic1$Cabin[B]='B'
titanic1$Cabin[C]='C'
titanic1$Cabin[D]='D'
titanic1$Cabin[E]='E'
titanic1$Cabin[FF]='FF'
titanic1$Cabin[G]='G'


#removing unnecessary variables
titanic1$id=NULL
titanic1$Name=NULL
titanic1$Ticket=NULL
titanic1$PassengerId=NULL

str(titanic1)
summary(titanic1)
class(titanic1$Pclass)

#converting to factors
titanic1$Pclass=as.factor(titanic1$Pclass)
titanic1$Cabin=as.factor(titanic1$Cabin)
titanic1$Married_women=as.factor(titanic1$Married_women)
titanic1$doctorate=as.factor(titanic1$doctorate)

#finding missing values using knn
library(DMwR)
titanic1=knnImputation(data=titanic1,k=5)
sum(is.na(titanic1))

str(titanic1)

library(caret)
titanic1=sparse.model.matrix(~.,data=titanic1)
titanic1=as.matrix(titanic1)
titanic1=as.data.frame(titanic1)
titanic1$`(Intercept)`=NULL
length(titanic1)

#*********************************************************************************


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
    index=sample(nrow(data),round(0.9*nrow(data)))
    train=data[index,]
    test=data[-index,]
    #creating seperate dependent and independent variables
    x=match(target,names(data))
    x
    train_x=train[,-x]
    train_y=train[,x]
    test_x=test[,-x]
    test_y=test[,x]
    #creating xgb matrix
    dtrain=xgb.DMatrix(data=as.matrix(train_x),label=train_y)
    dtest=xgb.DMatrix(data=as.matrix(test_x),label=test_y)
    #creating parameters for xgboost
    params=list(booster='gbtree',objective='binary:logistic',
                eta=0.01,gamma=0,maxdepth=7,min_child_weight=15,
                subsample=0.8,colsample_bytree=0.5,lamda=1)
    #creating the watchlist
    watchlist=list(train=dtrain,test=dtest)
    #buidling the model
    model<<-xgb.train(params=params,data=dtrain,nrounds=1500,watchlist=watchlist,
                    print_every_n=1,early_stopping_rounds=30,maximize=F,
                    eval_metric='logloss')
    it[i]=model$best_iteration
    graph=model$evaluation_log
    #making graphs using ggplot
    p=ggplot(graph)+geom_point(aes(graph$iter,graph$test_logloss,color="test"))+
      geom_point(aes(graph$iter,graph$train_logloss,color='train'))
    print(p)
    #predicitng test data
    pred=predict(model,dtest)
    pred=ifelse(pred>0.5,1,0)
    preds=prediction(pred,test[,target])
    AUC_test[i]=performance(preds,measure='auc')@y.values[[1]]
    mat=table(pred,test[,target])
    accuracy[i]=(mat[1,1]+mat[2,2])/(mat[1,2]+mat[2,1]+mat[1,1]+mat[2,2])
    recall[i]=mat[2,2]/(mat[2,2]+mat[2,1])
    precision[i]=mat[2,2]/(mat[2,2]+mat[1,2])
    f1_score[i]=(2*precision[i]*recall[i])/(precision[i]+recall[i])
    #predicitng train data
    pred=predict(model,dtrain)
    pred=ifelse(pred>0.5,1,0)
    preds=prediction(pred,train[,target])
    AUC_train[i]=performance(preds,measure='auc')@y.values[[1]]
    mat=table(pred,train[,target])
    accuracy1[i]=(mat[1,1]+mat[2,2])/(mat[1,2]+mat[2,1]+mat[1,1]+mat[2,2])
    recall1[i]=mat[2,2]/(mat[2,2]+mat[2,1])
    precision1[i]=mat[2,2]/(mat[2,2]+mat[1,2])
    f1_score1[i]=(2*precision1[i]*recall1[i])/(precision1[i]+recall1[i])
  }
  train_cm=c(mean(AUC_train),mean(accuracy1),mean(precision1),mean(recall1),mean(f1_score1))
  test_cm=c(mean(AUC_test),mean(accuracy),mean(precision),mean(recall),mean(f1_score))
  nam=c('AUC','accuracy','precision','recall','f1_score')
  k=sprintf('the average best iteration is %f',mean(it))
  df=data.frame(nam,train_cm,test_cm)
  n=list(k,df)
  return(n)
}
cm_xgb(titanic,'Survived1')
?xgboost

#*********************************************************************************

#creating xgb matrix
dtest=xgb.DMatrix(data=as.matrix(titanic1))

#predicting values of test data 
pred=predict(model,dtest)
pred=ifelse(pred>0.5,1,0)
pred
#creating the dataframe 
Id=892:1309
sub=cbind(Id,pred)
sub=as.data.frame(sub)
colnames(sub)=c('PassengerId','Survived')

#saving table
write.table(sub,file='titanic_sub',sep=',',row.names = FALSE)
