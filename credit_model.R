credit=read.csv('credit.csv')
View(credit)
head(credit)

#check for class balance
prop.table(table(credit$default)) # balanced

#check missing values
sum(is.na(credit)) # no missing values

#variable selection
table(credit$checking_balance)

barplot(table(credit$default,credit$checking_balance))
ggplot(credit,aes(credit$checking_balance,fill=credit$default))+geom_bar()

ggplot(credit,aes(credit$default,credit$months_loan_duration))+geom_boxplot()

#continue at home


#default 0 and 1
table(credit$default)
credit$default=ifelse(credit$default=='yes',1,0)
credit$default=as.factor(credit$default)
table(credit$default)


#train and test

set.seed(7987)

ids=sample(nrow(credit),0.8*nrow(credit))
train=credit[ids,]
test=credit[-ids,]

default_model=glm(default~checking_balance+months_loan_duration+credit_history+
                    amount+employment_duration+age+other_credit+existing_loans_count,
                  data=train,family='binomial')
summary(default_model)

default_model2=glm(default~checking_balance+months_loan_duration+credit_history+
                    amount+age+existing_loans_count+amount,
                  data=train,family='binomial')
summary(default_model2)

default_model3=glm(default~checking_balance+months_loan_duration+credit_history+
                     amount+age,
                   data=train,family='binomial')
summary(default_model3)

test$prob1=predict(default_model,test,type='response')
test$prob2=predict(default_model2,test,type='response')
test$prob3=predict(default_model3,test,type='response')

#ROCR
install.packages('ROCR')
library('ROCR')
pred=prediction(test$prob1,test$default)
perf=performance(pred,'tpr','fpr')
plot(perf)

pred2=prediction(test$prob2,test$default)
perf2=performance(pred,'tpr','fpr')
plot(perf2,add=T,colorize=T)

pred3=prediction(test$prob3,test$default)
perf3=performance(pred,'tpr','fpr')
plot(perf3,add=T,colorize=T)

#calculating AUC
AUC_1=performance(pred,measure='auc')@y.values[[1]]
AUC_1
AUC_2=performance(pred2,measure="auc")@y.values[[1]]
AUC_2
AUC_3=performance(pred3,measure="auc")@y.values[[1]]
AUC_3

test$pred2=ifelse(test$prob2>0.5,1,0)
table(test$default,test$pred2)

accuracy=(19+128)/200
accuracy
error=1-accuracy
error
precision=19/(19+15)
recall=19/(13+38)

precision
recall

F1_score=2*precision*recall/(precision+recall)
F1_score

#changing cutoff to improve recall

test$pred2=ifelse(test$prob2>0.3,1,0)
table(test$default,test$pred2)

recall=42/(42+15)
recall

#automatic model selection using forward approach
model=glm(default~.,data=train,family='binomial')
fwdmodel=step(model,direction='forward')
backmodel=step(model,direction='backward')

#perfromance of this model
test$probback=predict(backmodel,test,type='response')
test$predback=ifelse(test$probback>0.3,1,0)

table(test$default,test$predback)

recall=39/(39+18)
#our previous model was better