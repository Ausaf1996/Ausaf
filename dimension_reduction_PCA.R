car=read.csv('c:/Users/LENOVO/My Documents/data sets/carPrice.csv')
View(car)
str(car)
library(dummies)
install.packages('dummies')
library(dummies)
car_new=dummy.data.frame(car,names=c('carCompany',
                                             'fueltype',
                                             'aspiration',
                                             'doornumber',
                                             'carbody',
                                             'drivewheel',
                                             'enginewheel',
                                             'enginelocation',
                                             'enginetype',
                                             'cylindernumber',
                                             'fuelsystem'))
str(car_new)
set.seed(103)
indices=sample(1:nrow(car_new),0.7*nrow(car_new))

pca_train=car_new[indices,]
pca_test=car_new[-indices,]

train_label=pca_train[,76]
test_label=pca_test[,76]

pca_train=pca_train[,-c(1,76)]
pca_test=pca_test[,-c(1,76)]

cor(pca_train)

p_comp=prcomp(pca_train,scale.=T)
plot(p_comp)
summary(p_comp)

std_dev=p_comp$sdev

pr_var=std_dev^2

prop_varex=pr_var/sum(pr_var)

plot(cumsum(prop_varex),xlab='principal component',ylab='cumalative propotion',type='p')

prop_varex
cumsum(prop_varex)
p_comp$x
train_data_pca=data.frame(p_comp$x)
train_data_pca=train_data_pca[,1:40]

train_data_pca=cbind(train_data_pca, price=train_label)
View(train_data_pca)

model=lm(price~.,data=train_data_pca)
summary(model)

test_data_pca=data.frame(predict(p_comp,pca_test))
test_data_pca=test_data_pca[,1:40]

pred=predict(model,test_data_pca)
View(cbind(test_label,pred))
