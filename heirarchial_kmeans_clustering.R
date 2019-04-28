install.packages('ISLR')
data(iris)
View(iris_n)
sum(is.na(iris))

#Heirarchial clustering

iris_n=iris
iris_n$species=NULL
View(iris_n)

#removing missing values rows
mydata=na.omit(iris_n)

#scaling data
z_score=function(xy){
  (xy-mean(xy))/(sd(xy))
}
for(i in 1:length(mydata)){
  mydata[[i]]=z_score(mydata[[i]])
}

View(mydata)


#number of cluster using ward heirarchial clustering (dendograms)
d=dist(mydata,method='euclidean')#distance matrix
head(d)

fit=hclust(d,method='ward.D2') # ward is method of finding k value that is dendogram
plot(fit)

s=cutree(fit,k=3)
mydata_hc=cbind(mydata,s)
View(mydata_hc)

rect.hclust(fit,k=3,border='red')
rect.hclust(fit,k=5,border='blue')

#number of clusters using k-means
wss=(nrow(mydata)-1)*sum(apply(mydata,2,var))
wss
apply(mydata,2,var)

clust3=kmeans(mydata,1)
names(clust3)
clust3$withinss

for(i in 2:15)wss[i]=sum(kmeans(mydata,centers=i)$withinss)
plot(1:15,wss,type='b',xlab='clusters',ylab='within sum of squares')

fit=kmeans(mydata,3)
fit$cluster
fit
cluster=fit$cluster
mydata_kc=cbind(mydata,cluster)
View(mydata_kc)
table(mydata_kc$cluster)


aggregate(iris_n,by=list(cluster),FUN=mean)# to interpret results without scaling

table(mydata_kc[,5])

cluster1=mydata_kc[mydata_kc$cluster==1,]
cluster1
cluster2=mydata_kc[cluster==2,]

dim(cluster1)

summary(cluster1)
summary(cluster2)
table(mydata_kc$cluster)

#data exploration

library('ggplot2')

mydata_kc$cluster=as.integer(mydata_kc$cluster)
ggplot(mydata_kc,aes(mydata_kc$Sepal.Length,mydata_kc$Sepal.Width,color=cluster))+geom_point()
ggplot( mydata_kc , aes(Sepal.Length, Petal.Length, color=cluster )) + geom_point()
